namespace Frame

open Measure
open Types

open FSharpx.Collections

type Name = string

type SpeakerFont(?color: Color, ?size: float<pt>, ?weight: Typst.Weight, ?family: string) =
    member val Color = color with get
    member val Size = size with get
    member val Weight = weight with get
    member val Family = family with get

    member this.WithColor(color: Color) =
        SpeakerFont(color = color, ?size = this.Size, ?weight = this.Weight, ?family = this.Family)

    member this.WithColorRGB(r: int, g: int, b: int) = this.WithColor(RGB(r, g, b))

    member this.WithColorRGBA(r: int, g: int, b: int, a: int) = this.WithColor(RGBA(r, g, b, a))

    member this.WithSize(size: float<pt>) =
        SpeakerFont(?color = this.Color, size = size, ?weight = this.Weight, ?family = this.Family)

    member this.WithWeight(weight: Typst.Weight) =
        SpeakerFont(?color = this.Color, ?size = this.Size, weight = weight, ?family = this.Family)

    member this.WithFamily(family: string) =
        SpeakerFont(?color = this.Color, ?size = this.Size, ?weight = this.Weight, family = family)

    member this.DefaultWith(subtitleFont: SubtitleFont) =
        { Color = this.Color |> Option.defaultValue subtitleFont.Color
          Size = this.Size |> Option.defaultValue subtitleFont.Size
          Weight = this.Weight |> Option.defaultValue subtitleFont.Weight
          Family = this.Family |> Option.defaultValue subtitleFont.Family }

type SpeakerState =
    { Name: Name
      Style: string
      Font: SpeakerFont
      Appearance: FrameAppearance }

    member this.WithTalk(talk: string) : Voicevox.Speech =
        { Voicevox.Talk = talk
          Voicevox.Style = this.Style
          Voicevox.Speaker = this.Name }

module SpeakerState =
    let hFlip (s: SpeakerState) =
        { s with
            Appearance =
                { s.Appearance with
                    Appearance = Appearance.Appearance.hFlip s.Appearance.Appearance } }

    let turnOn (path: string list) (s: SpeakerState) =
        { s with
            Appearance =
                { s.Appearance with
                    Appearance =
                        s.Appearance.Appearance
                        |> Appearance.Appearance.tryTurnOn path
                        |> function
                            | Ok a -> a
                            | Error msg -> failwith (msg.ToString()) } }

    let turnOff (path: string list) (s: SpeakerState) =
        { s with
            Appearance =
                { s.Appearance with
                    Appearance =
                        s.Appearance.Appearance
                        |> Appearance.Appearance.tryTurnOff path
                        |> function
                            | Ok a -> a
                            | Error msg -> failwith (msg.ToString()) } }

type SubtitleState =
    { Font: SubtitleFont
      Pos: Pos
      Size: Size }

    member this.WithText(text: string, speakerFont: SpeakerFont) =
        { Text = text
          Font = speakerFont.DefaultWith this.Font
          Pos = this.Pos
          Size = this.Size }

[<RequireQualifiedAccess>]
type Background =
    | File of string
    | Color of Color

    member this.ToFFmpegInput =
        match this with
        | File path -> sprintf "\"%s\"" path
        | Color _color ->
            // TODO: Implement this.
            failwith "TODO: color background is not implemented yet."

type MovieState =
    { Frames: Frame Deque
      Speakers: Map<Name, SpeakerState>
      CurrentSpeaker: Name option
      Subtitle: SubtitleState
      Background: Background }

type MovieBuilder() =

    member __.Yield _ = ()

    [<CustomOperation "initialize">]
    member __.Initialize(_: unit, subtitle: SubtitleState, background: Background) =
        { Frames = Deque.empty
          Speakers = Map.empty
          CurrentSpeaker = None
          Subtitle = subtitle
          Background = background }

    [<CustomOperation "addSpeaker">]
    member __.AddSpeaker(s: MovieState, name: Name, speaker: SpeakerState) =
        { s with
            Speakers = s.Speakers.Add(name, speaker) }

    [<CustomOperation "speaker">]
    member __.SetSpeaker(s: MovieState, name: Name) = { s with CurrentSpeaker = Some name }

    [<CustomOperation "modify">]
    member __.ModifySpeaker(s: MovieState, name: Name, f: SpeakerState -> SpeakerState) =
        { s with
            Speakers = s.Speakers.Add(name, f s.Speakers.[name]) }

    [<CustomOperation "modify">]
    member this.ModifySpeaker(s: MovieState, f: SpeakerState -> SpeakerState) =
        let name =
            s.CurrentSpeaker
            |> Option.defaultWith (fun () -> failwith "Speaker is not set.")

        this.ModifySpeaker(s, name, f)

    [<CustomOperation "talk">]
    member __.YieldFrame(s: MovieState, name: Name, subtitleText: string, ?speechText: string) =
        let speaker = s.Speakers.[name]

        let speech = speaker.WithTalk(speechText |> Option.defaultValue subtitleText)

        let frameAppearances =
            s.Speakers |> Seq.map (fun kv -> kv.Value.Appearance) |> Seq.toList

        let subtitle = s.Subtitle.WithText(subtitleText, speaker.Font)

        let frame =
            { Speech = speech
              Subtitle = subtitle
              FrameAppearances = frameAppearances }

        { s with Frames = s.Frames.Conj frame }

    [<CustomOperation "talk">]
    member this.YieldFrame(s: MovieState, subtitleText: string, ?speechText: string) =
        let name =
            s.CurrentSpeaker
            |> Option.defaultWith (fun () -> failwith "Speaker is not set.")

        this.YieldFrame(s, name, subtitleText, ?speechText = speechText)

module MovieBuilder =
    let movie = MovieBuilder()

    let compose (env: Env.Env) (state: MovieState) (output: Path) =
        let frameOutputs = FrameOutput.framesToOutput env (state.Frames |> Seq.toList)

        FrameOutput.exportVideo env.FFmpeg state.Background.ToFFmpegInput frameOutputs output
