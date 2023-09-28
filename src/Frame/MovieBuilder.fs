namespace Frame

open Measure
open Types

open FSharpx.Collections

type Name = string

type SpeakerState =
    { SpeakerName: Name
      SpeechStyle: string
      FontColor: Color
      FontFamily: string option
      Appearance: Appearance.Appearance
      AppearanceX: int<px>
      AppearanceY: int<px> }

    member this.FrameAppearance =
        { Appearance = this.Appearance
          X = this.AppearanceX
          Y = this.AppearanceY }

    member this.WithTalk(talk: string) : Voicevox.Speech =
        { Voicevox.Talk = talk
          Voicevox.Style = this.SpeechStyle
          Voicevox.Speaker = this.SpeakerName }

module SpeakerState =
    let hFlip (s: SpeakerState) =
        { s with
            Appearance = Appearance.Appearance.hFlip s.Appearance }

    let turnOn (path: string list) (s: SpeakerState) =
        { s with
            Appearance =
                s.Appearance
                |> Appearance.Appearance.tryTurnOn path
                |> function
                    | Ok a -> a
                    | Error msg -> failwith (msg.ToString()) }

    let turnOff (path: string list) (s: SpeakerState) =
        { s with
            Appearance =
                s.Appearance
                |> Appearance.Appearance.tryTurnOff path
                |> function
                    | Ok a -> a
                    | Error msg -> failwith (msg.ToString()) }

type SubtitleState =
    { FontSize: float<pt>
      FontWeight: Typst.Weight
      FontFamily: string
      X: int<px>
      Y: int<px>
      Width: int<px>
      Height: int<px> }

    member this.WithText(text: string, color: Color) =
        { Text = text
          FontColor = color
          FontSize = this.FontSize
          FontWeight = this.FontWeight
          FontFamily = this.FontFamily
          X = this.X
          Y = this.Y
          Width = this.Width
          Height = this.Height }

[<RequireQualifiedAccess>]
type Background =
    | File of string
    | Color of Color

    member this.ToFFmpegInput =
        match this with
        | File path -> sprintf "\"%s\"" path
        | Color color ->
            // TODO: ちゃんと書く
            match color with
            | RGB(r, g, b) -> sprintf "color=%d:%d:%d" r g b
            | RGBA(r, g, b, a) -> sprintf "color=%d:%d:%d:%d" r g b a

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
            s.Speakers |> Seq.map (fun kv -> kv.Value.FrameAppearance) |> Seq.toList

        let subtitle = s.Subtitle.WithText(subtitleText, speaker.FontColor)

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
