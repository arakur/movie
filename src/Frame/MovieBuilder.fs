namespace Frame

open Measure
open FSharpx.Collections

type SpeakerState =
    { Name: string
      SpeechStyle: string
      FontColor: Typst.Color
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
          Voicevox.Speaker = this.Name }

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

    member this.WithText(text: string, color: Typst.Color) =
        { Text = text
          FontColor = color
          FontSize = this.FontSize
          FontWeight = this.FontWeight
          FontFamily = this.FontFamily
          X = this.X
          Y = this.Y
          Width = this.Width
          Height = this.Height }

type MovieState =
    { Frames: Frame Deque
      Speakers: Map<string, SpeakerState>
      CurrentSpeaker: string option
      Subtitle: SubtitleState }

type MovieBuilder() =

    member __.Yield _ = ()

    [<CustomOperation "initialize">]
    member __.Initialize(_: unit, subtitle: SubtitleState) =
        { Frames = Deque.empty
          Speakers = Map.empty
          CurrentSpeaker = None
          Subtitle = subtitle }

    [<CustomOperation "addSpeaker">]
    member __.AddSpeaker(s: MovieState, name: string, speaker: SpeakerState) =
        { s with
            Speakers = s.Speakers.Add(name, speaker) }

    [<CustomOperation "speaker">]
    member __.SetSpeaker(s: MovieState, name: string) = { s with CurrentSpeaker = Some name }

    [<CustomOperation "modify">]
    member __.ModifySpeaker(s: MovieState, name: string, f: SpeakerState -> SpeakerState) =
        { s with
            Speakers = s.Speakers.Add(name, f s.Speakers.[name]) }

    [<CustomOperation "modify">]
    member this.ModifySpeaker(s: MovieState, f: SpeakerState -> SpeakerState) =
        let name =
            s.CurrentSpeaker
            |> Option.defaultWith (fun () -> failwith "Speaker is not set.")

        this.ModifySpeaker(s, name, f)

    [<CustomOperation "talk">]
    member __.YieldFrame(s: MovieState, name: string, subtitleText: string, ?speechText: string) =
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

    let compose (env: Env.Env) (background: string) (state: MovieState) =
        let frameOutputs = FrameOutput.framesToOutput env (state.Frames |> Seq.toList)

        FrameOutput.exportVideo env.FFmpeg background frameOutputs "output/output.mp4"
