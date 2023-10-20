namespace Frame

open Measure
open Types

open FSharpx.Collections

type Name = string

type SpeakerState =
    { Name: Name
      Style: string
      Font: SubtitleFontUpdate
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

    let turnOn (path: string seq) (s: SpeakerState) =
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

    member this.WithText(text: string, update: SubtitleFontUpdate) =
        let fontUpdated: SubtitleFont =
            { Color = update.Color |> Option.orElse this.Font.Color
              Size = update.Size |> Option.orElse this.Font.Size
              Weight = update.Weight |> Option.orElse this.Font.Weight
              Family = update.Family |> List.append this.Font.Family }

        { Text = text
          Font = fontUpdated
          Pos = this.Pos
          Size = this.Size }

[<RequireQualifiedAccess>]
type Background =
    | Image of fileName: string
    | Video of fileName: string
    | RGB of r: int * g: int * b: int

    member this.ToFFmpegBackgroundInput: FFmpeg.Background =
        match this with
        | Image path -> FFmpeg.Background.Image path
        | Video path -> FFmpeg.Background.Video path
        | RGB(r, g, b) -> FFmpeg.Background.RGB(r, g, b)

type MovieState =
    { Frames: Frame Deque
      Speakers: Map<Name, SpeakerState>
      CurrentSpeaker: Name option
      Subtitle: SubtitleState
      Assets: Assets
      Background: Background }

    static member empty =
        { Frames = Deque.empty
          Speakers = Map.empty
          CurrentSpeaker = None
          Subtitle =
            { Font =
                { Color = None
                  Size = None
                  Weight = None
                  Family = [] }
              Pos = { X = 0<px>; Y = 0<px> }
              Size = { Width = 0<px>; Height = 0<px> } }
          Assets = Map.empty
          Background = Background.RGB(0, 0, 0) }

type Initialize =
    { Font: SubtitleFont
      Pos: Pos
      Size: Size
      Background: Background }

type MovieBuilder() =

    member __.Yield _ = ()

    [<CustomOperation "initialize">]
    member __.Initialize(_: unit, initialize: Initialize) =
        { Frames = Deque.empty
          Speakers = Map.empty
          CurrentSpeaker = None
          Subtitle =
            { Font = initialize.Font
              Pos = initialize.Pos
              Size = initialize.Size }
          Assets = Map.empty
          Background = initialize.Background }

    [<CustomOperation "addSpeaker">]
    member __.AddSpeaker(s: MovieState, name: Name, speaker: SpeakerState) =
        { s with
            Speakers = s.Speakers.Add(name, speaker) }

    [<CustomOperation "speaker">]
    member __.SetSpeaker(s: MovieState, name: Name) = { s with CurrentSpeaker = Some name }

    [<CustomOperation "setStyle">]
    member __.SetStyle(s: MovieState, style: string) =
        let currentSpeaker =
            s.CurrentSpeaker
            |> Option.defaultWith (fun () -> failwith "Speaker is not set.")

        let speaker = s.Speakers.[currentSpeaker]
        let speaker' = { speaker with Style = style }

        { s with
            Speakers = s.Speakers.Add(currentSpeaker, speaker') }

    [<CustomOperation "addImage">]
    member __.AddImage(s: MovieState, id: string, path: string, pos: Pos, resize: Resize option) =
        let asset: ImageAsset =
            { Path = path
              Pos = pos
              Resize = resize }

        let assetWithDuration =
            { Asset = asset
              StartFrame = s.Frames.Length
              EndFrame = None }

        let assets = s.Assets

        let assets' = assets |> Map.add id assetWithDuration

        { s with Assets = assets' }

    [<CustomOperation "addVideo">]
    member __.AddVideo
        (
            s: MovieState,
            id: string,
            path: string,
            pos: Pos,
            resize: Resize option,
            trimStart: float<sec> option,
            trimEnd: float<sec> option
        ) =
        let asset: VideoAsset =
            { Path = path
              Pos = pos
              Resize = resize
              Trim = (trimStart, trimEnd) }

        let assetWithDuration =
            { Asset = asset
              StartFrame = s.Frames.Length
              EndFrame = None }

        let assets = s.Assets

        let assets' = assets |> Map.add id assetWithDuration

        { s with Assets = assets' }

    [<CustomOperation "addAudio">]
    member __.AddAudio
        (
            s: MovieState,
            id: string,
            path: string,
            trimStart: float<sec> option,
            trimEnd: float<sec> option
        ) =
        let asset: AudioAsset =
            { Path = path
              Trim = trimStart, trimEnd }

        let assetWithDuration =
            { Asset = asset
              StartFrame = s.Frames.Length
              EndFrame = None }

        let assets = s.Assets

        let assets' = assets |> Map.add id assetWithDuration

        { s with Assets = assets' }

    [<CustomOperation "remove">]
    member __.Remove(s: MovieState, id: string) =
        let assets = s.Assets

        let asset = assets.[id]

        let assetEnded = asset |> AssetWithDuration.withEndFrame s.Frames.Length

        let assets' = assets |> Map.add id assetEnded

        { s with Assets = assets' }

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
        Export.exportVideo env state.Background.ToFFmpegBackgroundInput state.Frames state.Assets output
