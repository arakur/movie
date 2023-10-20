namespace Frame

open Measure
open Types

type IAsset =
    abstract Path: Path
    abstract Trim: float<sec> option * float<sec> option

type ILayerAsset =
    inherit IAsset
    abstract Pos: Pos
    abstract Resize: Resize option

type IImageLayerAsset =
    inherit ILayerAsset

type IAudioAsset =
    inherit IAsset

//

type SpeechAsset =
    { Path: Path }

    interface IAsset with
        member this.Path = this.Path
        member __.Trim = None, None

    interface IAudioAsset

type AppearanceAsset =
    { Path: Path
      Pos: Pos
      Resize: Resize option }

    interface IAsset with
        member this.Path = this.Path
        member __.Trim = None, None

    interface ILayerAsset with
        member this.Pos = this.Pos
        member this.Resize = this.Resize

    interface IImageLayerAsset

type SubtitleAsset =
    { Path: Path
      Pos: Pos }

    interface IAsset with
        member this.Path = this.Path
        member __.Trim = None, None

    interface ILayerAsset with
        member this.Pos = this.Pos
        member __.Resize = None

    interface IImageLayerAsset

type ImageAsset =
    { Path: Path
      Pos: Pos
      Resize: Resize option }

    interface IAsset with
        member this.Path = this.Path
        member __.Trim = None, None

    interface ILayerAsset with
        member this.Pos = this.Pos
        member this.Resize = this.Resize

    interface IImageLayerAsset

type VideoAsset =
    { Path: Path
      Pos: Pos
      Resize: Resize option
      Trim: float<sec> option * float<sec> option }

    interface IAsset with
        member this.Path = this.Path
        member this.Trim = this.Trim

    interface ILayerAsset with
        member this.Pos = this.Pos
        member this.Resize = this.Resize

    interface IAudioAsset

type AudioAsset =
    { Path: Path
      Trim: float<sec> option * float<sec> option }

    interface IAsset with
        member this.Path = this.Path
        member this.Trim = this.Trim

    interface IAudioAsset

// TODO: type TextBoxAsset

// [<RequireQualifiedAccess>]
// type Asset =
//     | Speech of SpeechAsset
//     | Appearance of AppearanceAsset
//     | Subtitle of SubtitleAsset
//     | BackgroundVideo of BackgroundVideoAsset
//     | BackgroundImage of BackgroundImageAsset
//     | Image of ImageAsset
//     | Video of VideoAsset
//     | Audio of AudioAsset
//     // TODO: | TextBox of TextBoxAsset

//     member this.Path =
//         match this with
//         | Image asset -> asset.Path
//         | Video asset -> asset.Path
//         | Audio asset -> asset.Path

//     member this.StartFrame =
//         match this with
//         | Image asset -> asset.StartFrame
//         | Video asset -> asset.StartFrame
//         | Audio asset -> asset.StartFrame

//     member this.EndFrame =
//         match this with
//         | Image asset -> asset.EndFrame
//         | Video asset -> asset.EndFrame
//         | Audio asset -> asset.EndFrame

//     member this.Trim =
//         match this with
//         | Image _ -> None, None
//         | Video asset -> asset.Trim
//         | Audio asset -> asset.Trim

//     static member withEndFrame (endFrame: int) (this: Asset) =
//         match this with
//         | Image asset -> Image { asset with EndFrame = Some endFrame }
//         | Video asset -> Video { asset with EndFrame = Some endFrame }
//         | Audio asset -> Audio { asset with EndFrame = Some endFrame }

type AssetWithDuration =
    { Asset: IAsset
      StartFrame: int
      EndFrame: int option }

    static member withEndFrame (endFrame: int) (this: AssetWithDuration) = { this with EndFrame = Some endFrame }

type Assets = Map<string, AssetWithDuration>
