namespace Frame

open Measure
open Types

type ImageAsset =
    { Path: string
      Pos: Pos
      Resize: Resize option
      StartFrame: int
      EndFrame: int option }

type VideoAsset =
    { Path: string
      Pos: Pos
      Resize: Resize option
      Trim: float<sec> option * float<sec> option
      StartFrame: int
      EndFrame: int option }

type AudioAsset =
    { Path: string
      Trim: float<sec> option * float<sec> option
      StartFrame: int
      EndFrame: int option }

[<RequireQualifiedAccess>]
type Asset =
    | Image of ImageAsset
    | Video of VideoAsset
    | Audio of AudioAsset

    member this.Path =
        match this with
        | Image asset -> asset.Path
        | Video asset -> asset.Path
        | Audio asset -> asset.Path

    member this.StartFrame =
        match this with
        | Image asset -> asset.StartFrame
        | Video asset -> asset.StartFrame
        | Audio asset -> asset.StartFrame

    member this.EndFrame =
        match this with
        | Image asset -> asset.EndFrame
        | Video asset -> asset.EndFrame
        | Audio asset -> asset.EndFrame

    member this.Trim =
        match this with
        | Image _ -> None, None
        | Video asset -> asset.Trim
        | Audio asset -> asset.Trim

    static member withEndFrame (endFrame: int) (this: Asset) =
        match this with
        | Image asset -> Image { asset with EndFrame = Some endFrame }
        | Video asset -> Video { asset with EndFrame = Some endFrame }
        | Audio asset -> Audio { asset with EndFrame = Some endFrame }

type Assets = Map<string, Asset>
