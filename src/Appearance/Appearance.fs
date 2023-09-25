namespace Appearance

open FSharp.Json
open FSharpx.Collections
open FSharpPlus
open ImageMagick
open FSharpPlus.Data

type Appearance =
    { Data: AppearanceData
      Path: string
      X: int
      Y: int
      FlipX: bool
      FlipY: bool }

    member this.Flip =
        match this.FlipX, this.FlipY with
        | false, false -> Flip.None
        | true, false -> Flip.X
        | false, true -> Flip.Y
        | true, true -> Flip.XY

    static member LoadDirectory(path: string) =
        let layerFilePath = path + "/appearance.json"
        let layerFile = System.IO.File.ReadAllText(layerFilePath)

        let data = Json.deserialize layerFile |> AppearanceData.from

        { Data = data
          Path = path
          X = 0
          Y = 0
          FlipX = false
          FlipY = false }

    member this.HFlip() = { this with FlipX = not this.FlipX }

    static member hFlip(appearance: Appearance) = appearance.HFlip()

    member this.VFlip() = { this with FlipY = not this.FlipY }

    static member vFlip(appearance: Appearance) = appearance.VFlip()

    member this.TryTurnOn(path: string list) : AppearanceResult<Appearance> =
        Children.tryTurnOn path this.Data.Children
        |> Result.map (fun children ->
            { this with
                Data = { this.Data with Children = children } })

    static member tryTurnOn (path: string list) (appearance: Appearance) = appearance.TryTurnOn path

    member this.TryTurnOff(path: string list) : AppearanceResult<Appearance> =
        Children.tryTurnOff path this.Data.Children
        |> Result.map (fun children ->
            { this with
                Data = { this.Data with Children = children } })

    static member tryTurnOff (path: string list) (appearance: Appearance) = appearance.TryTurnOff path

    member this.Write(magick: ImageMagick, outputTo: string) =
        let rec collectLayerForFlip (layerForFlip: LayerForFlip) =
            seq {
                let layer = layerForFlip.get this.Flip

                if layer.Kind = Pixel then
                    let path = sprintf "%s/%s" this.Path layer.Path

                    yield
                        layer.Level,
                        {| Path = path
                           Left = layer.Left
                           Top = layer.Top |}

                yield! collectChildren layer.Children
            }

        and collectChildren (children: Children) =
            seq {
                for layerView in children.Layers.Values do
                    if layerView.IsVisible then
                        yield! collectLayerForFlip layerView.LayerForFlip

                yield!
                    children.RadioLayers
                    |> Option.map (fun radioLayers -> collectLayerForFlip radioLayers.Layers.[radioLayers.Selected])
                    |> Option.defaultValue Seq.empty
            }

        let layers = collectChildren this.Data.Children |> Seq.sortBy fst |> Seq.map snd

        let arguments =
            seq {
                yield "convert"

                yield! [ "-size"; sprintf "%dx%d" this.Data.Width this.Data.Height ]

                yield "canvas:none"

                for layer in layers do
                    yield layer.Path
                    yield! [ "-geometry"; sprintf "+%d+%d" layer.Left layer.Top ]
                    yield "-composite"

                // flip

                if this.FlipX then
                    yield "-flop"

                if this.FlipY then
                    yield "-flip"

                yield outputTo
            }

        magick.Start(arguments)

    static member write (magick: ImageMagick) (outputTo: string) (appearance: Appearance) =
        appearance.Write(magick, outputTo)
