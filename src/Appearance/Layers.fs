namespace Appearance

open FSharpx.Collections
open LayersRaw
open LayersRaw
open FSharpPlus

[<RequireQualifiedAccess>]
type Flip =
    | None
    | X
    | Y
    | XY

    static member from(raw: LayersRaw.Flip) =
        match raw with
        | FlipX -> X
        | FlipY -> Y
        | FlipXY -> XY

type LayerKind =
    | Pixel
    | Group

    static member from(raw: LayersRaw.LayerKind) =
        match raw with
        | LayersRaw.Pixel -> Pixel
        | LayersRaw.Group -> Group

type Layer =
    { Level: int
      Kind: LayerKind
      Forced: bool
      Left: int
      Top: int
      Children: Children
      Path: string }

and LayerForFlip =
    { None: Layer option
      X: Layer option
      Y: Layer option
      XY: Layer option }

    static member Mk (layer: Layer) (flips: Flip seq) =
        flips
        |> Seq.fold
            (fun lff flip -> LayerForFlip.update flip layer lff)
            { None = None
              X = None
              Y = None
              XY = None }

    member this.get(flip: Flip) =
        match flip with
        | Flip.None ->
            this.None
            |> Option.orElseWith (fun () -> this.XY)
            |> Option.orElseWith (fun () -> this.X)
            |> Option.orElseWith (fun () -> this.Y)
        | Flip.X ->
            this.X
            |> Option.orElseWith (fun () -> this.Y)
            |> Option.orElseWith (fun () -> this.None)
            |> Option.orElseWith (fun () -> this.XY)
        | Flip.Y ->
            this.Y
            |> Option.orElseWith (fun () -> this.X)
            |> Option.orElseWith (fun () -> this.None)
            |> Option.orElseWith (fun () -> this.XY)
        | Flip.XY ->
            this.XY
            |> Option.orElseWith (fun () -> this.None)
            |> Option.orElseWith (fun () -> this.X)
            |> Option.orElseWith (fun () -> this.Y)
        |> Option.defaultWith (fun () ->
            failwith "Internal error: LayerForFlip has no layer although it should have one")

    static member update (flip: Flip) (layer: Layer) (this: LayerForFlip) =
        match flip with
        | Flip.None -> { this with None = Some layer }
        | Flip.X -> { this with X = Some layer }
        | Flip.Y -> { this with Y = Some layer }
        | Flip.XY -> { this with XY = Some layer }

    member this.Modify(f: Children -> Children AppearanceResult) : LayerForFlip AppearanceResult =
        let modifyEachLayer f =
            function
            | None -> Ok None
            | Some(layer: Layer) ->
                f layer.Children
                |> Result.map (fun children -> { layer with Children = children })
                |> Result.map Some

        monad {
            let! layerNone' = this.None |> modifyEachLayer f
            let! layerX' = this.X |> modifyEachLayer f
            let! layerY' = this.Y |> modifyEachLayer f
            let! layerXY' = this.XY |> modifyEachLayer f

            return
                { None = layerNone'
                  X = layerX'
                  Y = layerY'
                  XY = layerXY' }
        }

    static member modify (f: Children -> Children AppearanceResult) (this: LayerForFlip) = this.Modify f

and LayerView =
    { LayerForFlip: LayerForFlip
      IsVisible: bool }

and RadioLayers =
    { Layers: Map<string, LayerForFlip>
      Selected: string }

    member this.TryFind(name: string) = this.Layers.TryFind name

and Children =
    { Layers: Map<string, LayerView>
      RadioLayers: RadioLayers option }

    static member from(raw: LayersRaw.Layers) =
        let add
            (state:
                {| layers: Map<string, LayerView>
                   radioLayers: Map<string, LayerForFlip>
                   radioSelected: string option |})
            (raw: LayersRaw.Layer)
            =
            let layer: Layer =
                { Level = raw.Level
                  Kind = LayerKind.from raw.Kind
                  Forced = raw.Forced
                  Left = raw.Left
                  Top = raw.Top
                  Children = Children.from raw.Children
                  Path = raw.Path }

            let name = raw.RawName

            let flips =
                raw.Flip
                |> List.map Flip.from
                |> function
                    | [] -> [ Flip.None ]
                    | flips -> flips

            if raw.Radio then
                let layerForFlip' =
                    state.radioLayers
                    |> Map.tryFind name
                    |> Option.map (
                        raw.Flip
                        |> Seq.map Flip.from
                        |> Seq.foldBack (fun flip -> LayerForFlip.update flip layer)
                    )
                    |> Option.defaultValue (LayerForFlip.Mk layer flips)

                let radioLayers' = state.radioLayers |> Map.add name layerForFlip'

                let selected = if raw.IsVisible then Some name else state.radioSelected

                {| layers = state.layers
                   radioLayers = radioLayers'
                   radioSelected = selected |}
            else
                let layerView = state.layers |> Map.tryFind name

                let layerView' =
                    layerView
                    |> Option.map (
                        flips
                        |> Seq.foldBack (fun flip (layerView: LayerView) ->
                            { layerView with
                                LayerForFlip = layerView.LayerForFlip |> LayerForFlip.update flip layer })
                    )
                    |> Option.map (fun layerView ->
                        { layerView with
                            IsVisible = layerView.IsVisible || raw.IsVisible })
                    |> Option.defaultValue
                        { LayerForFlip = LayerForFlip.Mk layer flips
                          IsVisible = raw.IsVisible }

                let layers' = state.layers |> Map.add name layerView'

                {| layers = layers'
                   radioLayers = state.radioLayers
                   radioSelected = state.radioSelected |}

        let ret =
            raw
            |> Seq.fold
                add
                {| layers = Map.empty
                   radioLayers = Map.empty
                   radioSelected = None |}

        let radioSelected =
            ret.radioSelected
            // Select any layer if no layer is selected. If the map is empty, None is returned.
            |> Option.orElseWith (fun () -> ret.radioLayers.Keys |> Seq.tryHead)

        let radioLayers =
            radioSelected
            |> Option.map (fun name ->
                { Layers = ret.radioLayers
                  Selected = name })

        { Layers = ret.layers
          RadioLayers = radioLayers }

    member this.ModifyLayer name f =
        monad {
            let! layerView = this.Layers.TryFind name |> Option.toResultWith (LayerNotFound [ name ])

            let! layerView' = f layerView

            return
                { this with
                    Layers = this.Layers |> Map.add name layerView' }
        }

    member this.OrModifyRadioLayer error name f =
        monad {
            let! radioLayers = this.RadioLayers |> Option.toResultWith error

            let! layerView = radioLayers.TryFind name |> Option.toResultWith error

            let! layerView' = f layerView

            return
                { this with
                    RadioLayers =
                        Some
                            { radioLayers with
                                Layers = radioLayers.Layers |> Map.add name layerView' } }
        }

    member this.TryTurnOn(name: string) =
        this.ModifyLayer name (fun layerView ->
            if layerView.IsVisible then
                Error(LayerAlreadyTurnedOn [ name ])
            else
                Ok { layerView with IsVisible = true })
        |> Result.bindError (fun _ ->
            monad {
                let! radioLayers = this.RadioLayers |> Option.toResultWith (LayerNotFound [ name ])

                let! _exists_layer = radioLayers.TryFind name |> Option.toResultWith (LayerNotFound [ name ])

                if radioLayers.Selected = name then
                    return! Error(LayerAlreadyTurnedOn [ name ])
                else
                    return
                        { this with
                            RadioLayers = Some { radioLayers with Selected = name } }
            })

    member this.TryTurnOn(path: string list) =
        match path with
        | [] -> Error EmptyLocation
        | [ name ] -> this.TryTurnOn name
        | name :: rest ->
            let f = Children.tryTurnOn rest >> Result.mapError (AppearanceError.append name)

            this.ModifyLayer name (fun layerView ->
                monad {
                    let! layerForFlip' = layerView.LayerForFlip.Modify f

                    return
                        { layerView with
                            LayerForFlip = layerForFlip' }
                })
            |> Result.bindError (fun error -> this.OrModifyRadioLayer error name (LayerForFlip.modify f))

    static member tryTurnOn (path: string list) (children: Children) = children.TryTurnOn path

    member this.TryTurnOff name =
        this.ModifyLayer name (fun layerView ->
            if layerView.IsVisible then
                let forced =
                    [ layerView.LayerForFlip.None
                      layerView.LayerForFlip.X
                      layerView.LayerForFlip.Y
                      layerView.LayerForFlip.XY ]
                    |> Seq.choose id
                    |> Seq.forall (fun layer -> layer.Forced)

                if forced then
                    Error(LayerIsForced [ name ])
                else
                    Ok { layerView with IsVisible = false }
            else
                Error(LayerAlreadyTurnedOff [ name ]))
        |> Result.bindError (fun error ->
            monad {
                let! radioLayers = this.RadioLayers |> Option.toResultWith error

                let! _exists_layer = radioLayers.TryFind name |> Option.toResultWith error

                return! Error(LayerIsRadioLayer [ name ])
            })


    member this.TryTurnOff(path: string list) =
        match path with
        | [] -> Error EmptyLocation
        | [ name ] -> this.TryTurnOff name
        | name :: rest ->
            let f = Children.tryTurnOff rest >> Result.mapError (AppearanceError.append name)

            this.ModifyLayer name (fun layerView ->
                monad {
                    let! layerForFlip' = layerView.LayerForFlip.Modify f

                    return
                        { layerView with
                            LayerForFlip = layerForFlip' }
                })
            |> Result.bindError (fun error -> this.OrModifyRadioLayer error name (LayerForFlip.modify f))

    static member tryTurnOff (path: string list) (children: Children) = children.TryTurnOff path

type AppearanceData =
    { Width: int
      Height: int
      Children: Children }

    static member from(raw: LayersRaw.AppearanceData) =
        { Width = raw.Width
          Height = raw.Height
          Children = Children.from raw.Layers }
