module Appearance.LayersRaw

open FSharp.Json
open FSharpx.Collections
open System.Text.RegularExpressions

type Flip =
    | [<JsonUnionCase("flipx")>] FlipX
    | [<JsonUnionCase("flipy")>] FlipY
    | [<JsonUnionCase("flipxy")>] FlipXY

type LayerKind =
    | [<JsonUnionCase("pixel")>] Pixel
    | [<JsonUnionCase("group")>] Group

type Layer =
    { [<JsonField("kind")>]
      Kind: LayerKind
      [<JsonField("name")>]
      RawName: string
      [<JsonField("level")>]
      Level: int
      [<JsonField("radio")>]
      Radio: bool
      [<JsonField("forced")>]
      Forced: bool
      [<JsonField("flip")>]
      Flip: Flip list
      [<JsonField("is_visible")>]
      IsVisible: bool
      [<JsonField("left")>]
      Left: int
      [<JsonField("top")>]
      Top: int
      [<JsonField("children")>]
      Children: Layers
      [<JsonField("path")>]
      Path: string }

    member this.Name = Regex.Replace(this.RawName, "[0-9]+_", "")

and Layers = Layer list

type AppearanceData =
    { [<JsonField("width")>]
      Width: int
      [<JsonField("height")>]
      Height: int
      [<JsonField("layers")>]
      Layers: Layers }
