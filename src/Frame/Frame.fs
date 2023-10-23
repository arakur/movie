namespace Frame

open Measure
open Types

type SubtitleFont =
    { Color: Color option
      Size: float<pt> option
      Weight: Typst.Weight option
      Family: string list }

type SubtitleFontUpdate =
    { Color: Color option
      Size: float<pt> option
      Weight: Typst.Weight option
      Family: string list }

type Subtitle =
    { Text: string
      Font: SubtitleFont
      Pos: Pos
      Size: Size }

type FrameAppearance =
    { Appearance: Appearance.Appearance
      Pos: Pos
      Resize: Resize option }

type Frame =
    { Speech: Voicevox.Speech
      Subtitle: Subtitle
      FrameAppearances: FrameAppearance list
      Priority: LayerId list }
