namespace Frame

open Measure
open Types

type SubtitleFont =
    { Color: Color
      Size: float<pt>
      Weight: Typst.Weight
      Family: string }

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
      FrameAppearances: FrameAppearance list }
