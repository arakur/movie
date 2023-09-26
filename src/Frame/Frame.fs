namespace Frame

open Measure

type Subtitle =
    { Text: string
      FontColor: Typst.Color
      FontSize: float<pt>
      FontWeight: Typst.Weight
      FontFamily: string
      X: int<pt>
      Y: int<pt>
      Width: int<pt>
      Height: int<pt> }

type FrameAppearance =
    { Appearance: Appearance.Appearance
      X: int
      Y: int }

type Frame =
    { Speech: Voicevox.Speech
      Subtitle: Subtitle
      FrameAppearances: FrameAppearance list }
