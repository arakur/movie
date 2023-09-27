namespace Frame

open Measure

type Subtitle =
    { Text: string
      FontColor: Typst.Color
      FontSize: float<pt>
      FontWeight: Typst.Weight
      FontFamily: string
      X: int<px>
      Y: int<px>
      Width: int<px>
      Height: int<px> }

type FrameAppearance =
    { Appearance: Appearance.Appearance
      X: int<px>
      Y: int<px> }

type Frame =
    { Speech: Voicevox.Speech
      Subtitle: Subtitle
      FrameAppearances: FrameAppearance list }
