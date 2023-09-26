namespace Frame

type Subtitle =
    { Text: string
      FontColor: string
      FontSize: float
      FontWeight: string
      FontFamily: string
      X: int
      Y: int
      Width: int
      Height: int }

type FrameAppearance =
    { Appearance: Appearance.Appearance
      X: int
      Y: int }

type Frame =
    { Speech: Voicevox.Speech
      Subtitle: Subtitle
      FrameAppearances: FrameAppearance list }
