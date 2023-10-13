namespace Frame

open Types

type ImageAsset =
    { Path: string
      Pos: Pos
      Resize: Resize option
      StartFrame: int
      EndFrame: int option }

type Assets = { Images: Map<string, ImageAsset> }
