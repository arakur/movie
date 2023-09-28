namespace Types

open Measure

type Path = string

type Color =
    | RGB of int * int * int
    | RGBA of int * int * int * int

    member this.Compose() =
        match this with
        | RGB(r, g, b) -> sprintf "rgb(%d, %d, %d)" r g b
        | RGBA(r, g, b, a) -> sprintf "rgba(%d, %d, %d, %d)" r g b a

type Pos = { X: int<px>; Y: int<px> }

type PosFloat =
    { X: float<px>
      Y: float<px> }

    member this.ToInt() : Pos =
        { X = int this.X * 1<px>
          Y = int this.Y * 1<px> }

type Size = { Width: int<px>; Height: int<px> }
