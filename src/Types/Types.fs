namespace Types

type Path = string

type Color =
    | RGB of int * int * int
    | RGBA of int * int * int * int

    member this.Compose() =
        match this with
        | RGB(r, g, b) -> sprintf "rgb(%d, %d, %d)" r g b
        | RGBA(r, g, b, a) -> sprintf "rgba(%d, %d, %d, %d)" r g b a