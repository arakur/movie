namespace Frame

// comparison
[<StructuralEquality>]
[<StructuralComparison>]
[<RequireQualifiedAccess>]
type LayerId =
    | Appearance of name: string * frameIndex: int
    | Subtitle of frameIndex: int
    | Asset of name: string
