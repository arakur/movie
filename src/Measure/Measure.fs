namespace Measure

[<Measure>]
type pt

[<Measure>]
type px

module pt =
    let inline ofFloat (f: float) = f * 1.0<pt>

    let perPx = 0.75<pt / px>

module px =
    let inline asFloatPx (p: int<px>) = float p * 1.0<px>
