namespace Measure

[<Measure>]
type pt

module pt =
    let inline ofFloat (f: float) = f * 1.0<pt>
