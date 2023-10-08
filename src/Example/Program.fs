open Env
open Frame.MovieBuilder
open Script
open Parser

open FSharpPlus

let script = "script.txt"

let output = "output/output.mp4"

//

let evalEnv = EvalEnv.prelude().WithInnerOperatorSynonym("立ち絵", "appearance")

//

let movieState =
    script
    |> System.IO.File.ReadAllText
    |> AST.parse
    |> Result.defaultWith failwith
    |> Interpreter.build movie evalEnv
    |> Result.defaultWith failwith

do
    printfn "Ready for rendering..."

    use env = new Env("tmp")

    printfn "Rendering..."

    let p = compose env movieState output

    let log = p.StandardError.ReadToEnd()

    printfn "%s" log

    printfn "Done."
