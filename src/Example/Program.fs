open Env
open Frame.MovieBuilder
open Script

let script = System.IO.File.ReadAllText "script.txt"

let lines = Lexer.nodesFrom script

let ast =
    Parser.AST.from lines
    |> function
        | Ok ast -> ast
        | Error e -> failwith e

let evalEnv = EvalEnv.prelude().WithInnerOperatorSynonym("立ち絵", "appearance")

let movieState =
    Interpreter.run movie ast (evalEnv, Frame.MovieState.empty)
    |> function
        | Ok(_, state) -> state
        | Error e -> failwith e

//

let output = "output/output.mp4"

do
    printfn "Ready for rendering..."

    use env = new Env("tmp")

    printfn "Rendering..."

    let p = compose env movieState output

    let log = p.StandardError.ReadToEnd()

    printfn "%s" log

    printfn "Done."
