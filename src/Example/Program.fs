open Env
open Frame.MovieBuilder
open Script
open Parser

open FSharpPlus

let script = "script.txt"

do
    printfn "Ready for rendering..."

    use env = new Env("tmp")

    let mutable loop = true

    printfn "`render` to render, `exit` to exit."

    while loop do
        printf "command: "
        let input = System.Console.ReadLine()

        match input with
        | "exit" -> loop <- false
        | "render" ->
            printfn "Rendering..."

            let output = "output/output.mp4"

            let evalEnv = EvalEnv.prelude().WithInnerOperatorSynonym("立ち絵", "appearance")

            let movieState =
                script
                |> System.IO.File.ReadAllText
                |> AST.parse
                >>= Interpreter.build movie evalEnv

            match movieState with
            | Error msg -> printfn "Error: %s" msg
            | Ok movieState ->
                let p = compose env movieState output

                let log = p.StandardError.ReadToEnd()

                // printfn "%s" log

                printfn "Done."

        | _ -> printfn "Unknown command."
