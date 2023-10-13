﻿open Env
open Frame.MovieBuilder
open Script
open Parser

open FSharpPlus

let evalEnv =
    EvalEnv
        .prelude()
        .WithInnerOperatorSynonym("立ち絵", "appearance")
        .WithInnerOperatorSynonym("スタイル", "set-style")
        .WithInnerOperatorSynonym("画像", "add-image")
        .WithInnerOperatorSynonym("音声", "add-audio")
        .WithInnerOperatorSynonym("動画", "add-video")
        .WithInnerOperatorSynonym("消去", "remove")

let render path env =
    printfn "Rendering..."

    let output = "output/output.mp4"

    let script = path |> System.IO.File.ReadAllText

    let ast = script |> AST.parse

    let result =
        try
            ast >>= Interpreter.build movie evalEnv
        with msg ->
            Error $"Error: {msg}"

    match result with
    | Error msg -> printfn "Error: %s" msg
    | Ok movieState ->
        let p = compose env movieState output

        let log = p.StandardError.ReadToEnd()

        printfn "%s" log

        printfn "Done."

do
    printfn "Ready for rendering..."

    use env = new Env("tmp")

    let mutable loop = true

    printfn "`render` to render, `exit` to exit."

    while loop do
        printf "command: "
        let input = System.Console.ReadLine()

        let segments = input |> String.split [ " " ] |> Seq.toList

        match segments with
        | [ "exit" ] -> loop <- false
        | [ "render"; path ] ->
            if System.IO.File.Exists path then
                render path env
            else
                printfn $"File not found: {path}"
        | _ -> printfn "Unknown command."
