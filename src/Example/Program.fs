open Measure
open Types
open Appearance
open Typst
open Env
open Frame
open Frame.MovieBuilder
open Script

let script = System.IO.File.ReadAllText "script.txt"

let lines = Lexer.nodesFrom script

let ast =
    Parser.AST.from lines
    |> function
        | Ok ast -> ast
        | Error e -> failwith e

let initialState =
    movie {
        initialize
            { Font =
                { Color = RGB(255, 255, 255)
                  Size = 50.0<pt>
                  Weight = Weight.Bold
                  Family = "Noto Sans CJK JP" }
              Pos = { X = 400<px>; Y = 700<px> }
              Size = { Width = 1000<px>; Height = 300<px> }
              Background = Background.File "sample/sample-10s.mp4" }

        addSpeaker
            "春日部つむぎ"
            { Name = "春日部つむぎ"
              Style = "ノーマル"
              Font = SpeakerFont().WithColorRGB(251, 202, 77)
              Appearance =
                { Appearance = Appearance.LoadDirectory("appearance/春日部つむぎ")
                  Pos = { X = 0<px>; Y = 0<px> } } }

        addSpeaker
            "四国めたん"
            { Name = "四国めたん"
              Style = "ノーマル"
              Font = SpeakerFont().WithColorRGB(231, 96, 158)
              Appearance =
                { Appearance = Appearance.LoadDirectory("appearance/四国めたん")
                  Pos = { X = 800<px>; Y = 0<px> } } }
    }

let evalEnv = EvalEnv.prelude().WithInnerOperatorSynonym("立ち絵", "appearance")

let movieState =
    Interpreter.run movie ast (evalEnv, initialState)
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
