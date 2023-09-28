open Measure
open Types
open Appearance
open Typst
open Env
open Frame
open Frame.SpeakerState
open Frame.MovieBuilder

//

let movieState =
    movie {
        initialize
            { Font =
                { Color = RGB(255, 255, 255)
                  Size = 50.0<pt>
                  Weight = Bold
                  Family = "Noto Sans CJK JP" }
              Pos = { X = 400<px>; Y = 700<px> }
              Size = { Width = 1000<px>; Height = 300<px> } }
            (Background.File "sample/sample-10s.mp4")

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

        modify "春日部つむぎ" hFlip

        modify "四国めたん" (turnOn [ "白ロリ服"; "左腕"; "普通" ])

        //

        speaker "春日部つむぎ"

        talk "モナドは単なる自己関手の圏におけるモノイド対象だよ．" "もなどはたんなるじこかんしゅのけんにおけるモノイドたいしょうだよ"

        modify (turnOn [ "ほっぺ"; "かげり" ] >> turnOn [ "目"; "基本目セット"; "黒目"; "目逸らし" ])

        talk "何か問題でも？"

        //

        speaker "四国めたん"

        modify (turnOn [ "白ロリ服"; "左腕"; "口元に指" ])

        talk "おっそうだな．"
    }

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
