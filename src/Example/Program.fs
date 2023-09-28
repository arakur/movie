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
            { FontSize = 50.0<pt>
              FontWeight = Bold
              FontFamily = "Noto Sans CJK JP"
              X = 400<px>
              Y = 700<px>
              Width = 1000<px>
              Height = 300<px> }
            (Background.File "sample/sample-10s.mp4")

        addSpeaker
            "春日部つむぎ"
            { SpeakerName = "春日部つむぎ"
              SpeechStyle = "ノーマル"
              FontColor = RGB(251, 202, 77)
              FontFamily = None
              Appearance = Appearance.LoadDirectory("appearance/春日部つむぎ")
              AppearanceX = 0<px>
              AppearanceY = 0<px> }

        addSpeaker
            "四国めたん"
            { SpeakerName = "四国めたん"
              SpeechStyle = "ノーマル"
              FontColor = RGB(231, 96, 158)
              FontFamily = None
              Appearance = Appearance.LoadDirectory("appearance/四国めたん")
              AppearanceX = 800<px>
              AppearanceY = 0<px> }

        modify "春日部つむぎ" hFlip

        modify "四国めたん" (turnOn [ "白ロリ服"; "左腕"; "普通" ])

        //

        speaker "春日部つむぎ"

        talk "モナドは単なる自己関手の圏におけるモノイド対象だよ．" "モナドは単なる自己かん手の圏におけるモノイド対象だよ"

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
