open Voicevox

open Appearance
open ImageMagick

open FFmpeg

//

let unwrap<'a, 'b> (res: Result<'a, 'b>) : 'a =
    res |> FSharpPlus.Result.either id (fun msg -> failwith (msg.ToString()))

//

//

let 春日部つむぎ = Appearance.LoadDirectory("appearance/春日部つむぎ")
let 四国めたん = Appearance.LoadDirectory("appearance/四国めたん")


let 春日部つむぎ0 = 春日部つむぎ.HFlip()

let 春日部つむぎ1 =
    春日部つむぎ0
    |> Appearance.tryTurnOn [ "ほっぺ"; "かげり" ]
    |> unwrap
    |> Appearance.tryTurnOn [ "目"; "基本目セット"; "黒目"; "目逸らし" ]
    |> unwrap

let 四国めたん0 = 四国めたん |> Appearance.tryTurnOn [ "白ロリ服"; "左腕"; "普通" ] |> unwrap

let 四国めたん1 = 四国めたん0 |> Appearance.tryTurnOn [ "白ロリ服"; "左腕"; "口元に指" ] |> unwrap

//

let app app0 app1 : Frame.FrameAppearance list =
    [ { Appearance = app0; X = 0; Y = 0 }; { Appearance = app1; X = 800; Y = 0 } ]

let subtitle =
    {| Size = 50.0
       Weight = "bold"
       Family = "Noto Sans CJK JP"
       X = 400
       Y = 700
       Width = 1000
       Height = 300 |}

let color =
    {| 春日部つむぎ = "#FBCA4D"
       四国めたん = "#E7609E" |}

let frames: Frame.Frame list =
    [ { Speech =
          { Speaker = "春日部つむぎ"
            Style = "ノーマル"
            Talk = "モナドは単なる自己かん手の圏におけるモノイド対象だよ" }
        Subtitle =
          { Text = "モナドは単なる自己関手の圏におけるモノイド対象だよ．"
            FontColor = color.春日部つむぎ
            FontSize = subtitle.Size
            FontWeight = subtitle.Weight
            FontFamily = subtitle.Family
            X = subtitle.X
            Y = subtitle.Y
            Width = subtitle.Width
            Height = subtitle.Height }
        FrameAppearances = app 春日部つむぎ0 四国めたん0 }
      { Speech =
          { Speaker = "春日部つむぎ"
            Style = "ノーマル"
            Talk = "何か問題でも？" }
        Subtitle =
          { Text = "何か問題でも？"
            FontColor = color.春日部つむぎ
            FontSize = subtitle.Size
            FontWeight = subtitle.Weight
            FontFamily = subtitle.Family
            X = subtitle.X
            Y = subtitle.Y
            Width = subtitle.Width
            Height = subtitle.Height }
        FrameAppearances = app 春日部つむぎ1 四国めたん0 }
      { Speech =
          { Speaker = "四国めたん"
            Style = "ノーマル"
            Talk = "おっそうだな" }
        Subtitle =
          { Text = "おっそうだな．"
            FontColor = color.四国めたん
            FontSize = subtitle.Size
            FontWeight = subtitle.Weight
            FontFamily = subtitle.Family
            X = subtitle.X
            Y = subtitle.Y
            Width = subtitle.Width
            Height = subtitle.Height }
        FrameAppearances = app 春日部つむぎ1 四国めたん1 } ]

//

do
    let typst = "typst"

    let magick = ImageMagick("magick")

    use voicevox = new Voicevox()

    let ffmpeg = FFmpeg("ffmpeg")

    //

    let tmpDir = "tmp"

    if System.IO.Directory.Exists(tmpDir) then
        System.IO.Directory.Delete(tmpDir, true)

    System.IO.Directory.CreateDirectory(tmpDir) |> ignore

    //

    let frameOutputs = Frame.FrameOutput.framesToOutput typst magick voicevox frames

    let p =
        Frame.FrameOutput.exportVideo ffmpeg "sample/sample-10s.mp4" frameOutputs "output/output.mp4"

    let log = p.StandardError.ReadToEnd()

    printfn "%s" log
