namespace Env

open Types

type Env(tmpDir: Path, ?typst: Path, ?imageMagick: Path, ?ffmpeg: Path) =
    let voicevox = new Voicevox.Voicevox()
    do System.IO.Directory.CreateDirectory(tmpDir) |> ignore

    member val Typst = Typst.Typst(typst |> Option.defaultValue "typst") with get
    member val ImageMagick = ImageMagick.ImageMagick(imageMagick |> Option.defaultValue "magick") with get
    member val Voicevox = voicevox with get
    member val FFmpeg = FFmpeg.FFmpeg(ffmpeg |> Option.defaultValue "ffmpeg") with get
    member val TmpDir = tmpDir with get

    interface System.IDisposable with
        member __.Dispose() =
            (voicevox :> System.IDisposable).Dispose()
            System.IO.Directory.Delete(tmpDir, true) |> ignore
