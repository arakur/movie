namespace FFmpeg

open System.Diagnostics

[<RequireQualifiedAccess>]
type Arg =
    | K of string
    | KV of key: string * value: string

    member this.Compose() =
        match this with
        | K k -> k
        | KV(k, v) -> $"-{k} {v}"

type FFmpegArguments =
    { Inputs: Path list
      FilterComplex: FilterComplex option
      Args: Arg list
      Output: Path }

    /// <summary>
    /// Compose a command line argument for FFmpeg.
    /// </summary>
    member this.Compose() =
        seq {
            yield "-y"

            for input in this.Inputs do
                yield "-i"
                yield $"\"{input}\""

            match this.FilterComplex with
            | Some filterComplex -> yield filterComplex.Compose()
            | None -> ()

            for arg in this.Args do
                yield arg.Compose()

            yield $"\"{this.Output}\""
        }
        |> String.concat " "

type FFmpeg(path: Path) =
    member __.StartProcess(arguments: FFmpegArguments) =
        let ffmpeg = ProcessStartInfo(path, arguments.Compose())

        ffmpeg.UseShellExecute <- false
        ffmpeg.RedirectStandardOutput <- true
        ffmpeg.RedirectStandardError <- true
        Process.Start(ffmpeg)
