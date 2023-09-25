namespace Frame

open FFmpeg
open FFmpeg.FFmpegBuilder

open NAudio

type Subtitle =
    { Text: string
      FontColor: string
      FontSize: float
      FontWeight: string
      FontFamily: string
      X: int
      Y: int
      Width: int
      Height: int }

type FrameAppearance =
    { Appearance: Appearance.Appearance
      X: int
      Y: int }

type Frame =
    { Speech: Voicevox.Speech
      Subtitle: Subtitle
      FrameAppearances: FrameAppearance list }

//

type AppearanceArrangement = { Path: string; X: int; Y: int }

type FrameOutput =
    { VoiceFile: string
      SubtitleFile: string
      Length: float
      Arrangements: AppearanceArrangement list
      SubtitlePosition: {| X: int; Y: int |} }

    static member framesToOutput (typst: string) (magick: ImageMagick.ImageMagick) (frames: Frame list) =
        let subtitleFiles: string list =
            frames |> List.mapi (fun i _ -> i) |> List.map (sprintf "tmp/subtitle_%d.png")

        let subtitleFilesTask =
            Seq.zip frames subtitleFiles
            |> Seq.mapi (fun i (speech, subtitleFile) ->
                let typstSrc = sprintf "tmp/typst_src%d.typ" i
                let typstOut = sprintf "tmp/typst_out%d.pdf" i

                let content =
                    sprintf
                        "#set page(width: %fem, height: %fem, margin: 1em);#set text(size: %fpt, weight: \"%s\", fill: rgb(\"%s\"));%s"
                        (float speech.Subtitle.Width / speech.Subtitle.FontSize)
                        (float speech.Subtitle.Height / speech.Subtitle.FontSize)
                        speech.Subtitle.FontSize
                        speech.Subtitle.FontWeight
                        speech.Subtitle.FontColor
                        speech.Subtitle.Text

                System.IO.File.WriteAllText(typstSrc, content)

                let typstProc =
                    System.Diagnostics.Process.Start(typst, sprintf "compile %s %s" typstSrc typstOut)

                let magickProc =
                    magick.Start(
                        sprintf
                            "convert -density %d %s -resize %dx%d ( +clone -channel RGB -negate +channel -blur 0x10 ) +swap -background none -flatten %s"
                            300
                            typstOut
                            speech.Subtitle.Width
                            speech.Subtitle.Height
                            subtitleFile
                    )

                async {
                    typstProc.WaitForExitAsync() |> Async.AwaitTask |> Async.RunSynchronously
                    magickProc.WaitForExitAsync() |> Async.AwaitTask |> Async.RunSynchronously
                })
            |> Seq.toList

        let speechFiles: string list =
            frames |> List.mapi (fun i _ -> i) |> List.map (sprintf "tmp/voice_%d.wav")

        let speechFilesTask =
            async {
                use voicevox = new Voicevox.Voicevox()

                for frame, speechFile in Seq.zip frames speechFiles do
                    match voicevox.Synthesize frame.Speech with
                    | Ok wav -> wav.Save(speechFile)
                    | Error msg -> printfn "Error: %s" msg
            }

        (speechFilesTask :: subtitleFilesTask)
        |> Async.Parallel
        |> Async.RunSynchronously
        |> ignore

        //

        let speechOutputs =
            (speechFiles, subtitleFiles)
            ||> List.map2 (fun voice subtitle ->
                use reader = new Wave.WaveFileReader(voice)
                let length = float reader.Length / float reader.WaveFormat.AverageBytesPerSecond

                {| VoiceFile = voice
                   SubtitleFile = subtitle
                   Length = length |})

        //

        let appearanceMap =
            (Map.empty<Appearance.Appearance, string>, 0)
            |> List.foldBack
                (fun (frame: Frame) (map, count) ->
                    frame.FrameAppearances
                    |> List.map (fun appearance -> appearance.Appearance)
                    |> List.fold
                        (fun (map, count) appearance ->
                            match Map.tryFind appearance map with
                            | Some _ -> map, count
                            | None ->
                                let path = sprintf "tmp/app_%d.png" count
                                appearance.Write(magick, path).WaitForExit()
                                Map.add appearance path map, count + 1)
                        (map, count))
                frames
            |> fst

        frames
        |> List.mapi (fun i frame ->
            { VoiceFile = speechOutputs.[i].VoiceFile
              SubtitleFile = speechOutputs.[i].SubtitleFile
              Length = speechOutputs.[i].Length
              Arrangements =
                frame.FrameAppearances
                |> List.map (fun appearance ->
                    { Path = appearanceMap.[appearance.Appearance]
                      X = appearance.X
                      Y = appearance.Y })
              SubtitlePosition =
                {| X = frame.Subtitle.X
                   Y = frame.Subtitle.Y |} })



    static member exportVideo (ffmpeg: FFmpeg) (background: string) (frameOutputs: FrameOutput list) (output: string) =
        let arguments =
            builder {
                let! frameNodesV = innerNodeN frameOutputs.Length

                let! { VInput = backgroundV
                       AInput = backgroundA } = inputNode background

                let! voices =
                    inputNodeN (frameOutputs |> List.map (fun frame -> frame.VoiceFile))
                    |>> List.map InputNode.aInput

                let mutable currentSec: float = 0

                let mutable prevV = backgroundV

                for frame, frameNodeV in Seq.zip frameOutputs frameNodesV do
                    let! currentV = innerNode

                    let! apps =
                        inputNodeN (frame.Arrangements |> List.map (fun arr -> arr.Path))
                        |>> List.map InputNode.vInput


                    let duration = (currentSec, currentSec + frame.Length)
                    currentSec <- currentSec + frame.Length

                    let layers =
                        frame.Arrangements
                        |> List.mapi (fun i arr ->
                            { X = arr.X
                              Y = arr.Y
                              Duration = Some duration
                              Input = apps.[i]
                              Shortest = false })

                    let! subtitle = inputNode frame.SubtitleFile

                    let subtitleLayer =
                        { X = frame.SubtitlePosition.X
                          Y = frame.SubtitlePosition.Y
                          Duration = Some duration
                          Input = subtitle.VInput
                          Shortest = false }

                    do! overlay (layers @ [ subtitleLayer ]) prevV currentV
                    prevV <- currentV

                let! voiceConcatA = innerNode

                do! concatA voices voiceConcatA

                let outputV = prevV
                let! outputA = innerNode

                do! mixAudio [ backgroundA; voiceConcatA ] outputA

                do! mapping outputV
                do! mapping outputA
            }
            |> FilterComplexStateM.build [ Arg.KV("pix_fmt", "yuv420p") ] output

        // let graphViz = arguments.FilterComplex.Value.ToGraphVizDot() // DEBUG

        // System.IO.File.WriteAllText("tmp/filter_complex.dot", graphViz) // DEBUG

        ffmpeg.StartProcess arguments
