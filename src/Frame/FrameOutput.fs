namespace Frame

open Measure

open FFmpeg
open FFmpeg.FFmpegBuilder

open NAudio

type AppearanceArrangement = { Path: string; X: int; Y: int }

type FrameOutput =
    { VoiceFile: string
      SubtitleFile: string
      Length: float
      Arrangements: AppearanceArrangement list
      SubtitlePosition: {| X: int; Y: int |} }

    static member framesToOutput (env: Env.Env) (frames: Frame list) =
        let subtitleFiles: string list =
            frames
            |> List.mapi (fun i _ -> i)
            |> List.map (sprintf "%s/subtitle_%d.png" env.TmpDir)

        let subtitleTasks =
            Seq.zip frames subtitleFiles
            |> Seq.mapi (fun i (speech, subtitleFile) ->
                let uuid = System.Guid.NewGuid().ToString("N")

                let typstOut = sprintf "%s/typst_out_%s.pdf" env.TmpDir uuid

                let content: Typst.TypstSource =
                    { Page =
                        { Width = px.asFloatPx speech.Subtitle.Width * pt.perPx
                          Height = px.asFloatPx speech.Subtitle.Height * pt.perPx
                          Margin = speech.Subtitle.FontSize * 0.5 } // TODO: Magic number.
                      Text =
                        { Size = speech.Subtitle.FontSize
                          Weight = speech.Subtitle.FontWeight
                          Fill = speech.Subtitle.FontColor }
                      Content = speech.Subtitle.Text }

                task {
                    do! env.Typst.CompileAsync(content, typstOut)

                    do!
                        env.ImageMagick
                            .Start(
                                sprintf
                                    "convert %s -bordercolor none -border %d -background %s -alpha background -channel A -blur 0x1 -level 0,%f%% %s"
                                    typstOut
                                    10 // TODO: Magic number.
                                    "black" // TODO: Make configurable.
                                    0.1 // TODO: Magic number.
                                    subtitleFile
                            )
                            .WaitForExitAsync()
                })
            |> Seq.toList

        let speechFiles: string list =
            frames |> List.mapi (fun i _ -> sprintf "%s/voice_%d.wav" env.TmpDir i)

        let speechTask =
            task {
                for frame, speechFile in Seq.zip frames speechFiles do
                    match env.Voicevox.Synthesize frame.Speech with
                    | Ok wav -> do! wav.SaveAsync(speechFile)
                    | Error msg -> printfn "Error: %s" msg
            }

        speechTask :: subtitleTasks
        |> Seq.map (fun task -> task |> Async.AwaitTask)
        |> Async.Parallel
        |> Async.Ignore
        |> Async.RunSynchronously

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
                    |> Seq.map (fun appearance -> appearance.Appearance)
                    |> Seq.fold
                        (fun (map, count) appearance ->
                            match Map.tryFind appearance map with
                            | Some _ -> map, count
                            | None ->
                                let path = sprintf "%s/app_%d.png" env.TmpDir count
                                appearance.Write(env.ImageMagick, path).WaitForExit()
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
                |> Seq.map (fun appearance ->
                    { Path = appearanceMap.[appearance.Appearance]
                      X = int appearance.X
                      Y = int appearance.Y })
                |> Seq.toList
              SubtitlePosition =
                {| X = int frame.Subtitle.X
                   Y = int frame.Subtitle.Y |} })

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

                for frame in frameOutputs do
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

        ffmpeg.StartProcess arguments