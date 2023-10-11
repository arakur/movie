namespace Frame

open Measure
open Types

open FFmpeg
open FFmpeg.FFmpegBuilder

open NAudio

type AppearanceArrangement =
    { Path: Path
      Pos: Pos
      Resize: Resize option }

type FrameOutput =
    { VoiceFile: Path
      SubtitleFile: Path
      Length: float
      Arrangements: AppearanceArrangement list
      Pos: Pos }

    static member framesToOutput (env: Env.Env) (frames: Frame list) =
        let subtitleFiles: Path list =
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
                        { Width = px.asFloatPx speech.Subtitle.Size.Width * pt.perPx
                          Height = px.asFloatPx speech.Subtitle.Size.Height * pt.perPx
                          Margin = speech.Subtitle.Font.Size * 0.5 } // TODO: Magic number.
                      Text =
                        { Size = speech.Subtitle.Font.Size
                          Weight = speech.Subtitle.Font.Weight
                          Fill = speech.Subtitle.Font.Color }
                      Content = speech.Subtitle.Text }

                task {
                    do! env.Typst.CompileAsync(content, typstOut)

                    do!
                        env.ImageMagick
                            .Start(
                                sprintf
                                    "convert %s -bordercolor none -border %d -background %s -alpha background -channel A -blur 0x1
                                     -level 0,%f%% -resize %dx%d %s"
                                    typstOut
                                    10 // TODO: Magic number.
                                    "black" // TODO: Make configurable.
                                    0.1 // TODO: Magic number.
                                    (int speech.Subtitle.Size.Width)
                                    (int speech.Subtitle.Size.Height)
                                    subtitleFile
                            )
                            .WaitForExitAsync()
                })
            |> Seq.toList

        let speechFiles: Path list =
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
                      Pos = appearance.Pos
                      Resize = appearance.Resize })
                |> Seq.toList
              Pos = frame.Subtitle.Pos })

    static member exportVideo
        (ffmpeg: FFmpeg)
        (background: Background)
        (frameOutputs: FrameOutput list)
        (output: Path)
        =
        let arguments =
            builder {
                let! voices =
                    inputNodeN (
                        frameOutputs
                        |> List.map (fun frame ->
                            { Path = frame.VoiceFile
                              Arguments = [] })
                    )
                    |>> List.map InputNode.aInput

                let frameDurations =
                    frameOutputs
                    |> Seq.map (fun frame -> frame.Length)
                    |> Seq.scan (+) 0.0
                    |> Seq.windowed 2
                    |> Seq.map (fun window -> window[0], window[1])

                let wholeLength = frameOutputs |> List.map (fun frame -> frame.Length) |> Seq.sum

                let! backgroundNodeV, backgroundNodeA =
                    builder {
                        match background with
                        | Image path ->
                            let! input =
                                inputNode
                                    { Path = path
                                      Arguments = [ Arg.KV("loop", "1"); Arg.KV("t", wholeLength.ToString()) ] }

                            return input.VInput, None
                        | Video path ->
                            let! input = inputNode { Path = path; Arguments = [] }

                            return input.VInput, Some input.AInput
                        | RGB(r, g, b) ->
                            // FIXME: Cannot set a color source as background, it has overlaid on other sources.
                            let! colorInf = innerNode
                            let! color = innerNode
                            do! colorSource r g b colorInf
                            do! trim (0, wholeLength) colorInf color
                            return color, None
                    }

                let mutable prevV = backgroundNodeV

                for frame, duration in Seq.zip frameOutputs frameDurations do
                    let! currentV = innerNode

                    let! apps =
                        inputNodeN (frame.Arrangements |> List.map (fun app -> { Path = app.Path; Arguments = [] }))
                        |>> List.map InputNode.vInput

                    let resizes = frame.Arrangements |> List.map (fun app -> app.Resize)

                    let layers =
                        (frame.Arrangements, resizes)
                        ||> List.mapi2 (fun i arr resize ->
                            { Pos = arr.Pos
                              Resize = resize
                              Duration = Some duration
                              Input = apps.[i]
                              Shortest = false })

                    let! subtitle =
                        inputNode
                            { Path = frame.SubtitleFile
                              Arguments = [] }

                    let subtitleLayer =
                        { Pos = frame.Pos
                          Resize = None
                          Duration = Some duration
                          Input = subtitle.VInput
                          Shortest = false }

                    do! overlay (layers @ [ subtitleLayer ]) prevV currentV
                    prevV <- currentV

                let! voiceConcatA = innerNode

                do! concatA voices voiceConcatA

                let outputV = prevV

                let! outputA =
                    builder {
                        match backgroundNodeA with
                        | Some backgroundA ->
                            let! outputA = innerNode
                            do! mixAudio [ backgroundA; voiceConcatA ] outputA
                            return outputA
                        | None -> return voiceConcatA
                    }

                do! mapping outputV
                do! mapping outputA
            }
            |> FilterComplexStateM.build [ Arg.KV("pix_fmt", "yuv420p") ] output

        // printfn "%s" <| arguments.Compose() // DEBUG

        ffmpeg.StartProcess arguments
