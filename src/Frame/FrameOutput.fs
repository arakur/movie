namespace Frame

open Measure
open Types

open FFmpeg
open FFmpeg.FFmpegBuilder

open NAudio
open Appearance

type AppearanceArrangement =
    { Path: Path
      Pos: Pos
      Resize: Resize option }

type FrameOutput =
    { VoiceFile: Path
      SubtitleFile: Path
      Length: float<sec>
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
                          Margin =
                            speech.Subtitle.Font.Size
                            |> Option.map (fun size -> size * 0.5) // TODO: Magic number.
                            |> Option.defaultValue 5.0<pt> } // TODO: Magic number.
                      Text =
                        { Size = speech.Subtitle.Font.Size
                          Weight = speech.Subtitle.Font.Weight
                          Fill = speech.Subtitle.Font.Color }
                      FontFamily = speech.Subtitle.Font.Family
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

                let length =
                    float reader.Length / float reader.WaveFormat.AverageBytesPerSecond * 1.0<sec>

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
        (assets: Assets)
        (output: Path)
        =
        let voices =
            inputNodeN (
                frameOutputs
                |> List.map (fun frame ->
                    { Path = frame.VoiceFile
                      Arguments = [] })
            )
            |>> List.map InputNode.aInput

        let frameSlits =
            frameOutputs
            |> Seq.map (fun frame -> frame.Length)
            |> Seq.scan (+) 0.<sec>
            |> Array.ofSeq

        let frameDurations =
            frameSlits |> Seq.windowed 2 |> Seq.map (fun window -> window[0], window[1])

        let wholeLength = frameOutputs |> List.map (fun frame -> frame.Length) |> Seq.sum

        let backgroundNode =
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
                    do! trim (0.<sec>, wholeLength) colorInf color
                    return color, None
            }

        let assets' =
            assets.Values
            |> Seq.fold
                (fun acc asset ->
                    builder {
                        let! acc = acc

                        let arguments =
                            seq {
                                let trimStart, trimEnd = asset.Trim

                                if trimStart.IsSome then
                                    let start = trimStart.Value
                                    let offset = -start
                                    yield Arg.KV("ss", start |> string)
                                    yield Arg.KV("itsoffset", offset |> string)

                                if trimEnd.IsSome then
                                    let duration = trimEnd.Value - (trimStart |> Option.defaultValue 0.0<sec>)
                                    yield Arg.KV("t", duration |> string)
                            }
                            |> Seq.toList

                        let! input =
                            inputNode
                                { Path = asset.Path
                                  Arguments = arguments }

                        return (asset, input) :: acc
                    })
                ([] |> builder.Return)

        let collectAppSubtitleLayers frame duration layers =
            builder {
                let! layers = layers

                let! apps =
                    inputNodeN (frame.Arrangements |> List.map (fun app -> { Path = app.Path; Arguments = [] }))
                    |>> List.map InputNode.vInput

                let resizes = frame.Arrangements |> List.map (fun app -> app.Resize)

                let appLayers =
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

                return (subtitleLayer :: appLayers) :: layers
            }

        let collectAssetLayers (asset: Asset, assetInput) layers =
            builder {
                let! layers = layers

                match asset with
                | Asset.Image asset ->
                    let duration =
                        frameSlits.[asset.StartFrame],
                        asset.EndFrame
                        |> Option.map (fun endFrame -> frameSlits.[endFrame])
                        |> Option.defaultValue wholeLength

                    let imageLayer =
                        { Pos = asset.Pos
                          Resize = asset.Resize
                          Duration = Some duration
                          Input = assetInput.VInput
                          Shortest = false }

                    return [ imageLayer ] :: layers
                | Asset.Video asset ->

                    let duration =
                        frameSlits.[asset.StartFrame],
                        asset.EndFrame
                        |> Option.map (fun endFrame -> frameSlits.[endFrame])
                        |> Option.defaultValue wholeLength

                    let videoLayer =
                        { Pos = asset.Pos
                          Resize = asset.Resize
                          Duration = Some duration
                          Input = assetInput.VInput
                          Shortest = false }

                    return [ videoLayer ] :: layers
                | Asset.Audio _ -> return layers
            }

        let overlayLayers prevV layers =
            builder {
                let! prevV = prevV
                let! currentV = innerNode
                do! overlay layers prevV currentV
                return currentV
            }

        let voiceConcatA voices =
            builder {
                let! voiceConcatA = innerNode
                do! concatA voices voiceConcatA
                return voiceConcatA
            }

        let collectAssetAudios audios (asset, assetInput) =
            builder {
                let! audios = audios

                match asset with
                | Asset.Image _ -> return audios
                | Asset.Video _
                | Asset.Audio _ -> return assetInput.AInput :: audios
            }

        let outputV backgroundNodeV assets' =
            builder {
                let! frameLayers =
                    []
                    |> builder.Return
                    |> Seq.foldBack2 collectAppSubtitleLayers frameOutputs frameDurations
                    |> Seq.foldBack collectAssetLayers assets'

                return! frameLayers |> Seq.fold overlayLayers (builder.Return backgroundNodeV)
            }

        let outputA backgroundNodeA assets' =
            builder {
                let! voices = voices

                let! voiceConcatA = voiceConcatA voices

                let voiceBg =
                    match backgroundNodeA with
                    | Some backgroundA -> [ voiceConcatA; backgroundA ]
                    | None -> [ voiceConcatA ]

                let! audios = Seq.fold collectAssetAudios (voiceBg |> builder.Return) assets' |>> List.rev

                let! audiosDelayed = innerNodeN audios.Length

                do!
                    Seq.zip3 assets' audios audiosDelayed
                    |> Seq.fold
                        (fun acc ((asset, _), audio, delayed) ->
                            builder {
                                let delay =
                                    frameSlits.[asset.StartFrame]
                                    + (asset.Trim |> fst |> Option.defaultValue 0.0<sec>)

                                do! acc
                                do! adelay delay audio delayed
                            })
                        (builder.Return())

                let! outputA = innerNode
                do! mixAudio audiosDelayed outputA
                return outputA
            }

        let arguments =
            builder {
                let! backgroundNodeV, backgroundNodeA = backgroundNode
                let! assets' = assets'

                let! outputV = outputV backgroundNodeV assets'
                let! outputA = outputA backgroundNodeA assets'

                do! mapping outputV
                do! mapping outputA
            }
            |> FilterComplexStateM.build [ Arg.KV("pix_fmt", "yuv420p") ] output

        printfn "%s" <| arguments.Compose() // DEBUG

        ffmpeg.StartProcess arguments
