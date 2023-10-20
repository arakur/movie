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

type FrameOutputInfo =
    { VoiceFile: Path
      SubtitleFile: Path
      Length: float<sec>
      Arrangements: AppearanceArrangement list
      Pos: Pos }

type private ExportBuilder(env: Env.Env, assets': Assets, frames: Frame seq, background: Background) =
    let frameOutputInfos =
        let subtitleFiles: Path seq =
            frames |> Seq.mapi (fun i _ -> sprintf "%s/subtitle_%d.png" env.TmpDir i)

        let subtitleTasks =
            Seq.zip frames subtitleFiles
            |> Seq.map (fun (speech, subtitleFile) ->
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

        let speechFiles: Path seq =
            frames |> Seq.mapi (fun i _ -> sprintf "%s/voice_%d.wav" env.TmpDir i)

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
            ||> Seq.map2 (fun voice subtitle ->
                use reader = new Wave.WaveFileReader(voice)

                let length =
                    float reader.Length / float reader.WaveFormat.AverageBytesPerSecond * 1.0<sec>

                {| VoiceFile = voice
                   SubtitleFile = subtitle
                   Length = length |})
            |> Array.ofSeq

        //

        let appearanceMap =
            (Map.empty<Appearance.Appearance, string>, 0)
            |> Seq.foldBack
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
        |> Seq.mapi (fun i frame ->
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

    let slits =
        frameOutputInfos
        |> Seq.map (fun frame -> frame.Length)
        |> Seq.scan (+) 0.<sec>
        |> Array.ofSeq

    let wholeLength = slits |> Array.last

    let assets =
        let frameAssets = [] // TODO

        assets' |> Map.toSeq |> Seq.append frameAssets |> Map.ofSeq

    member private __.BackgroundNode =
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

    member private __.InputArguments assetWithDuration =
        seq {
            let asset = assetWithDuration.Asset

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

    member private this.TryVInput assetWithDuration =
        let asset = assetWithDuration.Asset
        let startFrame = assetWithDuration.StartFrame
        let endFrame = assetWithDuration.EndFrame
        let startSec = slits[startFrame]

        let endSec =
            endFrame
            |> Option.map (fun endFrame -> slits[endFrame])
            |> Option.defaultValue wholeLength

        let duration = endSec - startSec

        let arguments = this.InputArguments assetWithDuration

        builder {
            match asset with
            | :? IImageLayerAsset as asset ->
                let! { VInput = vInput } =
                    inputNode
                        { Path = asset.Path
                          Arguments = Arg.KV("loop", "1") :: Arg.KV("t", duration.ToString()) :: arguments }

                return Some vInput
            | :? ILayerAsset as asset ->
                let! { VInput = vInput } =
                    inputNode
                        { Path = asset.Path
                          Arguments = arguments }

                return Some vInput
            | _ -> return None
        }

    member private this.ToLayer(assetWithDuration: AssetWithDuration) =
        builder {
            match assetWithDuration.Asset with
            | :? ILayerAsset as asset ->
                let! vInput = this.TryVInput assetWithDuration |>> (fun o -> o.Value)

                let durationStart = slits[assetWithDuration.StartFrame]

                let durationEnd =
                    assetWithDuration.EndFrame
                    |> Option.map (fun frame -> slits[frame])
                    |> Option.defaultValue wholeLength

                return
                    Some
                        { Pos = asset.Pos
                          Resize = asset.Resize
                          Duration = Some(durationStart, durationEnd)
                          Input = vInput
                          Shortest = false }

            | _ -> return None
        }

    member private this.ToAudio(assetWithDuration: AssetWithDuration) =
        builder {
            match assetWithDuration.Asset with
            | :? IAudioAsset as asset ->
                let! { AInput = aInput } =
                    inputNode
                        { Path = asset.Path
                          Arguments = this.InputArguments assetWithDuration }

                return Some aInput
            | _ -> return None
        }

    member private this.OverlayLayers(backgroundNodeV) =
        let layers =
            (builder.Return [], assets.Values)
            ||> Seq.fold (fun acc assetWithDuration ->
                builder {
                    let! acc = acc

                    let! layer = this.ToLayer assetWithDuration

                    return layer :: acc
                })
            |>> List.choose id
            |>> List.rev

        builder {
            let! layers = layers
            let! outputV = innerNode
            do! overlay layers backgroundNodeV outputV
            return outputV
        }

    member private this.MiaAudios(backgroundNodeA) =
        let audios =
            (builder.Return [], assets.Values)
            ||> Seq.fold (fun acc assetWithDuration ->
                builder {
                    let! acc = acc

                    let! audio = this.ToAudio assetWithDuration

                    return audio :: acc
                })
            |>> List.choose id
            |>> (fun audios ->
                backgroundNodeA
                |> Option.map (fun bg -> bg :: audios)
                |> Option.defaultValue audios)
            |>> List.rev

        builder {
            let! audios = audios
            let! outputA = innerNode
            do! mixAudio audios outputA
            return outputA
        }

    member this.Compose =
        builder {
            let! backgroundNodeV, backgroundNodeA = this.BackgroundNode
            let! outputV = this.OverlayLayers backgroundNodeV
            let! outputA = this.MiaAudios backgroundNodeA
            do! mapping outputV
            do! mapping outputA
        }

module Export =
    let exportVideo (env: Env.Env) (background: Background) (frames: Frame seq) (assets: Assets) (output: Path) =
        let assetBuilder = ExportBuilder(env, assets, frames, background)

        let arguments =
            assetBuilder.Compose
            |> FilterComplexStateM.build [ Arg.KV("pix_fmt", "yuv420p") ] output

        printfn "%s" <| arguments.Compose() // DEBUG

        env.FFmpeg.StartProcess arguments


// and FrameOutput =
//     { VoiceFile: Path
//       SubtitleFile: Path
//       Length: float<sec>
//       Arrangements: AppearanceArrangement list
//       Pos: Pos }

//     static member framesToOutput (env: Env.Env) (frames: Frame list) =
//         let subtitleFiles: Path list =
//             frames
//             |> List.mapi (fun i _ -> i)
//             |> List.map (sprintf "%s/subtitle_%d.png" env.TmpDir)

//         let subtitleTasks =
//             Seq.zip frames subtitleFiles
//             |> Seq.mapi (fun i (speech, subtitleFile) ->
//                 let uuid = System.Guid.NewGuid().ToString("N")

//                 let typstOut = sprintf "%s/typst_out_%s.pdf" env.TmpDir uuid

//                 let content: Typst.TypstSource =
//                     { Page =
//                         { Width = px.asFloatPx speech.Subtitle.Size.Width * pt.perPx
//                           Height = px.asFloatPx speech.Subtitle.Size.Height * pt.perPx
//                           Margin =
//                             speech.Subtitle.Font.Size
//                             |> Option.map (fun size -> size * 0.5) // TODO: Magic number.
//                             |> Option.defaultValue 5.0<pt> } // TODO: Magic number.
//                       Text =
//                         { Size = speech.Subtitle.Font.Size
//                           Weight = speech.Subtitle.Font.Weight
//                           Fill = speech.Subtitle.Font.Color }
//                       FontFamily = speech.Subtitle.Font.Family
//                       Content = speech.Subtitle.Text }

//                 task {
//                     do! env.Typst.CompileAsync(content, typstOut)

//                     do!
//                         env.ImageMagick
//                             .Start(
//                                 sprintf
//                                     "convert %s -bordercolor none -border %d -background %s -alpha background -channel A -blur 0x1
//                                      -level 0,%f%% -resize %dx%d %s"
//                                     typstOut
//                                     10 // TODO: Magic number.
//                                     "black" // TODO: Make configurable.
//                                     0.1 // TODO: Magic number.
//                                     (int speech.Subtitle.Size.Width)
//                                     (int speech.Subtitle.Size.Height)
//                                     subtitleFile
//                             )
//                             .WaitForExitAsync()
//                 })
//             |> Seq.toList

//         let speechFiles: Path list =
//             frames |> List.mapi (fun i _ -> sprintf "%s/voice_%d.wav" env.TmpDir i)

//         let speechTask =
//             task {
//                 for frame, speechFile in Seq.zip frames speechFiles do
//                     match env.Voicevox.Synthesize frame.Speech with
//                     | Ok wav -> do! wav.SaveAsync(speechFile)
//                     | Error msg -> printfn "Error: %s" msg
//             }

//         speechTask :: subtitleTasks
//         |> Seq.map (fun task -> task |> Async.AwaitTask)
//         |> Async.Parallel
//         |> Async.Ignore
//         |> Async.RunSynchronously

//         //

//         let speechOutputs =
//             (speechFiles, subtitleFiles)
//             ||> List.map2 (fun voice subtitle ->
//                 use reader = new Wave.WaveFileReader(voice)

//                 let length =
//                     float reader.Length / float reader.WaveFormat.AverageBytesPerSecond * 1.0<sec>

//                 {| VoiceFile = voice
//                    SubtitleFile = subtitle
//                    Length = length |})

//         //

//         let appearanceMap =
//             (Map.empty<Appearance.Appearance, string>, 0)
//             |> List.foldBack
//                 (fun (frame: Frame) (map, count) ->
//                     frame.FrameAppearances
//                     |> Seq.map (fun appearance -> appearance.Appearance)
//                     |> Seq.fold
//                         (fun (map, count) appearance ->
//                             match Map.tryFind appearance map with
//                             | Some _ -> map, count
//                             | None ->
//                                 let path = sprintf "%s/app_%d.png" env.TmpDir count
//                                 appearance.Write(env.ImageMagick, path).WaitForExit()
//                                 Map.add appearance path map, count + 1)
//                         (map, count))
//                 frames
//             |> fst

//         frames
//         |> List.mapi (fun i frame ->
//             { VoiceFile = speechOutputs.[i].VoiceFile
//               SubtitleFile = speechOutputs.[i].SubtitleFile
//               Length = speechOutputs.[i].Length
//               Arrangements =
//                 frame.FrameAppearances
//                 |> Seq.map (fun appearance ->
//                     { Path = appearanceMap.[appearance.Appearance]
//                       Pos = appearance.Pos
//                       Resize = appearance.Resize })
//                 |> Seq.toList
//               Pos = frame.Subtitle.Pos })

//     static member exportVideo
//         (ffmpeg: FFmpeg)
//         (background: Background)
//         (frameOutputs: FrameOutput list)
//         (assets: Assets)
//         (output: Path)
//         =
//         let voices =
//             inputNodeN (
//                 frameOutputs
//                 |> List.map (fun frame ->
//                     { Path = frame.VoiceFile
//                       Arguments = [] })
//             )
//             |>> List.map InputNode.aInput

//         let frameSlits =
//             frameOutputs
//             |> Seq.map (fun frame -> frame.Length)
//             |> Seq.scan (+) 0.<sec>
//             |> Array.ofSeq

//         let state = AssetsBuildState.from frameOutputs

//         let frameDurations =
//             frameSlits |> Seq.windowed 2 |> Seq.map (fun window -> window[0], window[1])

//         let wholeLength = frameOutputs |> List.map (fun frame -> frame.Length) |> Seq.sum

//         let backgroundNode =
//             builder {
//                 match background with
//                 | Image path ->
//                     let! input =
//                         inputNode
//                             { Path = path
//                               Arguments = [ Arg.KV("loop", "1"); Arg.KV("t", wholeLength.ToString()) ] }

//                     return input.VInput, None
//                 | Video path ->
//                     let! input = inputNode { Path = path; Arguments = [] }

//                     return input.VInput, Some input.AInput
//                 | RGB(r, g, b) ->
//                     // FIXME: Cannot set a color source as background, it has overlaid on other sources.
//                     let! colorInf = innerNode
//                     let! color = innerNode
//                     do! colorSource r g b colorInf
//                     do! trim (0.<sec>, wholeLength) colorInf color
//                     return color, None
//             }

//         let assets' =
//             assets.Values
//             |> Seq.fold
//                 (fun acc assetWithDuration ->
//                     builder {
//                         let! acc = acc

//                         let! vInput = state.TryVInput assetWithDuration

//                         return (assetWithDuration, vInput) :: acc
//                     })
//                 ([] |> builder.Return)

//         let collectAppSubtitleLayers frame duration layers =
//             builder {
//                 let! layers = layers

//                 let! (apps: Node list) =
//                     inputNodeN (frame.Arrangements |> List.map (fun app -> { Path = app.Path; Arguments = [] }))
//                     |>> List.map InputNode.vInput

//                 let appLayers =
//                     (apps, frame.Arrangements)
//                     ||> List.map2 (fun app arr ->
//                         { Pos = arr.Pos
//                           Resize = arr.Resize
//                           Duration = Some duration
//                           Input = app
//                           Shortest = false })

//                 // TODO
//                 let subtitleAsset =
//                     { Path = frame.SubtitleFile
//                       Pos = frame.Pos }

//                 let! subtitle =
//                     inputNode
//                         { Path = frame.SubtitleFile
//                           Arguments = [] }

//                 let subtitleLayer =
//                     { Pos = frame.Pos
//                       Resize = None
//                       Duration = Some duration
//                       Input = subtitle.VInput
//                       Shortest = false }

//                 return (subtitleLayer :: appLayers) :: layers
//             }

//         let collectAssetLayers (assetWithDuration, assetInput) layers =
//             match assetInput with
//             | None -> layers
//             | Some assetInput ->
//                 builder {
//                     let! layers = layers

//                     let asset = assetWithDuration.Asset
//                     let startFrame = assetWithDuration.StartFrame
//                     let endFrame = assetWithDuration.EndFrame

//                     match asset with
//                     | :? ILayerAsset as asset ->
//                         let duration =
//                             frameSlits.[startFrame],
//                             endFrame
//                             |> Option.map (fun endFrame -> frameSlits.[endFrame])
//                             |> Option.defaultValue wholeLength

//                         let layer =
//                             { Pos = asset.Pos
//                               Resize = asset.Resize
//                               Duration = Some duration
//                               Input = assetInput
//                               Shortest = false }

//                         return [ layer ] :: layers
//                     | _ -> return layers
//                 }

//         let overlayLayers prevV layers =
//             builder {
//                 let! prevV = prevV
//                 let! currentV = innerNode
//                 do! overlay layers prevV currentV
//                 return currentV
//             }

//         let voiceConcatA voices =
//             builder {
//                 let! voiceConcatA = innerNode
//                 do! concatA voices voiceConcatA
//                 return voiceConcatA
//             }

//         let collectAssetAudios audios (assetWithDuration, assetInput) =
//             builder {
//                 let! audios = audios

//                 let asset = assetWithDuration.Asset

//                 match asset with
//                 | :? IAudioAsset as asset -> return assetInput.AInput :: audios
//                 | _ -> return audios
//             }

//         let outputV backgroundNodeV assets' =
//             builder {
//                 let! frameLayers =
//                     []
//                     |> builder.Return
//                     |> Seq.foldBack2 collectAppSubtitleLayers frameOutputs frameDurations
//                     |> Seq.foldBack collectAssetLayers assets'

//                 return! frameLayers |> Seq.fold overlayLayers (builder.Return backgroundNodeV)
//             }

//         let outputA backgroundNodeA assets' =
//             builder {
//                 let! voices = voices

//                 let! voiceConcatA = voiceConcatA voices

//                 let voiceBg =
//                     match backgroundNodeA with
//                     | Some backgroundA -> [ voiceConcatA; backgroundA ]
//                     | None -> [ voiceConcatA ]

//                 let! audios = Seq.fold collectAssetAudios (voiceBg |> builder.Return) assets' |>> List.rev

//                 let! audiosDelayed = innerNodeN audios.Length

//                 do!
//                     Seq.zip3 assets' audios audiosDelayed
//                     |> Seq.fold
//                         (fun acc ((assetWIthDuration, _), audio, delayed) ->
//                             builder {
//                                 let delay =
//                                     frameSlits.[assetWIthDuration.StartFrame]
//                                     + (assetWIthDuration.Asset.Trim |> fst |> Option.defaultValue 0.0<sec>)

//                                 do! acc
//                                 do! adelay delay audio delayed
//                             })
//                         (builder.Return())

//                 let! outputA = innerNode
//                 do! mixAudio audiosDelayed outputA
//                 return outputA
//             }

//         let arguments =
//             builder {
//                 let! backgroundNodeV, backgroundNodeA = backgroundNode
//                 let! assets' = assets'

//                 let! outputV = outputV backgroundNodeV assets'
//                 let! outputA = outputA backgroundNodeA assets'

//                 do! mapping outputV
//                 do! mapping outputA
//             }
//             |> FilterComplexStateM.build [ Arg.KV("pix_fmt", "yuv420p") ] output

//         printfn "%s" <| arguments.Compose() // DEBUG

//         ffmpeg.StartProcess arguments
