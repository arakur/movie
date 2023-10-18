namespace Script

open FSharpPlus
open FSharpPlus.Data
open FSharpx.Collections

module Interpreter =
    let rec tryEval (env: IEvalEnv) (expr: Expr) : Result<Value, string> =
        match expr with
        | Expr.Numeral(n, m) ->
            if n |> String.contains '.' then
                try
                    Ok(Value.Numeral(Numeral.Float(float n, m)))
                with _ ->
                    Error("Invalid float.")
            else
                try
                    Ok(Value.Numeral(Numeral.Int(int n, m)))
                with _ ->
                    Error("Invalid int.")
        | Expr.String s -> Ok(Value.String s)
        | Expr.Variable v -> env.TryVariable v |> Option.toResultWith (sprintf "Variable '%s' not found." v)
        | Expr.App(f, args) ->
            (f |> tryEval env, args)
            ||> Seq.fold (fun acc arg ->
                monad {
                    let! acc = acc
                    let! arg = arg |> tryEval env
                    return! Value.tryApply env arg acc
                })
        | Expr.BinaryOperator(op, expr0, expr1) ->
            monad {
                let! lhs = expr0 |> tryEval env
                let! rhs = expr1 |> tryEval env

                let! f =
                    env.TryBinaryOperator op.Name
                    |> Option.toResultWith (sprintf "Binary operator '%s' not found." op.Name)

                return! f lhs rhs
            }
        | Expr.Tuple exprs ->
            exprs
            |> List.map (tryEval env)
            |> ResultExt.sequence
            |> Result.map Seq.toArray
            |> Result.map Value.Tuple

    type private FontState =
        { Color: Types.Color option
          Family: string RevList
          Size: float<Measure.pt> option
          Weight: Typst.Weight option }

        static member tryCompose(this: FontState) : Result<Frame.SubtitleFont, string> =
            monad {
                let color = this.Color
                let family = this.Family |> RevList.toList
                let size = this.Size
                let weight = this.Weight

                return
                    { Color = color
                      Family = family
                      Size = size
                      Weight = weight }
            }


        static member tryComposeUpdate(this: FontState) : Result<Frame.SubtitleFontUpdate, string> =
            monad {
                let color = this.Color
                let family = this.Family |> RevList.toList
                let size = this.Size
                let weight = this.Weight

                return
                    { Color = color
                      Family = family
                      Size = size
                      Weight = weight }
            }

    let private runFont (block: Statement list option) (env: EvalEnv, fontState: FontState option) =
        let runFontStatement
            (acc: Result<EvalEnv * FontState option, string>)
            (statement: Statement)
            : Result<EvalEnv * FontState option, string> =
            monad {
                let! env, font = acc

                match statement with
                | Gets(targetExpr, contentExpr) ->
                    let! target = targetExpr |> tryEval env
                    let! content = contentExpr |> tryEval env

                    let font' =
                        font
                        |> Option.defaultValue
                            { Color = None
                              Size = None
                              Weight = None
                              Family = RevList.empty }

                    let! fieldName = target |> Value.tryAsString

                    match fieldName with
                    | "color" ->
                        let! r, g, b =
                            content
                            |> Value.tryAsTuple
                            |> Result.bind (Seq.map Value.tryAsInt >> ResultExt.sequence)
                            |> Result.map Array.ofSeq
                            |> Result.bind (ArrayExt.tryAsTuple3 "Expected a tuple with 3 elements.")

                        let font'' =
                            { font' with
                                Color = Some(Types.RGB(r, g, b)) }

                        return env, Some font''
                    | "size" ->
                        let! size = content |> Value.tryAsFloatPt

                        let font'' = { font' with Size = Some size }

                        return env, Some font''
                    | "weight" ->
                        let tryNamedWeight () =
                            monad {
                                let! name = content |> Value.tryAsString

                                let! weight = name |> Typst.Weight.tryFrom |> Option.toResultWith "Invalid weight name."

                                let font'' = { font' with Weight = Some weight }

                                return env, Some font''
                            }

                        let tryIntWeight () =
                            monad {
                                let! weightValue = content |> Value.tryAsInt

                                let font'' =
                                    { font' with
                                        Weight = Some(Typst.Weight.OfInt weightValue) }

                                return env, Some font''
                            }

                        return! tryNamedWeight () |> ResultExt.orElse tryIntWeight
                    | "family" ->
                        let! family = content |> Value.tryAsString

                        let font'' =
                            { font' with
                                Family = font'.Family |> RevList.add family }

                        return env, Some font''
                    | _ -> return! Error $"Unknown field name `{fieldName}` in font."
                | _ -> return! Error "Invalid statement."
            }

        let init = Ok(env, fontState)

        block
        |> Option.toResultWith "Expected block."
        |> Result.bind (Seq.fold runFontStatement init)

    //

    type private PosState =
        { X: int<Measure.px> option
          Y: int<Measure.px> option }

        static member tryCompose(this: PosState) : Result<Types.Pos, string> =
            monad {
                let! posX = this.X |> Option.toResultWith "X is not set."
                let! posY = this.Y |> Option.toResultWith "Y is not set."

                return
                    { Types.Pos.X = posX
                      Types.Pos.Y = posY }
            }

    let private runPos (block: Statement list option) (env: EvalEnv, posState: PosState option) =
        let runPosStatement
            (acc: Result<EvalEnv * PosState option, string>)
            (statement: Statement)
            : Result<EvalEnv * PosState option, string> =
            monad {
                let! env, pos = acc

                match statement with
                | Gets(targetExpr, contentExpr) ->
                    let! fieldName = targetExpr |> tryEval env |> Result.bind Value.tryAsString

                    let pos' = pos |> Option.defaultValue { X = None; Y = None }

                    match fieldName with
                    | "x" ->
                        let! x = contentExpr |> tryEval env |> Result.bind Value.tryAsFloatPxToInt

                        let pos'' = { pos' with X = Some x }

                        return env, Some pos''
                    | "y" ->
                        let! y = contentExpr |> tryEval env |> Result.bind Value.tryAsFloatPxToInt

                        let pos'' = { pos' with Y = Some y }

                        return env, Some pos''
                    | _ -> return! Error $"Unknown field name `{fieldName}` in pos."
                | _ -> return! Error "Invalid statement."
            }

        let init = Ok(env, posState)

        block
        |> Option.toResultWith "Expected block."
        |> Result.bind (Seq.fold runPosStatement init)

    type private DurationState =
        { Start: float<Measure.sec> option
          End: float<Measure.sec> option }

        static member tryCompose
            (this: DurationState)
            : Result<float<Measure.sec> option * float<Measure.sec> option, string> =
            monad { return (this.Start, this.End) }

    let private runDuration (block: Statement list option) (env: EvalEnv, durationState: DurationState option) =
        let runDurationStatement
            (acc: Result<EvalEnv * DurationState option, string>)
            (statement: Statement)
            : Result<EvalEnv * DurationState option, string> =
            monad {
                let! env, duration = acc

                match statement with
                | Gets(targetExpr, contentExpr) ->
                    let! fieldName = targetExpr |> tryEval env |> Result.bind Value.tryAsString

                    let duration' = duration |> Option.defaultValue { Start = None; End = None }

                    match fieldName with
                    | "start" ->
                        let! start = contentExpr |> tryEval env |> Result.bind Value.tryAsFloatSec

                        let pos'' = { duration' with Start = Some start }

                        return env, Some pos''
                    | "end" ->
                        let! end_ = contentExpr |> tryEval env |> Result.bind Value.tryAsFloatSec

                        let pos'' = { duration' with End = Some end_ }

                        return env, Some pos''
                    | _ -> return! Error $"Unknown field name `{fieldName}` in pos."
                | _ -> return! Error "Invalid statement."
            }

        let init = Ok(env, durationState)

        block
        |> Option.toResultWith "Expected block."
        |> Result.bind (Seq.fold runDurationStatement init)

    type private SizeState =
        { Width: int<Measure.px> option
          Height: int<Measure.px> option }

        static member tryCompose(this: SizeState) : Result<Types.Size, string> =
            monad {
                let! width = this.Width |> Option.toResultWith "Width is not set."
                let! height = this.Height |> Option.toResultWith "Height is not set."

                return
                    { Types.Size.Width = width
                      Types.Size.Height = height }
            }

    let private runSize (block: Statement list option) (env: EvalEnv, sizeState: SizeState option) =
        let runSizeStatement
            (acc: Result<EvalEnv * SizeState option, string>)
            (statement: Statement)
            : Result<EvalEnv * SizeState option, string> =
            monad {
                let! env, size = acc

                match statement with
                | Gets(targetExpr, contentExpr) ->
                    let! target = targetExpr |> tryEval env
                    let! content = contentExpr |> tryEval env

                    let size' = size |> Option.defaultValue { Width = None; Height = None }

                    let! fieldName = target |> Value.tryAsString

                    match fieldName with
                    | "width" ->
                        let! width = content |> Value.tryAsFloatPxToInt

                        let size'' = { size' with Width = Some width }

                        return env, Some size''
                    | "height" ->
                        let! height = content |> Value.tryAsFloatPxToInt

                        let size'' = { size' with Height = Some height }

                        return env, Some size''
                    | _ -> return! Error $"Unknown field name `{fieldName}` in size."
                | _ -> return! Error "Invalid statement."
            }

        let init = Ok(env, sizeState)

        block
        |> Option.toResultWith "Expected block."
        |> Result.bind (Seq.fold runSizeStatement init)

    type private ResizeState =
        { Width: int<Measure.px> option
          Height: int<Measure.px> option
          Scale: float option
          ScaleX: float option
          ScaleY: float option }

        member this.TryToResize =
            match this with
            | { Scale = Some scale
                ScaleX = None
                ScaleY = None
                Width = None
                Height = None } -> Ok(Some(Types.Resize.Scale scale))
            | { Scale = None
                ScaleX = Some scaleX
                ScaleY = Some scaleY
                Width = None
                Height = None } -> Ok(Some(Types.Resize.ScaleXY(scaleX, scaleY)))
            | { Scale = None
                ScaleX = None
                ScaleY = None
                Width = Some width
                Height = Some height } -> Ok(Some(Types.Resize.Size(width, height)))
            | { Scale = None
                ScaleX = None
                ScaleY = None
                Width = Some width
                Height = None } -> Ok(Some(Types.Resize.SizeX width))
            | { Scale = None
                ScaleX = None
                ScaleY = None
                Width = None
                Height = Some height } -> Ok(Some(Types.Resize.SizeY height))
            | _ -> Error "Fields of resize are not given correctly." // TODO

    let private runResize (block: Statement list option) (env: EvalEnv, resizeState: ResizeState option) =
        let runResizeStatement
            (acc: Result<EvalEnv * ResizeState option, string>)
            (statement: Statement)
            : Result<EvalEnv * ResizeState option, string> =
            monad {
                let! env, resize = acc

                match statement with
                | Gets(targetExpr, contentExpr) ->
                    let! target = targetExpr |> tryEval env
                    let! content = contentExpr |> tryEval env

                    let resize' =
                        resize
                        |> Option.defaultValue
                            { Width = None
                              Height = None
                              Scale = None
                              ScaleX = None
                              ScaleY = None }

                    let! fieldName = target |> Value.tryAsString

                    match fieldName with
                    | "width" ->
                        let! width = content |> Value.tryAsFloatPxToInt

                        let resize'' = { resize' with Width = Some width }

                        return env, Some resize''
                    | "height" ->
                        let! height = content |> Value.tryAsFloatPxToInt

                        let resize'' = { resize' with Height = Some height }

                        return env, Some resize''
                    | "scale" ->
                        let! scale = content |> Value.tryAsFloat

                        let resize'' = { resize' with Scale = Some scale }

                        return env, Some resize''
                    | "scale-x" ->
                        let! scaleX = content |> Value.tryAsFloat

                        let resize'' = { resize' with ScaleX = Some scaleX }

                        return env, Some resize''
                    | "scale-y" ->
                        let! scaleY = content |> Value.tryAsFloat

                        let resize'' = { resize' with ScaleY = Some(scaleY) }

                        return env, Some resize''
                    | _ -> return! Error $"Unknown field name `{fieldName}` in resize."
                | _ -> return! Error "Invalid statement."
            }

        let init = Ok(env, resizeState)

        block
        |> Option.toResultWith "Expected block."
        |> Result.bind (Seq.fold runResizeStatement init)

    type private InitializeState =
        { Font: FontState option
          Pos: PosState option
          Size: SizeState option
          Background: Frame.Background option }

        static member tryCompose this =
            monad {
                let! font =
                    this.Font
                    |> Option.toResultWith "Font is not set."
                    |> Result.bind FontState.tryCompose

                let! pos =
                    this.Pos
                    |> Option.toResultWith "Pos is not set."
                    |> Result.bind PosState.tryCompose

                let! size =
                    this.Size
                    |> Option.toResultWith "Size is not set."
                    |> Result.bind SizeState.tryCompose

                let! background = this.Background |> Option.toResultWith "Background is not set."

                return
                    { Frame.Initialize.Font = font
                      Frame.Initialize.Pos = pos
                      Frame.Initialize.Size = size
                      Frame.Initialize.Background = background }
            }

    let runInitialize
        (movie: Frame.MovieBuilder)
        (args: Value array)
        (block: Statement list option)
        (env: EvalEnv, _state: Frame.MovieState)
        : Result<EvalEnv * Frame.MovieState, string> =
        let initialState =
            { Font = None
              Pos = None
              Size = None
              Background = None }

        let runInitializeStatement (acc: Result<EvalEnv * InitializeState, string>) (statement: Statement) =
            monad {
                let! env, config = acc

                match statement with
                | Gets(targetExpr, contentExpr) ->
                    let! target = targetExpr |> tryEval env |> Result.bind Value.tryAsString

                    match target with
                    | "background" ->
                        let! backgroundTypeValue, backgroundContentValue =
                            contentExpr
                            |> tryEval env
                            |> Result.bind Value.tryAsTuple
                            |> Result.bind (ArrayExt.tryAsTuple2 "Expected a tuple with 2 elements.")

                        let! backgroundType = backgroundTypeValue |> Value.tryAsString

                        match backgroundType with
                        | "video" ->
                            let! path = backgroundContentValue |> Value.tryAsString
                            let background = Frame.Background.Video path

                            let config' =
                                { config with
                                    Background = Some background }

                            return env, config'
                        | "image" ->
                            let! path = backgroundContentValue |> Value.tryAsString
                            let background = Frame.Background.Image path

                            let config' =
                                { config with
                                    Background = Some background }

                            return env, config'
                        | "rgb" ->
                            let! r, g, b =
                                backgroundContentValue
                                |> Value.tryAsTuple
                                |> Result.bind (Seq.map Value.tryAsInt >> ResultExt.sequence)
                                |> Result.map Array.ofSeq
                                |> Result.bind (ArrayExt.tryAsTuple3 "Expected a tuple with 3 elements.")

                            let background = Frame.Background.RGB(r, g, b)

                            let config' =
                                { config with
                                    Background = Some background }

                            return env, config'
                        | _ -> return! Error "Unknown background type."
                    | _ -> return! Error "Invalid assignment."
                | Do(expr, optBlock) ->
                    let! fieldName = expr |> tryEval env |> Result.bind Value.tryAsString

                    match fieldName with
                    | "font" ->
                        let! env', font' = runFont optBlock (env, config.Font)
                        return env', { config with Font = font' }
                    | "pos" ->
                        let! env', pos' = runPos optBlock (env, config.Pos)
                        return env', { config with Pos = pos' }
                    | "size" ->
                        let! env', size' = runSize optBlock (env, config.Size)
                        return env', { config with Size = size' }
                    | _ -> return! Error $"Unknown field name `{fieldName}` in initialize."
                | _ -> return! Error "Invalid statement."
            }

        let buildConfig (env: EvalEnv, config: InitializeState) =
            monad {
                let! config' = config |> InitializeState.tryCompose

                let state' = movie.Initialize((), config')

                return env, state'
            }

        monad {
            do! args |> ArrayExt.tryAsEmpty "Expected no arguments."
            let! block' = block |> Option.toResultWith "Expected block."
            let! config = (Ok(env, initialState), block') ||> Seq.fold runInitializeStatement

            return! buildConfig config
        }

    //

    type private AddSpeakerAppearanceState =
        { Path: string option
          Pos: PosState option
          Resize: ResizeState option }

        static member tryCompose(this: AddSpeakerAppearanceState) : Result<Frame.FrameAppearance, string> =
            monad {
                let! path = this.Path |> Option.toResultWith "Path is not set."

                let! pos =
                    this.Pos
                    |> Option.toResultWith "Pos is not set."
                    |> Result.bind PosState.tryCompose

                let! resize =
                    this.Resize
                    |> function
                        | None -> Ok None
                        | Some resize -> resize.TryToResize

                let appearance = Appearance.Appearance.LoadDirectory path

                return
                    { Appearance = appearance
                      Pos = pos
                      Resize = resize }
            }

    let private runAddSpeakerAppearance
        (block: Statement list option)
        (env: EvalEnv, appState: AddSpeakerAppearanceState option)
        =
        let runAddSpeakerAppearanceStatement
            (acc: Result<EvalEnv * AddSpeakerAppearanceState option, string>)
            (statement: Statement)
            : Result<EvalEnv * AddSpeakerAppearanceState option, string> =
            monad {
                let! env, appState = acc

                let appState' =
                    appState
                    |> Option.defaultValue
                        { Path = None
                          Pos = None
                          Resize = None }

                match statement with
                | Do(expr, optBlock) ->
                    let! fieldName = expr |> tryEval env |> Result.bind Value.tryAsString

                    match fieldName with
                    | "pos" ->
                        let! env', pos' = runPos optBlock (env, appState'.Pos)
                        let appState'' = { appState' with Pos = pos' }
                        return env', Some appState''
                    | "resize" ->
                        let! env', resize' = runResize optBlock (env, appState'.Resize)
                        let appState'' = { appState' with Resize = resize' }
                        return env', Some appState''
                    | _ -> return! Error $"Unknown field name `{fieldName}` in appearance."
                | Gets(targetExpr, contentExpr) ->
                    let! fieldName = targetExpr |> tryEval env |> Result.bind Value.tryAsString
                    let! content = contentExpr |> tryEval env

                    match fieldName with
                    | "path" ->
                        let! path = content |> Value.tryAsString

                        let appState'' = { appState' with Path = Some path }

                        return env, Some appState''
                    | _ -> return! Error $"Unknown field name `{fieldName}` in appearance."
                | _ -> return! Error "Invalid statement."
            }

        let init = Ok(env, appState)

        block
        |> Option.toResultWith "Expected block."
        |> Result.bind (Seq.fold runAddSpeakerAppearanceStatement init)

    type private AddSpeakerState =
        { Name: string option
          Style: string option
          Font: FontState option
          Appearance: AddSpeakerAppearanceState option }

        static member tryCompose this =
            monad {
                let! name = this.Name |> Option.toResultWith "Name is not set."
                let! style = this.Style |> Option.toResultWith "Style is not set."

                let! font =
                    this.Font
                    |> Option.toResultWith "Font is not set."
                    |> Result.bind FontState.tryComposeUpdate

                let! appearance =
                    this.Appearance
                    |> Option.toResultWith "Appearance is not set."
                    |> Result.bind AddSpeakerAppearanceState.tryCompose

                return
                    { Frame.SpeakerState.Name = name
                      Frame.SpeakerState.Style = style
                      Frame.SpeakerState.Font = font
                      Frame.SpeakerState.Appearance = appearance }
            }

    let private runAddSpeaker
        (movie: Frame.MovieBuilder)
        (args: Value array)
        (block: Statement list option)
        (env: EvalEnv, state: Frame.MovieState)
        : Result<EvalEnv * Frame.MovieState, string> =
        let initialState =
            { Name = None
              Style = None
              Font = None
              Appearance = None }

        let runAddSpeakerStatement (acc: Result<EvalEnv * AddSpeakerState, string>) (statement: Statement) =
            monad {
                let! env, config = acc

                match statement with
                | Gets(targetExpr, contentExpr) ->
                    let! target = targetExpr |> tryEval env |> Result.bind Value.tryAsString

                    match target with
                    | "name" ->
                        let! name = contentExpr |> tryEval env |> Result.bind Value.tryAsString

                        let config' = { config with Name = Some name }

                        return env, config'
                    | "style" ->
                        let! style = contentExpr |> tryEval env |> Result.bind Value.tryAsString

                        let config' = { config with Style = Some style }

                        return env, config'
                    | _ -> return! Error "Invalid assignment."
                | Do(expr, optBlock) ->
                    let! fieldName = expr |> tryEval env |> Result.bind Value.tryAsString

                    match fieldName with
                    | "font" ->
                        let! env', font' = runFont optBlock (env, config.Font)
                        return env', { config with Font = font' }
                    | "appearance" ->
                        let! env', app' = runAddSpeakerAppearance optBlock (env, config.Appearance)
                        return env', { config with Appearance = app' }
                    | _ -> return! Error $"Unknown field name `{fieldName}` in add-speaker."
                | _ -> return! Error "Invalid statement."
            }

        let buildConfig speakerName (env: EvalEnv, addSpeaker: AddSpeakerState) =
            monad {
                let! config = addSpeaker |> AddSpeakerState.tryCompose
                let state' = movie.AddSpeaker(state, speakerName, config)
                return env, state'
            }

        monad {
            let! speakerName =
                args
                |> ArrayExt.tryAsSingleton "Expected one argument."
                |> Result.bind Value.tryAsString

            let! block' = block |> Option.toResultWith "Expected block."
            let! config = (Ok(env, initialState), block') ||> Seq.fold runAddSpeakerStatement
            return! buildConfig speakerName config
        }

    //

    let runAppearance
        (movie: Frame.MovieBuilder)
        (args: Value array)
        (block: Statement list option)
        (env: EvalEnv, state: Frame.MovieState)
        : Result<EvalEnv * Frame.MovieState, string> =
        let operators =
            [ InnerOperator.on,
              fun (args: Value array) (block: Statement list option) ->
                  monad {
                      let! layerPath =
                          args
                          |> ArrayExt.tryAsSingleton "Expected one argument."
                          |> Result.bind Value.tryAsString

                      if block <> None then
                          do! Error "Expected no block."

                      let segments = layerPath |> String.split [ "/" ]
                      let f' = Frame.SpeakerState.turnOn segments
                      return env, f'
                  }
              InnerOperator.hflip,
              fun (args: Value array) (block: Statement list option) ->
                  monad {
                      do! args |> ArrayExt.tryAsEmpty "Expected no arguments."

                      if block <> None then
                          do! Error "Expected no block."

                      let f' = Frame.SpeakerState.hFlip
                      return env, f'
                  } ]
            |> Map.ofSeq

        let runAppearanceStatement
            (acc: Result<EvalEnv * (Frame.SpeakerState -> Frame.SpeakerState), string>)
            (statement: Statement)
            : Result<EvalEnv * (Frame.SpeakerState -> Frame.SpeakerState), string> =
            match statement with
            | Do(expr, block) ->
                monad {
                    let! env, f = acc

                    if block <> None then
                        do! Error "Expected no block."

                    let! s, args = expr |> tryEval env |> Result.bind Value.tryAsInnerOperatorApplied

                    let! op = operators.TryFind s |> Option.toResultWith "Unknown inner operator."
                    let! env', f' = op args block
                    return env', f >> f'
                }
            | _ -> Error "Invalid statement."

        let init = Ok(env, id)

        monad {
            do! args |> ArrayExt.tryAsEmpty "Expected no arguments."

            let! block' = block |> Option.toResultWith "Expected block."

            return block'
        }
        |> Result.bind (Seq.fold runAppearanceStatement init)
        |> Result.map (fun (env, modify) ->
            let state' = movie.ModifySpeaker(state, modify)
            env, state')

    let runSetStyle
        (movie: Frame.MovieBuilder)
        (args: Value array)
        (block: Statement list option)
        (env: EvalEnv, state: Frame.MovieState)
        : Result<EvalEnv * Frame.MovieState, string> =
        monad {
            let! style =
                args
                |> ArrayExt.tryAsSingleton "Expected no arguments."
                |> Result.bind Value.tryAsString

            if block.IsSome then
                do! Error "Expected no block."

            let state' = movie.SetStyle(state, style)

            return env, state'
        }

    type private AddImageState =
        { Path: string option
          Pos: PosState option
          Resize: ResizeState option
          BindsTo: string option }

    let runAddImage
        (movie: Frame.MovieBuilder)
        (args: Value array)
        (block: Statement list option)
        (env: EvalEnv, state: Frame.MovieState)
        : Result<EvalEnv * Frame.MovieState, string> =
        let runAddImageStatement (acc: Result<EvalEnv * AddImageState, string>) (statement: Statement) =
            monad {
                let! env, config = acc

                match statement with
                | Gets(targetExpr, contentExpr) ->
                    let! fieldName = targetExpr |> tryEval env |> Result.bind Value.tryAsString
                    let! content = contentExpr |> tryEval env

                    match fieldName with
                    | "path" ->
                        let! path = content |> Value.tryAsString

                        let config': AddImageState = { config with Path = Some path }

                        return env, config'
                    | _ -> return! Error $"Unknown field name `{fieldName}` in add-image."
                | Do(expr, optBlock) ->
                    let! fieldName = expr |> tryEval env |> Result.bind Value.tryAsString

                    match fieldName with
                    | "pos" ->
                        let! env', pos' = runPos optBlock (env, config.Pos)
                        let config' = { config with Pos = pos' }
                        return env', config'
                    | "resize" ->
                        let! env', resize' = runResize optBlock (env, config.Resize)
                        let config' = { config with Resize = resize' }
                        return env', config'
                    | _ -> return! Error $"Unknown field name `{fieldName}` in add-image."
                | BindsTo(targetExpr, pattern) ->
                    if targetExpr.IsSome then
                        do! Error "Expected no target."

                    let pattern' =
                        match pattern with
                        | Pattern.Variable name -> name

                    let config' = { config with BindsTo = Some pattern' }

                    return env, config'
                | _ -> return! Error "Invalid statement."
            }

        monad {
            do! args |> ArrayExt.tryAsEmpty "Expected no arguments."

            let! block' = block |> Option.toResultWith "Expected block."

            let initialState =
                { Path = None
                  Pos = None
                  Resize = None
                  BindsTo = None }

            let! env', config = (Ok(env, initialState), block') ||> Seq.fold runAddImageStatement

            let! path = config.Path |> Option.toResultWith "Path is not set."

            let! pos =
                config.Pos
                |> Option.toResultWith "Pos is not set."
                |> Result.bind PosState.tryCompose

            let resize = config.Resize

            let! resize' =
                resize
                |> function
                    | None -> Ok None
                    | Some resize -> resize.TryToResize

            let assetId = System.Guid.NewGuid().ToString("N")

            let state' = movie.AddImage(state, assetId, path, pos, resize')

            let env'' =
                match config.BindsTo with
                | None -> env'
                | Some varName -> env'.WithVariable(varName, Value.AssetRef { Type = AssetType.Image; Id = assetId })

            return env'', state'
        }

    type private AddVideoState =
        { Path: string option
          Pos: PosState option
          Resize: ResizeState option
          Trim: DurationState option
          BindsTo: string option }

    let runAddVideo
        (movie: Frame.MovieBuilder)
        (args: Value array)
        (block: Statement list option)
        (env: EvalEnv, state: Frame.MovieState)
        : Result<EvalEnv * Frame.MovieState, string> =
        let runAddVideoStatement (acc: Result<EvalEnv * AddVideoState, string>) (statement: Statement) =
            monad {
                let! env, config = acc

                match statement with
                | Gets(targetExpr, contentExpr) ->
                    let! fieldName = targetExpr |> tryEval env |> Result.bind Value.tryAsString
                    let! content = contentExpr |> tryEval env

                    match fieldName with
                    | "path" ->
                        let! path = content |> Value.tryAsString

                        let config': AddVideoState = { config with Path = Some path }

                        return env, config'
                    | _ -> return! Error $"Unknown field name `{fieldName}` in add-image."
                | Do(expr, optBlock) ->
                    let! fieldName = expr |> tryEval env |> Result.bind Value.tryAsString

                    match fieldName with
                    | "pos" ->
                        let! env', pos' = runPos optBlock (env, config.Pos)
                        let config' = { config with Pos = pos' }
                        return env', config'
                    | "resize" ->
                        let! env', resize' = runResize optBlock (env, config.Resize)
                        let config' = { config with Resize = resize' }
                        return env', config'
                    | "trim" ->
                        let! env', trim' = runDuration optBlock (env, config.Trim)
                        let config' = { config with Trim = trim' }
                        return env', config'
                    | _ -> return! Error $"Unknown field name `{fieldName}` in add-image."
                | BindsTo(targetExpr, pattern) ->
                    if targetExpr.IsSome then
                        do! Error "Expected no target."

                    let pattern' =
                        match pattern with
                        | Pattern.Variable name -> name

                    let config' = { config with BindsTo = Some pattern' }

                    return env, config'
                | _ -> return! Error "Invalid statement."
            }

        monad {
            do! args |> ArrayExt.tryAsEmpty "Expected no arguments."

            let! block' = block |> Option.toResultWith "Expected block."

            let initialState =
                { Path = None
                  Pos = None
                  Resize = None
                  Trim = None
                  BindsTo = None }

            let! env', config = (Ok(env, initialState), block') ||> Seq.fold runAddVideoStatement

            let! path = config.Path |> Option.toResultWith "Path is not set."

            let! pos =
                config.Pos
                |> Option.toResultWith "Pos is not set."
                |> Result.bind PosState.tryCompose

            let! resize =
                config.Resize
                |> function
                    | None -> Ok None
                    | Some resize -> resize.TryToResize

            let { Start = trimStart; End = trimEnd } =
                config.Trim |> Option.defaultValue { Start = None; End = None }

            let assetId = System.Guid.NewGuid().ToString("N")

            let state' = movie.AddVideo(state, assetId, path, pos, resize, trimStart, trimEnd)

            let env'' =
                match config.BindsTo with
                | None -> env'
                | Some varName -> env'.WithVariable(varName, Value.AssetRef { Type = AssetType.Video; Id = assetId })

            return env'', state'
        }

    let runSetPriority
        (_movie: Frame.MovieBuilder)
        (args: Value array)
        (block: Statement list option)
        (env: EvalEnv, state: Frame.MovieState)
        : Result<EvalEnv * Frame.MovieState, string> =
        let runSetPriorityStatement (acc: Result<EvalEnv, string>) (statement: Statement) =
            monad {
                let! env = acc

                match statement with
                | Do(expr, optBlock) ->
                    let! lower, upper = expr |> tryEval env |> Result.bind Value.tryAsPriorityRelation
                    return env.WithPriorityRelation(lower, upper)
                | _ -> return! Error "Invalid statement."
            }

        monad {
            do! args |> ArrayExt.tryAsEmpty "Expected no arguments."

            let! block' = block |> Option.toResultWith "Expected block."

            let! env' = (Ok env, block') ||> Seq.fold runSetPriorityStatement

            return env', state
        }

    type private AddAudioState =
        { Path: string option
          Trim: DurationState option
          BindsTo: string option }

    let runAddAudio
        (movie: Frame.MovieBuilder)
        (args: Value array)
        (block: Statement list option)
        (env: EvalEnv, state: Frame.MovieState)
        : Result<EvalEnv * Frame.MovieState, string> =
        let runAddAudioStatement (acc: Result<EvalEnv * AddAudioState, string>) (statement: Statement) =
            monad {
                let! env, config = acc

                match statement with
                | Gets(targetExpr, contentExpr) ->
                    let! fieldName = targetExpr |> tryEval env |> Result.bind Value.tryAsString
                    let! content = contentExpr |> tryEval env

                    match fieldName with
                    | "path" ->
                        let! path = content |> Value.tryAsString

                        let config': AddAudioState = { config with Path = Some path }

                        return env, config'
                    | _ -> return! Error $"Unknown field name `{fieldName}` in add-image."
                | Do(expr, optBlock) ->
                    let! fieldName = expr |> tryEval env |> Result.bind Value.tryAsString

                    match fieldName with
                    | "trim" ->
                        let! env', trim' = runDuration optBlock (env, config.Trim)
                        let config' = { config with Trim = trim' }
                        return env', config'
                    | _ -> return! Error $"Unknown field name `{fieldName}` in add-image."
                | BindsTo(targetExpr, pattern) ->
                    if targetExpr.IsSome then
                        do! Error "Expected no target."

                    let pattern' =
                        match pattern with
                        | Pattern.Variable name -> name

                    let config' = { config with BindsTo = Some pattern' }

                    return env, config'
                | _ -> return! Error "Invalid statement."
            }

        monad {
            do! args |> ArrayExt.tryAsEmpty "Expected no arguments."

            let! block' = block |> Option.toResultWith "Expected block."

            let initialState =
                { Path = None
                  Trim = None
                  BindsTo = None }

            let! env', config = (Ok(env, initialState), block') ||> Seq.fold runAddAudioStatement

            let! path = config.Path |> Option.toResultWith "Path is not set."

            let { Start = trimStart; End = trimEnd } =
                config.Trim |> Option.defaultValue { Start = None; End = None }

            let assetId = System.Guid.NewGuid().ToString("N")

            let state' = movie.AddAudio(state, assetId, path, trimStart, trimEnd)

            let env'' =
                match config.BindsTo with
                | None -> env'
                | Some varName -> env'.WithVariable(varName, Value.AssetRef { Type = AssetType.Audio; Id = assetId })

            return env'', state'
        }

    let runRemove
        (movie: Frame.MovieBuilder)
        (args: Value array)
        (block: Statement list option)
        (env: EvalEnv, state: Frame.MovieState)
        : Result<EvalEnv * Frame.MovieState, string> =
        monad {
            let! assetRef =
                args
                |> ArrayExt.tryAsSingleton "Expected one argument."
                |> Result.bind Value.tryAsAssetRef

            if block.IsSome then
                do! Error "Expected no block."

            let state' = movie.Remove(state, assetRef.Id)
            return env, state'
        }

    //

    let private statementOperators =
        [ InnerOperator.initialize, runInitialize
          InnerOperator.addSpeaker, runAddSpeaker
          InnerOperator.appearance, runAppearance
          InnerOperator.setStyle, runSetStyle
          InnerOperator.addImage, runAddImage
          InnerOperator.addAudio, runAddAudio
          InnerOperator.addVideo, runAddVideo
          InnerOperator.setPriority, runSetPriority
          InnerOperator.remove, runRemove ]
        |> Map.ofSeq

    //

    let run (movie: Frame.MovieBuilder) (ast: AST) (env: EvalEnv, state: Frame.MovieState) =
        let rec runStatement
            (acc: Result<EvalEnv * Frame.MovieState, string>)
            (statement: Statement)
            : Result<EvalEnv * Frame.MovieState, string> =
            monad {
                let! env, state = acc

                match statement with
                | Do(expr, block) ->
                    let! s, args = expr |> tryEval env |> Result.bind Value.tryAsInnerOperatorApplied
                    let! op = statementOperators.TryFind s |> Option.toResultWith "Unknown inner operator."
                    return! op movie args block (env, state)
                | At(expr, block) ->
                    let! name = expr |> tryEval env |> Result.bind Value.tryAsString

                    let state' = movie.SetSpeaker(state, name)

                    match block with
                    | None -> return env, state'
                    | Some block ->
                        // TODO: ローカルステートメントを記述出来るようにする．
                        return! (Ok(env, state'), block) ||> Seq.fold runStatement
                | Gets(_, _) -> return! Error "Assignment is not allowed at the top level."
                | BindsTo(_, _) -> return! Error "Binding is not allowed at the top level."
                | Talk talk ->
                    let speech' = talk.Speech |> String.replace "\n" "" |> String.replace " " ""
                    return env, movie.YieldFrame(state, talk.Subtitle, speech')
            }

        (Ok(env, state), ast.Statements) ||> Seq.fold runStatement

    let build (movie: Frame.MovieBuilder) (env: EvalEnv) (ast: AST) =
        let state = Frame.MovieState.empty
        run movie ast (env, state) |> Result.map snd
