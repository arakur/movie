namespace Script

open FSharpPlus
open FSharpPlus.Data

type RevList<'a> = { Contents: 'a list; Length: int }

module RevList =
    let empty<'a> = { Contents = []; Length = 0 }

    let add<'a> v (this: RevList<'a>) =
        { Contents = v :: this.Contents
          Length = this.Length + 1 }

    let toList<'a> (this: RevList<'a>) = this.Contents |> List.rev

    let toArray<'a> (this: RevList<'a>) = this |> toList |> Array.ofList

module private ResultExt =
    let orElse<'a> (secondThunk: unit -> Result<'a, string>) (first: Result<'a, string>) =
        first
        |> Result.bindError (fun error -> secondThunk () |> Result.mapError (sprintf "%s && %s" error))

    let sequence<'a, 'b> (xs: Result<'a, 'b> seq) : Result<'a seq, 'b> =
        (Ok [], xs)
        ||> Seq.fold (fun acc x ->
            monad {
                let! acc = acc
                let! x = x
                return! Ok(acc |> List.cons x)
            })
        |>> Seq.rev

module private ArrayExt =
    let tryAsEmpty<'a, 'e> (message: 'e) (xs: 'a array) : Result<unit, 'e> =
        match xs with
        | [||] -> Ok()
        | _ -> Error message

    let tryAsSingleton<'a, 'e> (message: 'e) (xs: 'a array) : Result<'a, 'e> =
        match xs with
        | [| x |] -> Ok x
        | _ -> Error message

    let tryAsTuple2<'a, 'e> (message: 'e) (xs: 'a array) : Result<'a * 'a, 'e> =
        match xs with
        | [| x0; x1 |] -> Ok(x0, x1)
        | _ -> Error message

    let tryAsTuple3<'a, 'e> (message: 'e) (xs: 'a array) : Result<'a * 'a * 'a, 'e> =
        match xs with
        | [| x0; x1; x2 |] -> Ok(x0, x1, x2)
        | _ -> Error message

//

type Measure = string

[<RequireQualifiedAccess>]
type Numeral =
    | Int of int * measure: Measure option
    | Float of float * measure: Measure option

    member this.Measure =
        match this with
        | Int(_, m)
        | Float(_, m) -> m

    static member tryAsInt this =
        match this with
        | Int(i, None) -> Ok i
        | _ -> Error "Invalid type; expected int."

    static member tryAsFloat this =
        match this with
        | Int(i, None) -> Ok(float i)
        | Float(f, None) -> Ok f
        | _ -> Error "Invalid type; expected float."

    static member tryAsFloatMeas measure this =
        match this with
        | Int(i, Some m) when m = measure -> Ok(float i)
        | Float(f, Some m) when m = measure -> Ok f
        | _ -> Error(sprintf "Invalid type; expected float with %s." measure)

    static member tryAdd lhs rhs =
        match lhs, rhs with
        | Int(i0, m0), Int(i1, m1) when m0 = m1 -> Ok(Int(i0 + i1, m0))
        | Float(f0, m0), Float(f1, m1) when m0 = m1 -> Ok(Float(f0 + f1, m0))
        | Int(i0, m0), Float(f1, m1) when m0 = m1 -> Ok(Float(float i0 + f1, m0))
        | Float(f0, m0), Int(i1, m1) when m0 = m1 -> Ok(Float(f0 + float i1, m0))
        | _ -> Error "Invalid type; expected same measure."

    static member trySub lhs rhs =
        match lhs, rhs with
        | Int(i0, m0), Int(i1, m1) when m0 = m1 -> Ok(Int(i0 - i1, m0))
        | Float(f0, m0), Float(f1, m1) when m0 = m1 -> Ok(Float(f0 - f1, m0))
        | Int(i0, m0), Float(f1, m1) when m0 = m1 -> Ok(Float(float i0 - f1, m0))
        | Float(f0, m0), Int(i1, m1) when m0 = m1 -> Ok(Float(f0 - float i1, m0))
        | _ -> Error "Invalid type; expected same measure."

[<RequireQualifiedAccess>]
type Value =
    | Numeral of Numeral
    | String of string
    | BinaryOperator of string
    | BinaryOperatorLeftApplied of string * Value
    | Tuple of Value array
    | InnerOperator of string
    | InnerOperatorPartiallyApplied of string * arity: int * Value RevList
    | InnerOperatorApplied of string * Value array

    static member tryAsNumeral this =
        match this with
        | Numeral num -> Ok num
        | _ -> Error "Invalid type; expected numeral."

    static member tryAsInt this =
        this |> Value.tryAsNumeral >>= Numeral.tryAsInt

    static member tryAsFloat this =
        this |> Value.tryAsNumeral >>= Numeral.tryAsFloat

    static member tryAsFloatMeas measure this =
        this |> Value.tryAsNumeral >>= Numeral.tryAsFloatMeas measure

    static member tryAsString this =
        match this with
        | String str -> Ok str
        | _ -> Error "Invalid type; expected string."

    static member tryAsTuple this =
        match this with
        | Tuple tup -> Ok tup
        | _ -> Error "Invalid type; expected tuple."

    static member tryAsTupleWithSize size this =
        match this with
        | Tuple tup when tup.Length = size -> Ok tup
        | _ -> Error "Invalid type; expected tuple."

    static member tryAsInnerOperatorApplied this =
        match this with
        | InnerOperatorApplied(inner, args) -> Ok(inner, args)
        | _ -> Error "Invalid type; expected inner operator with full arguments."

    static member tryAdd lhs rhs =
        match lhs, rhs with
        | Numeral lhs, Numeral rhs -> Numeral.tryAdd lhs rhs |>> Value.Numeral
        | _ -> Error "Invalid type; expected numeral."

    static member trySub lhs rhs =
        match lhs, rhs with
        | Numeral lhs, Numeral rhs -> Numeral.trySub lhs rhs |>> Value.Numeral
        | _ -> Error "Invalid type; expected numeral."

module private InnerOperator =
    [<Literal>]
    let initialize = "initialize"

    [<Literal>]
    let addSpeaker = "add-speaker"

    [<Literal>]
    let appearance = "appearance"


    [<Literal>]
    let setStyle = "set-style"

    [<Literal>]
    let on = "on"

    [<Literal>]
    let hflip = "hflip"

    let innerOperators =
        [| initialize, 0; addSpeaker, 1; appearance, 0; setStyle, 1; on, 1; hflip, 0 |]

module private BinaryOperators =
    [<Literal>]
    let add = "+"

    [<Literal>]
    let sub = "-"

    let binaryOperators = [| add, Value.tryAdd; sub, Value.trySub |]

type IEvalEnv =
    abstract member TryVariable: string -> Value option
    abstract member TryArityInnerOperator: string -> int option
    abstract member TryBinaryOperator: string -> (Value -> Value -> Result<Value, string>) option

type EvalEnv
    (
        variables: Map<string, Value>,
        innerOperators: Map<string, int>,
        binaryOperators: Map<string, Value -> Value -> Result<Value, string>>
    ) =
    static member prelude() : EvalEnv =
        let innerOperators = InnerOperator.innerOperators |> Map.ofSeq

        let variables =
            innerOperators
            |> Map.toSeq
            |> Seq.map (fun (name, arity) ->
                if arity = 0 then
                    name, Value.InnerOperatorApplied(name, [||])
                else
                    name, Value.InnerOperator name)
            |> Map.ofSeq

        let binaryOperators = BinaryOperators.binaryOperators |> Map.ofSeq

        EvalEnv(variables, innerOperators, binaryOperators)

    member val Variables = variables
    member val InnerOperators = innerOperators
    member val BinaryOperators = binaryOperators

    member this.WithVariable(var: string, value: Value) =
        EvalEnv(this.Variables.Add(var, value), this.InnerOperators, this.BinaryOperators)

    member this.WithInnerOperator(inner: string, arity: int) =
        EvalEnv(this.Variables, this.InnerOperators.Add(inner, arity), this.BinaryOperators)

    member this.WithBinaryOperator(bin: string, f: Value -> Value -> Result<Value, string>) =
        EvalEnv(this.Variables, this.InnerOperators, this.BinaryOperators.Add(bin, f))

    member this.WithInnerOperatorSynonym(var: string, inner: string) =
        let arity = this.InnerOperators.TryFind inner

        match arity with
        | None -> failwithf "An inner operator `%s` not found." inner
        | Some 0 -> this.WithVariable(var, Value.InnerOperatorApplied(inner, [||]))
        | Some _ -> this.WithVariable(var, Value.InnerOperator inner)

    interface IEvalEnv with
        member this.TryVariable v = this.Variables.TryFind v
        member this.TryArityInnerOperator op = this.InnerOperators.TryFind op
        member this.TryBinaryOperator op = this.BinaryOperators.TryFind op

module Value =
    let tryApply (env: IEvalEnv) (arg: Value) (f: Value) =
        match f with
        | Value.Numeral _
        | Value.String _
        | Value.Tuple _ -> Error "Cannot apply."
        | Value.BinaryOperator op -> Ok <| Value.BinaryOperatorLeftApplied(op, arg)
        | Value.BinaryOperatorLeftApplied(op, lhs) ->
            monad {
                let! f =
                    env.TryBinaryOperator op
                    |> Option.toResultWith (sprintf "Binary operator '%s' not found." op)

                return! f lhs arg
            }
        | Value.InnerOperatorApplied _ -> Error "Too many arguments."
        | Value.InnerOperator op ->
            monad {
                let! arity =
                    env.TryArityInnerOperator op
                    |> Option.toResultWith (sprintf "Inner operator '%s' not found." op)

                match arity with
                | 0 -> return! Error "Cannot apply."
                | 1 -> return Value.InnerOperatorApplied(op, [| arg |])
                | _ -> return Value.InnerOperatorPartiallyApplied(op, arity, RevList.empty |> RevList.add arg)
            }
        | Value.InnerOperatorPartiallyApplied(op, arity, args) ->
            if args.Length + 1 = arity then
                Ok(Value.InnerOperatorApplied(op, args |> RevList.add arg |> RevList.toArray))
            else
                Ok(Value.InnerOperatorPartiallyApplied(op, arity, args |> RevList.add arg))

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
            exprs |> List.map (tryEval env) |> ResultExt.sequence
            |>> Seq.toArray
            |>> Value.Tuple

    //

    type private FontState =
        { Color: Types.Color option
          Family: string option
          Size: float<Measure.pt> option
          Weight: Typst.Weight option }

    let private runFont (block: Statement list option) (env: IEvalEnv, fontState: FontState option) =
        block
        |> Option.toResultWith "Expected block."
        >>= Seq.fold
            (fun acc statement ->
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
                                  Family = None }

                        let! fieldName = target |> Value.tryAsString

                        match fieldName with
                        | "color" ->
                            let! r, g, b =
                                content |> Value.tryAsTuple >>= (Seq.map Value.tryAsInt >> ResultExt.sequence)
                                |>> Array.ofSeq
                                >>= ArrayExt.tryAsTuple3 "Expected a tuple with 3 elements."

                            let font'' =
                                { font' with
                                    Color = Some(Types.RGB(r, g, b)) }

                            return env, Some font''
                        | "size" ->
                            let! size = content |> Value.tryAsFloatMeas "pt"

                            let font'' =
                                { font' with
                                    Size = Some(size * 1.<Measure.pt>) }

                            return env, Some font''
                        | "weight" ->
                            let tryNamedWeight () =
                                monad {
                                    let! name = content |> Value.tryAsString

                                    let! weight =
                                        name |> Typst.Weight.tryFrom |> Option.toResultWith "Invalid weight name."

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
                            let font'' = { font' with Family = Some family }
                            return env, Some font''
                        | _ -> return! Error $"Unknown field name `{fieldName}` in font."
                    | _ -> return! Error "Invalid statement."
                })
            (Ok(env, fontState))

    //

    type private PosState =
        { X: int<Measure.px> option
          Y: int<Measure.px> option }

    let private runPos (block: Statement list option) (env: IEvalEnv, posState: PosState option) =
        block
        |> Option.toResultWith "Expected block."
        >>= Seq.fold
            (fun acc statement ->
                monad {
                    let! env, pos = acc

                    match statement with
                    | Gets(targetExpr, contentExpr) ->
                        let! fieldName = targetExpr |> tryEval env >>= Value.tryAsString

                        let pos' = pos |> Option.defaultValue { X = None; Y = None }

                        match fieldName with
                        | "x" ->
                            let! x = contentExpr |> tryEval env >>= Value.tryAsFloatMeas "px"

                            let pos'' =
                                { pos' with
                                    X = Some(int x * 1<Measure.px>) }

                            return env, Some pos''
                        | "y" ->
                            let! y = contentExpr |> tryEval env >>= Value.tryAsFloatMeas "px"

                            let pos'' =
                                { pos' with
                                    Y = Some(int y * 1<Measure.px>) }

                            return env, Some pos''
                        | _ -> return! Error $"Unknown field name `{fieldName}` in pos."
                    | _ -> return! Error "Invalid statement."
                })
            (Ok(env, posState))

    type private SizeState =
        { Width: int<Measure.px> option
          Height: int<Measure.px> option }

    let private runSize (block: Statement list option) (env: IEvalEnv, sizeState: SizeState option) =
        block
        |> Option.toResultWith "Expected block."
        >>= (fun block' ->
            (Ok(env, sizeState), block')
            ||> Seq.fold (fun acc statement ->
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
                            let! width = content |> Value.tryAsFloatMeas "px"

                            let size'' =
                                { size' with
                                    Width = Some(int width * 1<Measure.px>) }

                            return env, Some size''
                        | "height" ->
                            let! height = content |> Value.tryAsFloatMeas "px"

                            let size'' =
                                { size' with
                                    Height = Some(int height * 1<Measure.px>) }

                            return env, Some size''
                        | _ -> return! Error $"Unknown field name `{fieldName}` in size."
                    | _ -> return! Error "Invalid statement."
                }))

    type private ResizeState =
        { Width: int<Measure.px> option
          Height: int<Measure.px> option
          Scale: float option
          ScaleX: float option
          ScaleY: float option }

    let private runResize (block: Statement list option) (env: IEvalEnv, resizeState: ResizeState option) =
        block
        |> Option.toResultWith "Expected block."
        >>= (fun block' ->
            (Ok(env, resizeState), block')
            ||> Seq.fold (fun acc statement ->
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
                            let! width = content |> Value.tryAsFloatMeas "px"

                            let resize'' =
                                { resize' with
                                    Width = Some(int width * 1<Measure.px>) }

                            return env, Some resize''
                        | "height" ->
                            let! height = content |> Value.tryAsFloatMeas "px"

                            let resize'' =
                                { resize' with
                                    Height = Some(int height * 1<Measure.px>) }

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
                }))

    type private InitializeState =
        { Font: FontState option
          Pos: PosState option
          Size: SizeState option
          Background: Frame.Background option }

    let runInitialize
        (movie: Frame.MovieBuilder)
        (args: Value array)
        (block: Statement list option)
        (env: IEvalEnv, _state: Frame.MovieState)
        : Result<IEvalEnv * Frame.MovieState, string> =
        let initialState =
            { Font = None
              Pos = None
              Size = None
              Background = None }

        let runInitializeStatement (acc: Result<IEvalEnv * InitializeState, string>) (statement: Statement) =
            monad {
                let! env, config = acc

                match statement with
                | Gets(targetExpr, contentExpr) ->
                    let! target = targetExpr |> tryEval env >>= Value.tryAsString

                    match target with
                    | "background" ->
                        // TODO: 色指定にも対応する．
                        let! backgroundTypeValue, backgroundContentValue =
                            contentExpr
                            |> tryEval env
                            >>= Value.tryAsTuple
                            >>= ArrayExt.tryAsTuple2 "Expected a tuple with 2 elements."

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
                                >>= (Seq.map Value.tryAsInt >> ResultExt.sequence)
                                |>> Array.ofSeq
                                >>= ArrayExt.tryAsTuple3 "Expected a tuple with 3 elements."

                            let background = Frame.Background.RGB(r, g, b)

                            let config' =
                                { config with
                                    Background = Some background }

                            return env, config'
                        | _ -> return! Error "Unknown background type."
                    | _ -> return! Error "Invalid assignment."
                | Do(expr, optBlock) ->
                    let! fieldName = expr |> tryEval env >>= Value.tryAsString

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

        let buildConfig (env: IEvalEnv, config: InitializeState) =
            monad {
                let! font = config.Font |> Option.toResultWith "Font is not set."
                let! pos = config.Pos |> Option.toResultWith "Pos is not set."
                let! size = config.Size |> Option.toResultWith "Size is not set."
                let! background = config.Background |> Option.toResultWith "Background is not set."

                let! fontColor = font.Color |> Option.toResultWith "Font color is not set."
                let! fontSize = font.Size |> Option.toResultWith "Font size is not set."
                let! fontWeight = font.Weight |> Option.toResultWith "Font weight is not set."
                let! fontFamily = font.Family |> Option.toResultWith "Font family is not set."

                let! posX = pos.X |> Option.toResultWith "Pos X is not set."
                let! posY = pos.Y |> Option.toResultWith "Pos Y is not set."

                let! sizeWidth = size.Width |> Option.toResultWith "Size width is not set."
                let! sizeHeight = size.Height |> Option.toResultWith "Size height is not set."

                let config' =
                    { Frame.Initialize.Font =
                        { Color = fontColor
                          Size = fontSize
                          Weight = fontWeight
                          Family = fontFamily }
                      Frame.Initialize.Pos = { X = posX; Y = posY }
                      Frame.Initialize.Size =
                        { Width = sizeWidth
                          Height = sizeHeight }
                      Frame.Initialize.Background = background }

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

    let private runAddSpeakerAppearance
        (block: Statement list option)
        (env: IEvalEnv, app: AddSpeakerAppearanceState option)
        =
        block
        |> Option.toResultWith "Expected block."
        >>= Seq.fold
            (fun acc statement ->
                monad {
                    let! env, app = acc

                    let app' =
                        app
                        |> Option.defaultValue
                            { Path = None
                              Pos = None
                              Resize = None }

                    match statement with
                    | Do(expr, optBlock) ->
                        let! fieldName = expr |> tryEval env >>= Value.tryAsString

                        match fieldName with
                        | "pos" ->
                            let! env', pos' = runPos optBlock (env, app'.Pos)
                            let app'' = { app' with Pos = pos' }
                            return env', Some app''
                        | "resize" ->
                            let! env', resize' = runResize optBlock (env, app'.Resize)
                            let app'' = { app' with Resize = resize' }
                            return env', Some app''
                        | _ -> return! Error $"Unknown field name `{fieldName}` in appearance."
                    | Gets(targetExpr, contentExpr) ->
                        let! fieldName = targetExpr |> tryEval env >>= Value.tryAsString
                        let! content = contentExpr |> tryEval env

                        match fieldName with
                        | "path" ->
                            let! path = content |> Value.tryAsString

                            let app'' = { app' with Path = Some path }

                            return env, Some app''
                        | _ -> return! Error $"Unknown field name `{fieldName}` in appearance."
                    | _ -> return! Error "Invalid statement."
                })
            (Ok(env, app))

    type private AddSpeakerState =
        { Name: string option
          Style: string option
          Font: FontState option
          Appearance: AddSpeakerAppearanceState option }

    let private runAddSpeaker
        (movie: Frame.MovieBuilder)
        (args: Value array)
        (block: Statement list option)
        (env: IEvalEnv, state: Frame.MovieState)
        : Result<IEvalEnv * Frame.MovieState, string> =
        let initialState =
            { Name = None
              Style = None
              Font = None
              Appearance = None }

        let runAddSpeakerStatement (acc: Result<IEvalEnv * AddSpeakerState, string>) (statement: Statement) =
            monad {
                let! env, config = acc

                match statement with
                | Gets(targetExpr, contentExpr) ->
                    let! target = targetExpr |> tryEval env >>= Value.tryAsString

                    match target with
                    | "name" ->
                        let! name = contentExpr |> tryEval env >>= Value.tryAsString

                        let config' = { config with Name = Some name }

                        return env, config'
                    | "style" ->
                        let! style = contentExpr |> tryEval env >>= Value.tryAsString

                        let config' = { config with Style = Some style }

                        return env, config'
                    | _ -> return! Error "Invalid assignment."
                | Do(expr, optBlock) ->
                    let! fieldName = expr |> tryEval env >>= Value.tryAsString

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

        let buildConfig speakerName (env: IEvalEnv, config: AddSpeakerState) =
            monad {
                let! name = config.Name |> Option.toResultWith "Name is not set."
                let! style = config.Style |> Option.toResultWith "Style is not set."
                let! font = config.Font |> Option.toResultWith "Font is not set."
                let! appearance = config.Appearance |> Option.toResultWith "Appearance is not set."

                let fontColor = font.Color
                let fontSize = font.Size
                let fontWeight = font.Weight
                let fontFamily = font.Family

                let! appearancePath = appearance.Path |> Option.toResultWith "Path is not set."

                let! appearancePos = appearance.Pos |> Option.toResultWith "Pos is not set."

                let! posX = appearancePos.X |> Option.toResultWith "Pos X is not set."
                let! posY = appearancePos.Y |> Option.toResultWith "Pos Y is not set."

                let appearanceResize = appearance.Resize

                let! resize' =
                    appearanceResize
                    |> function
                        | None -> Ok None
                        | Some resize ->
                            match resize with
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

                let config' =
                    { Frame.SpeakerState.Name = name
                      Frame.SpeakerState.Style = style
                      Frame.SpeakerState.Font =
                        Frame.SpeakerFont(
                            ?color = fontColor,
                            ?size = fontSize,
                            ?weight = fontWeight,
                            ?family = fontFamily
                        )
                      Frame.SpeakerState.Appearance =
                        { Appearance = Appearance.Appearance.LoadDirectory appearancePath
                          Pos = { X = posX; Y = posY }
                          Resize = resize' } }

                let state' = movie.AddSpeaker(state, speakerName, config')
                return env, state'
            }

        monad {
            let! speakerName = args |> ArrayExt.tryAsSingleton "Expected one argument." >>= Value.tryAsString
            let! block' = block |> Option.toResultWith "Expected block."
            let! config = (Ok(env, initialState), block') ||> Seq.fold runAddSpeakerStatement
            return! buildConfig speakerName config
        }

    //

    let runAppearance
        (movie: Frame.MovieBuilder)
        (args: Value array)
        (block: Statement list option)
        (env: IEvalEnv, state: Frame.MovieState)
        : Result<IEvalEnv * Frame.MovieState, string> =
        let operators =
            [ InnerOperator.on,
              fun (args: Value array) (block: Statement list option) ->
                  monad {
                      let! layerPath = args |> ArrayExt.tryAsSingleton "Expected one argument." >>= Value.tryAsString

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

        monad {
            do! args |> ArrayExt.tryAsEmpty "Expected no arguments."

            let! block' = block |> Option.toResultWith "Expected block."

            return!
                (Ok(env, id), block')
                ||> Seq.fold (fun acc statement ->
                    match statement with
                    | Do(expr, block) ->
                        monad {
                            let! env, f = acc

                            if block <> None then
                                do! Error "Expected no block."

                            let! s, args = expr |> tryEval env >>= Value.tryAsInnerOperatorApplied

                            let! op = operators.TryFind s |> Option.toResultWith "Unknown inner operator."
                            let! env', f' = op args block
                            return env', f >> f'
                        }
                    | _ -> Error "Invalid statement.")
                |>> (fun (env, modify) ->
                    let state' = movie.ModifySpeaker(state, modify)
                    env, state')
        }

    let runSetStyle
        (movie: Frame.MovieBuilder)
        (args: Value array)
        (block: Statement list option)
        (env: IEvalEnv, state: Frame.MovieState)
        : Result<IEvalEnv * Frame.MovieState, string> =
        monad {
            let! style = args |> ArrayExt.tryAsSingleton "Expected no arguments." >>= Value.tryAsString

            if block.IsSome then
                do! Error "Expected no block."

            let state' = movie.SetStyle(state, style)

            return env, state'
        }

    //

    let private statementOperators =
        [ InnerOperator.initialize, runInitialize
          InnerOperator.addSpeaker, runAddSpeaker
          InnerOperator.appearance, runAppearance
          InnerOperator.setStyle, runSetStyle ]
        |> Map.ofSeq

    let rec runStatement
        (movie: Frame.MovieBuilder)
        (statement: Statement)
        (env: IEvalEnv, state: Frame.MovieState)
        : Result<IEvalEnv * Frame.MovieState, string> =

        match statement with
        | Do(expr, block) ->
            monad {
                let! s, args = expr |> tryEval env >>= Value.tryAsInnerOperatorApplied
                let! op = statementOperators.TryFind s |> Option.toResultWith "Unknown inner operator."
                return! op movie args block (env, state)
            }
        | At(expr, block) ->
            monad {
                let! name = expr |> tryEval env >>= Value.tryAsString

                let state' = movie.SetSpeaker(state, name)

                match block with
                | None -> return (env, state')
                | Some block ->
                    // TODO: ローカルステートメントを記述出来るようにする．
                    return!
                        (Ok(env, state'), block)
                        ||> Seq.fold (fun acc statement -> acc >>= runStatement movie statement)
            }
        | Gets(_, _) -> Error "Assignment is not allowed at the top level."
        | Talk talk ->
            let speech' = talk.Speech |> String.replace "\n" "" |> String.replace " " ""
            Ok(env, movie.YieldFrame(state, talk.Subtitle, speech'))

    //

    let run (movie: Frame.MovieBuilder) (ast: AST) (env: IEvalEnv, state: Frame.MovieState) =
        (Ok(env, state), ast.Statements)
        ||> Seq.fold (fun acc statement -> acc >>= runStatement movie statement)

    let build (movie: Frame.MovieBuilder) (env: IEvalEnv) (ast: AST) =
        let state = Frame.MovieState.empty
        run movie ast (env, state) |> Result.map snd
