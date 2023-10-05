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
    let sequence<'a, 'b> (xs: Result<'a, 'b> seq) : Result<'a seq, 'b> =
        (Ok [], xs)
        ||> Seq.fold (fun acc x ->
            monad {
                let! acc = acc
                let! x = x
                return! Ok(acc |> List.cons x)
            })
        |>> Seq.rev

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

type BinaryOperator = string

type InnerOperator = string

[<RequireQualifiedAccess>]
type Value =
    | Numeral of Numeral
    | String of string
    | BinaryOperator of BinaryOperator
    | BinaryOperatorLeftApplied of BinaryOperator * Value
    | Tuple of Value array
    | InnerOperator of InnerOperator
    | InnerOperatorPartiallyApplied of InnerOperator * arity: int * Value RevList
    | InnerOperatorApplied of InnerOperator * Value array

type IEvalEnv =
    abstract member TryVariable: string -> Value option
    abstract member TryArityInnerOperator: InnerOperator -> int option
    abstract member TryBinaryOperator: BinaryOperator -> (Numeral -> Numeral -> Result<Numeral, string>) option

type EvalEnv
    (
        variables: Map<string, Value>,
        innerOperators: Map<InnerOperator, int>,
        binaryOperators: Map<BinaryOperator, Numeral -> Numeral -> Result<Numeral, string>>
    ) =
    new() = EvalEnv(Map.empty, Map.empty, Map.empty)

    member val Variables = variables
    member val InnerOperators = innerOperators
    member val BinaryOperators = binaryOperators

    member this.WithVariable(var: string, value: Value) =
        EvalEnv(this.Variables.Add(var, value), this.InnerOperators, this.BinaryOperators)

    member this.WithInnerOperator(inner: InnerOperator, arity: int) =
        EvalEnv(this.Variables, this.InnerOperators.Add(inner, arity), this.BinaryOperators)

    member this.WithBinaryOperator(bin: BinaryOperator, f: Numeral -> Numeral -> Result<Numeral, string>) =
        EvalEnv(this.Variables, this.InnerOperators, this.BinaryOperators.Add(bin, f))

    member this.WithInnerOperatorSynonym(var: string, inner: InnerOperator) =
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
        | Value.BinaryOperatorLeftApplied(op, arg0) ->
            monad {
                let! f =
                    env.TryBinaryOperator op
                    |> Option.toResultWith (sprintf "Binary operator '%s' not found." op)

                match arg0, arg with
                | Value.Numeral n0, Value.Numeral n1 -> return! f n0 n1 |>> Value.Numeral
                | _ -> return! Error "Invalid arguments."
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
    let rec tryEval (env: IEvalEnv) (expr: Parser.Expr) : Result<Value, string> =
        match expr with
        | Parser.Expr.Numeral(n, m) ->
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
        | Parser.Expr.String s -> Ok(Value.String s)
        | Parser.Expr.Variable v -> env.TryVariable v |> Option.toResultWith (sprintf "Variable '%s' not found." v)
        | Parser.Expr.App(f, args) ->
            (f |> tryEval env, args)
            ||> Seq.fold (fun acc arg ->
                monad {
                    let! acc = acc
                    let! arg = arg |> tryEval env
                    return! Value.tryApply env arg acc
                })
        | Parser.Expr.BinaryOperator(op, expr0, expr1) ->
            monad {
                let! v0 = expr0 |> tryEval env
                let! v1 = expr1 |> tryEval env

                let! f =
                    env.TryBinaryOperator op.Name
                    |> Option.toResultWith (sprintf "Binary operator '%s' not found." op.Name)

                match v0, v1 with
                | Value.Numeral n0, Value.Numeral n1 -> return! f n0 n1 |>> Value.Numeral
                | _ -> return! Error "Invalid arguments."
            }
        | Parser.Expr.Tuple exprs ->
            exprs |> List.map (tryEval env) |> ResultExt.sequence
            |>> Seq.toArray
            |>> Value.Tuple


    let rec runStatement
        (movie: Frame.MovieBuilder)
        (env: IEvalEnv, state: Frame.MovieState)
        (statement: Parser.Statement)
        : Result<IEvalEnv * Frame.MovieState, string> =
        match statement with
        | Parser.Statement.Do(expr, block) ->
            monad {
                let! v = expr |> tryEval env

                match v with
                | Value.InnerOperatorApplied(s, args) ->
                    match s with
                    | "initialize" ->
                        match args, block with
                        | [||], Some block -> return failwith "TODO"
                        | _ -> return! Error "Invalid argument types."
                    | "add-speaker" ->
                        match args, block with
                        | [| Value.String name |], Some block -> return failwith "TODO"
                        | _ -> return! Error "Invalid argument types."
                    | "appearance" ->
                        match args, block with
                        | [||], Some block ->
                            return!
                                (Ok(env, id), block)
                                ||> Seq.fold (fun acc statement ->
                                    monad {
                                        let! env, f = acc

                                        match statement with
                                        | Parser.Statement.Do(expr, block) ->
                                            let! v = expr |> tryEval env

                                            match v with
                                            | Value.InnerOperatorApplied(s, args) ->
                                                match s with
                                                | "on" ->
                                                    match args, block with
                                                    | [| Value.String layerPath |], None ->
                                                        let segments = layerPath |> String.split [ "/" ]

                                                        let f' = f >> Frame.SpeakerState.turnOn segments

                                                        return env, f'
                                                    | _ -> return! Error "Invalid argument types."
                                                | _ -> return! Error "Unknown inner operator."
                                            | _ ->
                                                return!
                                                    Error
                                                        "Invalid expression as a statement; must be an inner operator."
                                        | _ -> return failwith "TODO"
                                    })
                                |>> (fun (env, modify) ->
                                    let state' = movie.ModifySpeaker(state, modify)
                                    env, state')
                        | _ -> return! Error "Invalid argument types."
                    | _ -> return! Error "Unknown inner operator."
                | _ -> return! Error "Invalid expression as a statement; must be an inner operator."
            }
        | Parser.Statement.At(expr, block) ->
            monad {
                let! v = expr |> tryEval env

                match v with
                | Value.String name ->
                    let state' = movie.SetSpeaker(state, name)

                    match block with
                    | None -> return (env, state')
                    | Some block ->
                        // TODO: ローカルステートメントを記述出来るようにする．
                        return!
                            (Ok(env, state'), block)
                            ||> Seq.fold (fun acc statement ->
                                monad {
                                    let! acc = acc
                                    return! runStatement movie acc statement
                                })
                | _ -> return! Error "Invalid speaker name; must be a string."
            }
        | Parser.Statement.Gets(_, _) -> Error "Assignment is not allowed at the top level."
        | Parser.Statement.Talk talk -> Ok(env, movie.YieldFrame(state, talk.Subtitle, talk.Speech))

    //

    let run (movie: Frame.MovieBuilder) (env: IEvalEnv, state: Frame.MovieState) (ast: Parser.AST) =
        (Ok(env, state), ast.Statements)
        ||> Seq.fold (fun acc statement ->
            monad {
                let! env, state = acc
                return! runStatement movie (env, state) statement
            })
