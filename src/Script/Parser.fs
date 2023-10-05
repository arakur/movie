namespace Script

module Parser =
    open FSharpPlus

    [<RequireQualifiedAccess>]
    type BinaryOperator =
        | Other of string

        member this.Name =
            match this with
            | Other s -> s

    [<RequireQualifiedAccess>]
    type IntermediateExpr =
        | Numeral of string * measure: string option
        | String of string
        | Variable of string
        | App of IntermediateExpr * IntermediateExpr list
        | BinOpSeries of init: (IntermediateExpr * BinaryOperator) list * last: IntermediateExpr
        | Tuple of IntermediateExpr list

    [<RequireQualifiedAccess>]
    type Intermediate =
        | Do of IntermediateExpr
        | At of IntermediateExpr
        | Gets of IntermediateExpr * IntermediateExpr

        static member initFrom(line: Lexer.LineNode list) =
            let singleNode (node: Lexer.LineNode) =
                match node with
                | Lexer.LineNode.Numeral(value, measure) -> Ok(IntermediateExpr.Numeral(value, measure))
                | Lexer.LineNode.String(value)
                | Lexer.LineNode.Word(value) -> Ok(IntermediateExpr.String value)
                | Lexer.LineNode.Variable(value) -> Ok(IntermediateExpr.Variable value)
                | _ -> Error "Unexpected node."

            let parseSingleNode (line: Lexer.LineNode list) =
                match line with
                | [] -> Error "Empty line."
                | node :: rest -> singleNode node |>> (fun n -> n, rest)

            let binOp (node: Lexer.LineNode) =
                match node with
                | Lexer.LineNode.OtherOperator(value) -> Ok(BinaryOperator.Other value)
                | _ -> Error "Expected bin op."

            let parseBinOp (line: Lexer.LineNode list) =
                match line with
                | [] -> Error "Empty line."
                | node :: rest -> binOp node |>> (fun n -> n, rest)

            let rec parseParen (line: Lexer.LineNode list) =
                match line with
                | [] -> Error "Empty line."
                | Lexer.LineNode.OpenParen :: rest ->
                    let rec collectContent line =
                        match line with
                        | Lexer.LineNode.CloseParen :: rest' -> Ok([], rest')
                        | _ ->
                            parseBinOpSeries' line
                            |> Result.bind (function
                                | expr, Lexer.LineNode.CloseParen :: rest' -> Ok([ expr ], rest')
                                | expr, Lexer.LineNode.Comma :: rest' ->
                                    collectContent rest'
                                    |> Result.bind (fun (restContents, rest'') -> Ok(expr :: restContents, rest''))
                                | _ -> Error "Expected comma or close paren.")

                    collectContent rest
                    |> Result.bind (fun (contents, rest') ->
                        match contents with
                        | [ expr ] -> Ok(expr, rest')
                        | _ -> Ok(IntermediateExpr.Tuple(contents), rest'))
                | _ -> Error "Expected open paren."

            and parseTerm (line: Lexer.LineNode list) =
                let trySingleOrParen line =
                    parseParen line |> Result.bindError (fun _ -> parseSingleNode line)

                let rec collectTail line =
                    match trySingleOrParen line with
                    | Error(_) -> [], line
                    | Ok(head, rest) ->
                        let tail', rest' = collectTail rest
                        head :: tail', rest'

                trySingleOrParen line
                |>> (fun (head, rest) ->
                    let tail, rest' = collectTail rest

                    if tail = [] then
                        head, rest'
                    else
                        IntermediateExpr.App(head, tail), rest')

            and parseBinOpSeries
                (line: Lexer.LineNode list)
                : Result<(IntermediateExpr * BinaryOperator) list * IntermediateExpr * Lexer.LineNode list, string> =
                let tryTermBinOp =
                    line |> parseTerm |>> (fun (term, rest) -> term, rest, parseBinOp rest)

                tryTermBinOp
                |> Result.bind (function
                    | term, rest, Error _ -> Ok([], term, rest)
                    | term, _, Ok(binOp, rest) ->
                        parseBinOpSeries rest
                        |>> (fun (init, last, rest') -> (term, binOp) :: init, last, rest'))

            and parseBinOpSeries' (line: Lexer.LineNode list) =
                parseBinOpSeries line
                |> Result.bind (function
                    | [], last, rest -> Ok(last, rest)
                    | init, last, rest -> Ok(IntermediateExpr.BinOpSeries(init, last), rest))

            let parseAt (line: Lexer.LineNode list) =
                match line with
                | Lexer.LineNode.At :: rest ->
                    parseBinOpSeries' rest
                    |> Result.bind (fun (ret, rest') ->
                        if rest' <> [] then
                            Error "Expected end of line."
                        else
                            Ok(At ret))
                | _ -> Error "Expected `@`."

            let parseGets (line: Lexer.LineNode list) =
                let rec findGets (prev: Lexer.LineNode list) (line: Lexer.LineNode list) =
                    match line with
                    | Lexer.LineNode.Gets :: rest -> Ok(prev |> List.rev, rest)
                    | node :: rest -> findGets (node :: prev) rest
                    | _ -> Error "Expected `<-`."

                monad {
                    let! target, content = findGets [] line

                    let! target', targetRest = parseBinOpSeries' target

                    do!
                        if targetRest <> [] then
                            Error "Expected end of line."
                        else
                            Ok()

                    let! content', contentRest = parseBinOpSeries' content

                    do!
                        if contentRest <> [] then
                            Error "Expected end of line."
                        else
                            Ok()

                    return Gets(target', content')
                }

            parseAt line
            |> Result.bindError (fun _ -> parseGets line)
            |> Result.bindError (fun _ ->
                parseBinOpSeries' line
                |> Result.bind (function
                    | ret, [] -> Ok(Do ret)
                    | _, rest -> Error(sprintf "Unexpected: %A." rest)))

    [<RequireQualifiedAccess>]
    type Associativity =
        | Left
        | Right
        | None

    type Talk = { Subtitle: string; Speech: string }

    [<RequireQualifiedAccess>]
    type Expr =
        | Numeral of string * measure: string option
        | String of string
        | Variable of string
        | App of Expr * args: Expr list
        | BinaryOperator of BinaryOperator * Expr * Expr
        | Tuple of Expr list

        static member tryFrom(intermediate: IntermediateExpr) =
            match intermediate with
            | IntermediateExpr.Numeral(value, measure) -> Ok(Numeral(value, measure))
            | IntermediateExpr.String(value) -> Ok(String value)
            | IntermediateExpr.Variable(value) -> Ok(Variable value)
            | IntermediateExpr.App(func, args) ->
                monad {
                    let! func' = Expr.tryFrom func

                    let! args' =
                        args
                        |> Seq.fold
                            (fun acc arg ->
                                monad {
                                    let! acc' = acc
                                    let! arg' = Expr.tryFrom arg
                                    return arg' :: acc'
                                })
                            (Ok [])
                        |>> List.rev

                    return App(func', args')
                }
            | IntermediateExpr.BinOpSeries(init, last) ->
                // TODO: Give this from outside.
                let binOpDictionary =
                    [ [ BinaryOperator.Other "&&"; BinaryOperator.Other "||" ], Associativity.Left
                      [ BinaryOperator.Other "="
                        BinaryOperator.Other "<"
                        BinaryOperator.Other ">"
                        BinaryOperator.Other "<="
                        BinaryOperator.Other ">=" ],
                      Associativity.None
                      [ BinaryOperator.Other "+"; BinaryOperator.Other "-" ], Associativity.Left
                      [ BinaryOperator.Other "*"; BinaryOperator.Other "/" ], Associativity.Left

                      ]

                let rec fold
                    (dictionary: (BinaryOperator list * Associativity) list)
                    (init: (IntermediateExpr * BinaryOperator) list, last: IntermediateExpr)
                    : Result<Expr, string> =
                    match dictionary with
                    | [] ->
                        match init with
                        | [] -> Expr.tryFrom last
                        | _ ->
                            Error(
                                snd init.Head
                                |> function
                                    | BinaryOperator.Other s -> sprintf "Unknown binary operator `%s`." s
                            )
                    | (operators, associativity) :: restDictionary ->
                        let rec tryCollect current (init: (IntermediateExpr * BinaryOperator) list) =
                            match init with
                            | [] -> Ok([], (current, last))
                            | (e, op) :: rest when operators |> Seq.contains op ->
                                monad {
                                    let! parts, lastPart = tryCollect [] rest
                                    return (List.rev current, e, op) :: parts, lastPart
                                }
                            | (e, op) :: rest -> tryCollect ((e, op) :: current) rest

                        tryCollect [] init
                        |> bind (fun (parts, lastPart) ->
                            if parts = [] then
                                fold restDictionary lastPart
                            else
                                match associativity with
                                | Associativity.Left ->
                                    parts
                                    |> Seq.map (fun (init', last', op') ->
                                        fold restDictionary (init', last') |> Result.map (fun expr -> expr, op'))
                                    |> Seq.fold
                                        (fun acc res ->
                                            monad {
                                                let! acc' = acc
                                                let! res' = res
                                                return res' :: acc'
                                            })
                                        (Ok [])
                                    |> bind (fun parts' ->
                                        parts'
                                        |> Seq.rev
                                        |> Seq.reduce (fun (expr0, op0) (expr1, op1) ->
                                            BinaryOperator(op0, expr0, expr1), op1)
                                        |> (fun (expr, op) ->
                                            monad {
                                                let! lastExpr = fold restDictionary lastPart
                                                return BinaryOperator(op, expr, lastExpr)
                                            }))
                                | Associativity.Right ->
                                    parts
                                    |> Seq.map (fun (init', last', op') ->
                                        fold restDictionary (init', last') |> Result.map (fun expr -> expr, op'))
                                    |> Seq.fold
                                        (fun acc res ->
                                            monad {
                                                let! acc' = acc
                                                let! res' = res
                                                return res' :: acc'
                                            })
                                        (Ok [])
                                    |> bind (fun parts' ->
                                        (fold restDictionary lastPart, parts')
                                        ||> Seq.fold (fun acc (expr, op) ->
                                            monad {
                                                let! acc' = acc
                                                return BinaryOperator(op, expr, acc')
                                            }))
                                | Associativity.None ->
                                    match parts with
                                    | [ init', last', op' ] ->
                                        monad {
                                            let! first = fold restDictionary (init', last')
                                            let! second = fold restDictionary lastPart
                                            return BinaryOperator(op', first, second)
                                        }
                                    | [] -> Error "Unreachable."
                                    | _ ->
                                        Error(
                                            sprintf
                                                "An operator `%s` is not associative."
                                                (parts.Head
                                                 |> function
                                                     | (_, _, BinaryOperator.Other s) -> s)
                                        ))

                fold binOpDictionary (init, last)

            | IntermediateExpr.Tuple(contents) ->
                (contents, Ok [])
                ||> Seq.foldBack (fun content acc ->
                    monad {
                        let! acc = acc
                        let! expr = Expr.tryFrom content
                        return expr :: acc
                    })
                |>> Tuple

    and Statement =
        | Do of Expr * block: Statement list option
        | At of Expr * block: Statement list option
        | Gets of Expr * Expr
        | Talk of Talk

    type AST =
        { Statements: Statement list }

        static member from(nodes: Lexer.Node list) =
            let rec statement (intermediate: Intermediate) =
                match intermediate with
                | Intermediate.Do e -> Expr.tryFrom e |>> (fun e -> Do(e, None))
                | Intermediate.At e -> Expr.tryFrom e |>> (fun e -> At(e, None))
                | Intermediate.Gets(target, content) ->
                    monad {
                        let! target' = Expr.tryFrom target
                        let! content' = Expr.tryFrom content
                        return Gets(target', content')
                    }

            let rec statements nodes currentRev =
                match nodes with
                | [] -> Ok(List.rev currentRev, [])
                | node :: rest ->
                    match node with
                    | Lexer.Node.Talk talkTokens ->
                        let subtitle =
                            talkTokens
                            |> Seq.choose (function
                                | Lexer.TalkToken.Both s
                                | Lexer.TalkToken.Subtitle s -> Some s
                                | Lexer.TalkToken.Speech _ -> None)
                            |> String.concat ""

                        let speech =
                            talkTokens
                            |> Seq.choose (function
                                | Lexer.TalkToken.Both s
                                | Lexer.TalkToken.Speech s -> Some s
                                | Lexer.TalkToken.Subtitle _ -> None)
                            |> String.concat ""

                        statements rest (Talk { Subtitle = subtitle; Speech = speech } :: currentRev)
                    | Lexer.Node.Line line ->
                        monad {
                            let! intermediate = Intermediate.initFrom line
                            let! statement = statement intermediate
                            return! statements rest (statement :: currentRev)
                        }
                    | Lexer.Node.BeginIndent ->
                        match currentRev with
                        | [] -> Error "Unexpected begin indent."
                        | Do(e, None) :: currentRev' ->
                            monad {
                                let! blockContent, rest' = statements rest []
                                let statement' = Do(e, Some blockContent)
                                return! statements rest' (statement' :: currentRev')
                            }
                        | At(e, None) :: currentRev' ->
                            monad {
                                let! blockContent, rest' = statements rest []
                                let statement' = At(e, Some blockContent)
                                return! statements rest' (statement' :: currentRev')
                            }
                        | _ -> Error "Unexpected begin indent."
                    | Lexer.Node.EndIndent -> Ok(List.rev currentRev, rest)

            statements nodes [] |>> (fun (statements, _) -> { Statements = statements })
