namespace Script

module Parser =
    open FSharpPlus

    [<RequireQualifiedAccess>]
    type BinOp = Other of string

    [<RequireQualifiedAccess>]
    type IntermediateExpr =
        | Numeral of string * measure: string option
        | String of string
        | Variable of string
        | App of IntermediateExpr * IntermediateExpr list
        | BinOpSeries of init: (IntermediateExpr * BinOp) list * last: IntermediateExpr
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
                | Lexer.LineNode.OtherOperator(value) -> Ok(BinOp.Other value)
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
                : Result<(IntermediateExpr * BinOp) list * IntermediateExpr * Lexer.LineNode list, string> =
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
            |> Result.bindError (fun _ ->
                parseBinOpSeries' line
                |> Result.bind (function
                    | ret, [] -> Ok(Do ret)
                    | _, rest -> Error(sprintf "Unexpected: %A." rest)))
            |> Result.bindError (fun _ -> parseGets line)

module AST =
    open FSharpPlus

    [<RequireQualifiedAccess>]
    type BinOp =
        | Other of string

        static member from(binOp: Parser.BinOp) =
            match binOp with
            | Parser.BinOp.Other s -> Other s

    [<RequireQualifiedAccess>]
    type Associativity =
        | Left
        | Right
        | None

    [<RequireQualifiedAccess>]
    type Expr =
        | Numeral of string * measure: string option
        | String of string
        | Variable of string
        | App of Expr * args: Expr list
        | BinOp of BinOp * Expr * Expr
        | Tuple of Expr list

        static member tryFrom(intermediate: Parser.IntermediateExpr) =
            match intermediate with
            | Parser.IntermediateExpr.Numeral(value, measure) -> Ok(Numeral(value, measure))
            | Parser.IntermediateExpr.String(value) -> Ok(String value)
            | Parser.IntermediateExpr.Variable(value) -> Ok(Variable value)
            | Parser.IntermediateExpr.App(func, args) ->
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
            | Parser.IntermediateExpr.BinOpSeries(init, last) ->
                // TODO: Give this from outside.
                let binOpDictionary =
                    [ [ BinOp.Other "&&"; BinOp.Other "||" ], Associativity.Left
                      [ BinOp.Other "="
                        BinOp.Other "<"
                        BinOp.Other ">"
                        BinOp.Other "<="
                        BinOp.Other ">=" ],
                      Associativity.None
                      [ BinOp.Other "+"; BinOp.Other "-" ], Associativity.Left
                      [ BinOp.Other "*"; BinOp.Other "/" ], Associativity.Left

                      ]

                let rec fold
                    (dictionary: (BinOp list * Associativity) list)
                    (init: (Parser.IntermediateExpr * Parser.BinOp) list, last: Parser.IntermediateExpr)
                    : Result<Expr, string> =
                    match dictionary with
                    | [] ->
                        match init with
                        | [] -> Expr.tryFrom last
                        | _ ->
                            Error(
                                snd init.Head
                                |> function
                                    | Parser.BinOp.Other s -> sprintf "Unknown binary operator `%s`." s
                            )
                    | (operators, associativity) :: restDictionary ->
                        let rec tryCollect current (init: (Parser.IntermediateExpr * Parser.BinOp) list) =
                            match init with
                            | [] -> Ok([], (current, last))
                            | (e, op) :: rest when operators |> Seq.contains (BinOp.from op) ->
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
                                            BinOp(BinOp.from op0, expr0, expr1), op1)
                                        |> (fun (expr, op) ->
                                            monad {
                                                let! lastExpr = fold restDictionary lastPart
                                                return BinOp(BinOp.from op, expr, lastExpr)
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
                                                return BinOp(BinOp.from op, expr, acc')
                                            }))
                                | Associativity.None ->
                                    match parts with
                                    | [ init', last', op' ] ->
                                        monad {
                                            let! first = fold restDictionary (init', last')
                                            let! second = fold restDictionary lastPart
                                            return BinOp(BinOp.from op', first, second)
                                        }
                                    | [] -> Error "Unreachable."
                                    | _ ->
                                        Error(
                                            sprintf
                                                "An operator `%s` is not associative."
                                                (parts.Head
                                                 |> function
                                                     | (_, _, Parser.BinOp.Other s) -> s)
                                        ))

                fold binOpDictionary (init, last)

            | Parser.IntermediateExpr.Tuple(contents) ->
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
        | Talk of string

    type AST =
        { Statements: Statement list }

        static member from(nodes: Lexer.Node list) =
            let rec statement (intermediate: Parser.Intermediate) =
                match intermediate with
                | Parser.Intermediate.Do e -> Expr.tryFrom e |>> (fun e -> Do(e, None))
                | Parser.Intermediate.At e -> Expr.tryFrom e |>> (fun e -> At(e, None))
                | Parser.Intermediate.Gets(target, content) ->
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
                    | Lexer.Node.Talk talk -> statements rest (Talk talk :: currentRev)
                    | Lexer.Node.Line line ->
                        monad {
                            let! intermediate = Parser.Intermediate.initFrom line
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
