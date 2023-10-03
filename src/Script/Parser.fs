namespace Script

module AST =
    type BinOp =
        | Get
        | Other of string

    [<RequireQualifiedAccess>]
    type Expr =
        | Numeral of string * measure: string option
        | String of string
        | Variable of string
        | App of Expr * Expr list
        | BinOp of BinOp * Expr * Expr

module Parser =
    [<RequireQualifiedAccess>]
    type Intermediate =
        | Numeral of string * measure: string option
        | String of string
        | Variable of string
        | App of Intermediate * Intermediate list
        | BinOp of AST.BinOp * Intermediate * Intermediate
        | BinOpSeries of initRev: (Intermediate * AST.BinOp) list * last: Intermediate
        | At of Intermediate

        static member initFrom(line: Lexer.LineNode list) =
            let singleNode (node: Lexer.LineNode) =
                match node with
                | Lexer.LineNode.Numeral(value, measure) -> Ok(Numeral(value, measure))
                | Lexer.LineNode.String(value)
                | Lexer.LineNode.Word(value) -> Ok(String value)
                | Lexer.LineNode.Variable(value) -> Ok(Variable value)
                | _ -> Error "Unexpected node."

            let parseSingleNode (line: Lexer.LineNode list) =
                match line with
                | [] -> Error "Empty line."
                | node :: rest -> singleNode node |> Result.map (fun n -> n, rest)

            let binOp (node: Lexer.LineNode) =
                match node with
                | Lexer.LineNode.Gets -> Ok(AST.BinOp.Get)
                | Lexer.LineNode.OtherOperator(value) -> Ok(AST.BinOp.Other value)
                | _ -> Error "Expected bin op."

            let parseBinOp (line: Lexer.LineNode list) =
                match line with
                | [] -> Error "Empty line."
                | node :: rest -> binOp node |> Result.map (fun n -> n, rest)

            let rec parseParen (line: Lexer.LineNode list) =
                match line with
                | [] -> Error "Empty line."
                | Lexer.LineNode.OpenParen :: rest ->
                    parseBinOpSeries rest
                    |> Result.bind (fun (init, last, rest') ->
                        match rest' with
                        | Lexer.LineNode.CloseParen :: rest'' -> Ok(BinOpSeries(init, last), rest'')
                        | _ -> Error "Expected close paren.")
                | _ -> Error "Expected open paren."

            and parseTerm (line: Lexer.LineNode list) =
                let trySingleOrParen line =
                    match parseParen line with
                    | Ok(ret, rest) -> Ok(ret, rest)
                    | Error(_) -> parseSingleNode line

                let rec collectTail line =
                    match trySingleOrParen line with
                    | Error(_) -> [], line
                    | Ok(head, rest) ->
                        let tail', rest' = collectTail rest
                        head :: tail', rest'

                trySingleOrParen line
                |> Result.map (fun (head, rest) ->
                    let tail, rest' = collectTail rest
                    if tail = [] then head, rest' else App(head, tail), rest')

            and parseBinOpSeries
                (line: Lexer.LineNode list)
                : Result<(Intermediate * AST.BinOp) list * Intermediate * Lexer.LineNode list, string> =
                let tryTermBinOp =
                    line
                    |> parseTerm
                    |> Result.map (fun (term, rest) -> term, rest, parseBinOp rest)

                tryTermBinOp
                |> Result.bind (function
                    | term, rest, Error _ -> Ok([], term, rest)
                    | term, _, Ok(binOp, rest) ->
                        match parseBinOpSeries rest with
                        | Error msg2 -> Error msg2
                        | Ok(init, last, rest') -> Ok((term, binOp) :: init, last, rest'))

            let parseBinOpSeries' (line: Lexer.LineNode list) =
                parseBinOpSeries line
                |> Result.bind (function
                    | [], last, rest -> Ok(last, rest)
                    | init, last, rest -> Ok(BinOpSeries(init, last), rest))

            let parseAt (line: Lexer.LineNode list) =
                match line with
                | Lexer.LineNode.At :: rest -> parseBinOpSeries' rest |> Result.map (fun (ret, rest') -> At(ret), rest')
                | _ -> parseBinOpSeries' line

            parseAt line
            |> Result.bind (function
                | ret, [] -> Ok ret
                | _, rest -> Error(sprintf "Unexpected: %A." rest))
