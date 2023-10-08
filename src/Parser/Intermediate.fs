namespace Parser

open Script

open FSharpPlus

type BinaryOperatorName = string

[<RequireQualifiedAccess>]
type private IntermediateExpr =
    | Numeral of string * _measure: string option
    | String of string
    | Variable of string
    | App of IntermediateExpr * IntermediateExpr list
    | BinOpSeries of _init: (IntermediateExpr * BinaryOperatorName) list * _last: IntermediateExpr
    | Tuple of IntermediateExpr list

[<RequireQualifiedAccess>]
type private Intermediate =
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
            | Lexer.LineNode.OtherOperator value -> Ok(Script.BinaryOperator.Other value)
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
            : Result<(IntermediateExpr * BinaryOperatorName) list * IntermediateExpr * Lexer.LineNode list, string> =
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

                if contentRest <> [] then
                    do! Error "Expected end of line."

                return Gets(target', content')
            }

        parseAt line
        |> Result.bindError (fun _ -> parseGets line)
        |> Result.bindError (fun _ ->
            parseBinOpSeries' line
            |> Result.bind (function
                | ret, [] -> Ok(Do ret)
                | _, rest -> Error(sprintf "Unexpected: %A." rest)))
