module Parser.AST

open Script
open FSharpPlus

let rec private from (nodes: Lexer.Node list) =
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
        | Intermediate.BindsTo(target, pattern) ->
            monad {
                let! target' =
                    match target with
                    | None -> Ok None
                    | Some target -> Expr.tryFrom target |>> Some

                let pattern' =
                    match pattern with
                    | Parser.Pattern.Variable name -> Pattern.Variable name

                return BindsTo(target', pattern')
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


let parse (source: string) =
    source.Replace("\r\n", "\n") // Preprocess.
    |> Lexer.nodesFrom
    |> from
