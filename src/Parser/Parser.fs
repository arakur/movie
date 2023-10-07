namespace Parser

[<AutoOpen>]
module Parser =
    let parse (source: string) =
        source.Replace("\r\n", "\n") // Preprocess.
        |> Lexer.nodesFrom
        |> AST.from
