namespace Script

open FSharpPlus

module Lexer =
    open FParsec

    [<RequireQualifiedAccess>]
    type LineNode =
        | Numeral of string * measure: string option
        | Variable of string
        | String of string
        | Word of string
        | Gets
        | At
        | Comma
        | OtherOperator of string
        | OpenParen
        | CloseParen

        member this.IsBinOp =
            match this with
            | OtherOperator _ -> true
            | _ -> false

    type Node =
        | Talk of string
        | Line of LineNode list
        | BeginIndent
        | EndIndent

    module Line =
        let indent: string -> int = Seq.takeWhile (fun c -> c = ' ') >> Seq.length

        let content: string -> string =
            String.split [ "//" ] >> Seq.head >> String.trim [ ' ' ]


    //

    type private LexerState =
        { mutable currentIndent: int
          mutable indents: int list
          mutable lines: string seq }

        member this.DropLine = this.lines <- this.lines |> Seq.tail

        member this.PopLine =
            let line = this.lines |> Seq.head
            this.DropLine
            line

        member this.TryNextLine = this.lines |> Seq.tryHead

        member this.PushIndent indent =
            this.indents <- this.currentIndent :: this.indents
            this.currentIndent <- indent

        member this.PopIndentWhile indent : int =
            let mutable count = 0

            while this.currentIndent > indent do
                this.currentIndent <- this.indents |> Seq.head
                this.indents <- this.indents |> List.tail
                count <- count + 1

            count

    let private symbolChars =
        [ '!'
          '"'
          '#'
          '$'
          '%'
          '&'
          '\''
          '('
          ')'
          '*'
          '+'
          ','
          '-'
          '.'
          '/'
          ':'
          ';'
          '<'
          '='
          '>'
          '?'
          '@'
          '['
          '\\'
          ']'
          '^'
          '_'
          '`'
          '{'
          '|'
          '}'
          '~' ]

    let private linesFrom (source: string) : Node seq =
        let parseNumeral: Parser<LineNode, unit> =
            let sign: Parser<string, unit> = pchar '-' <|> pchar '+' |>> (fun c -> c.ToString())

            let digits: Parser<string, unit> = many1 digit |>> String.ofList
            let dotDigits: Parser<string, unit> = pchar '.' >>. digits |>> (fun s -> "." + s)

            let measure: Parser<string, unit> =
                many (satisfy (fun c -> System.Char.IsAsciiLetter c || c = '%'))
                |>> String.ofList

            pipe4 (sign <|> pstring "") digits (dotDigits <|> pstring "") measure (fun s1 s2 s3 m ->
                LineNode.Numeral(s1 + s2 + s3, (if m = "" then None else Some m)))

        let parseVariable: Parser<LineNode, unit> =
            let head = pchar '#'
            let rest = many1 (noneOf [ ' '; ':'; '('; ')' ])
            pipe2 head rest (fun _h r -> r |> String.ofList |> LineNode.Variable)

        let parseString: Parser<LineNode, unit> =
            let escape = pstring "\\\"" >>% '\"'
            let nonEscape = satisfy (fun c -> c <> '"' && c <> '\\')
            let content = manyChars (escape <|> nonEscape)
            let quote = pchar '"'
            content |> between quote quote |>> LineNode.String

        let parseWord: Parser<LineNode, unit> =
            let head = noneOf symbolChars
            let rest = many (noneOf [ ' '; ',' ])
            pipe2 head rest (fun h r -> h :: r |> String.ofList |> LineNode.Word)

        let gets: Parser<LineNode, unit> = pstring "<-" >>% LineNode.Gets

        let at: Parser<LineNode, unit> = pstring "@" >>% LineNode.At

        let comma: Parser<LineNode, unit> = pchar ',' >>% LineNode.Comma

        let openParen: Parser<LineNode, unit> = pchar '(' >>% LineNode.OpenParen

        let closeParen: Parser<LineNode, unit> = pchar ')' >>% LineNode.CloseParen

        let parseOperator: Parser<LineNode, unit> =
            let op = many1 (anyOf symbolChars) |>> String.ofList
            op |>> LineNode.OtherOperator

        let whiteSpace: Parser<unit, unit> = spaces >>% ()

        let parseNode: Parser<LineNode, unit> =
            choice
                [ attempt parseNumeral
                  parseVariable
                  parseString
                  parseWord
                  gets
                  comma
                  openParen
                  closeParen
                  parseOperator ]
            .>> whiteSpace

        let parseLine: Parser<LineNode list, unit> =
            attempt (pipe2 at (many1 parseNode) (fun at nodes -> at :: nodes))
            <|> many1 parseNode

        source
        |> String.split [ ";" ]
        |> Seq.filter (fun s -> s <> "")
        |> Seq.map (fun s -> s |> String.trim [ ' ' ])
        |> Seq.map (fun s -> run parseLine s)
        |> Seq.map (function
            | Success(result, _, _) -> result |> List.ofSeq |> Line
            | Failure(message, _, _) -> failwith message)

    let nodesFrom (source: string) =
        let mutable state =
            { currentIndent = 0
              indents = []
              lines = source |> String.split [ "\n" ] }

        let readTalk lineContent =
            // Read next lines while the indent is greater than the current indent:
            // > these
            //  lines
            //    are all
            //       read but
            // this line is not read.

            let rec loop content =
                seq {
                    match state.TryNextLine with
                    | None -> yield Talk content
                    | Some nextLine ->
                        let nextIndent = nextLine |> Line.indent
                        let nextContent = nextLine |> Line.content

                        if nextContent = "" then
                            state.DropLine
                            yield! loop content
                        elif nextIndent <= state.currentIndent then
                            yield Talk content
                        else
                            state.DropLine
                            yield! loop (content + "\n" + nextContent)
                }

            lineContent |> String.drop 1 |> String.trimStart [ ' ' ] |> loop

        let readLine lineContent =
            // Read next lines while the indent is greater than the current indent and until the line contains a colon:
            // #this
            //   is
            //   a
            //       single
            //          line
            // #this
            //   is a next line: begin indent

            let originalIndent = state.currentIndent

            let rec loop content =
                let segments = content |> String.split [ ":" ]
                let mutable currentSegment = segments |> Seq.head
                let mutable segments' = []

                for segment in segments |> Seq.tail do
                    if currentSegment.EndsWith "\\" || currentSegment.EndsWith ":" then
                        currentSegment <- currentSegment + ":" + segment
                    else
                        segments' <- currentSegment :: segments'
                        currentSegment <- segment

                segments' <- currentSegment :: segments'
                segments' <- segments' |> List.rev

                if segments'.Length > 1 then
                    // Block begins here.
                    let headSegment, intermediateSegments, lastSegment =
                        segments'.Head,
                        segments'.Tail |> Seq.take (Seq.length segments' - 2),
                        segments'.Tail |> Seq.last

                    seq {
                        printf "{%d}" originalIndent // DEBUG
                        printf "{%s}" headSegment // DEBUG

                        let mutable currentIndentSeq = [ originalIndent + String.length headSegment + 1 ]

                        yield! linesFrom headSegment

                        for segment in intermediateSegments do
                            yield BeginIndent

                            // Number of leading spaces of the segment.
                            let segmentSpaces = segment |> Line.indent

                            // Position of colon following with the segment.
                            let currentIndent' = currentIndentSeq.Head + String.length segment + 1

                            // Shift the previous indentation with segmentSpaces and push currentIndent'.
                            currentIndentSeq <-
                                currentIndent' :: currentIndentSeq.Head + segmentSpaces :: currentIndentSeq.Tail

                            yield! linesFrom segment

                        // The last segment has no following colon and so do only adding leading spaces to the previous indentation.
                        let lastSpaces = lastSegment |> Line.indent
                        currentIndentSeq <- currentIndentSeq.Head + lastSpaces :: currentIndentSeq.Tail

                        let lastContent = lastSegment |> Line.content

                        if lastContent = "" then
                            // 最後のセグメントが空ならば，次の行で行われるインデントにこの行における最後以外のコロンの分の回数を上乗せする．
                            // これで次の行では `currentIndentSeq - 1` 回同じ場所でインデントされた扱いとなる．
                            match state.TryNextLine with
                            | None -> failwith "Unexpected end of file."
                            | Some nextLine ->
                                let nextIndent = nextLine |> Line.indent

                                for _ in 1 .. (currentIndentSeq.Length - 2) do
                                    state.PushIndent nextIndent
                        else
                            // 最後のセグメントが空でないならば，この行で行われたインデントはすべて通常通りインデントされた扱いとなる．
                            // FIXME: インデントがずれてしまうので直す．
                            printf "{%A}" currentIndentSeq // DEBUG
                            currentIndentSeq |> Seq.rev |> Seq.iter state.PushIndent
                            // そして最後のセグメントを追加する．
                            yield BeginIndent
                            yield! linesFrom lastContent
                    }
                else
                    // Block does not begin here; continue reading lines or end.
                    match state.TryNextLine with
                    | None -> seq { yield! linesFrom content }
                    | Some nextLine ->
                        let nextIndent = nextLine |> Line.indent
                        let nextContent = nextLine |> Line.content

                        if nextContent = "" then
                            state.DropLine
                            loop content
                        elif nextIndent <= originalIndent then
                            seq { yield! linesFrom content }
                        else
                            state.DropLine
                            loop (content + " " + nextContent)

            loop lineContent

        let readOnce () =
            let line = state.PopLine

            let indent = line |> Line.indent
            let content = line |> Line.content

            seq {
                if content <> "" then
                    if indent > state.currentIndent then
                        yield BeginIndent
                        state.PushIndent indent
                    elif indent < state.currentIndent then
                        let count = state.PopIndentWhile indent

                        for _ in 1..count -> EndIndent

                    if content.StartsWith ">" then
                        yield! readTalk content
                    else
                        yield! readLine content
            }

        //

        seq {
            while not (Seq.isEmpty state.lines) do
                yield! readOnce ()

            let lastIndent = state.PopIndentWhile 0
            for _ in 1..lastIndent -> EndIndent
        }
