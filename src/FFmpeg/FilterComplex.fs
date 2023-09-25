namespace FFmpeg


type FilterComplex =
    { Filters: Filter list
      Mappings: Node list }

    /// <summary>
    /// Generate a filter_complex argument for FFmpeg.
    /// </summary>
    member this.Compose() =
        let filterComplex =
            this.Filters
            |> List.map (fun filter -> filter.Compose())
            |> String.concat ";"
            |> fun s -> s.Replace("\"", "\\\"")
            |> fun s -> $"-filter_complex \"{s}\""

        let mappings =
            this.Mappings
            |> List.map (fun node -> $"-map \"{node.MappingName}\"")
            |> String.concat " "

        $"{filterComplex} {mappings}"

    /// <summary>
    /// Generate a GraphViz DOT file for debugging.
    /// </summary>
    member this.ToGraphVizDot() =
        let node name props =
            let props' = props |> Seq.map (fun (k, v) -> $"{k}={v}") |> String.concat ", "
            $"\"{name}\" [{props'}]"

        let edge source target = $"\"{source}\" -> \"{target}\""

        let dottedEdge source target =
            $"\"{source}\" -> \"{target}\" [style=dotted]"

        let targetInputNode node =
            match node with
            | Node.Input(_, _) -> seq { yield dottedEdge "input" (node.Compose()) }
            | Node.Intermediate name -> Seq.empty
            | Node.Inner _ -> Seq.empty

        seq {
            yield node "input" [ "shape", "diamond" ]
            yield node "output" [ "shape", "diamond" ]

            for i, filter in this.Filters |> Seq.indexed do
                for input in filter.Inputs do
                    yield! targetInputNode input

                let args =
                    filter.Args
                    |> List.map (fun arg -> arg.Compose())
                    |> String.concat ":"
                    |> fun s -> if s = "" then "" else "=" + s

                let f = $"filter{i}"
                let filterLabel = $"\"{filter.Name}{args}\""

                yield node f [ "shape", "box"; "label", filterLabel ]

                for input in filter.Inputs do
                    yield edge (input.Compose()) f

                for output in filter.Outputs do
                    yield edge f (output.Compose())

            for node in this.Mappings do
                yield! targetInputNode node
                yield dottedEdge (node.Compose()) "output"
        }
        |> String.concat "\n"
        |> fun filters -> $"digraph {{\n{filters}\n}}"
