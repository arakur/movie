namespace FFmpeg

open Types

type FFmpegBuilderState =
    { Inputs: Input list
      Filters: Filter list
      Mappings: Node list
      InnerNodeIndex: int }

type FFmpegBuilderStateM<'a> =
    | FFmpegStateM of (FFmpegBuilderState -> 'a * FFmpegBuilderState)

    member this.Run =
        match this with
        | FFmpegStateM f -> f

    static member (|>>)(FFmpegStateM m, f: 'a -> 'b) =
        FFmpegStateM(fun s ->
            let (a, s') = m s
            f a, s')

    static member (>>=)(FFmpegStateM m, f: 'a -> FFmpegBuilderStateM<'b>) =
        FFmpegStateM(fun s ->
            let (a, s') = m s
            (f a).Run s')


module FilterComplexStateM =
    let run (FFmpegStateM f) = f

    let return_ a = FFmpegStateM(fun s -> (a, s))

    let bind (FFmpegStateM m) (f: 'a -> FFmpegBuilderStateM<'b>) =
        FFmpegStateM(fun s ->
            let (a, s') = m s
            run (f a) s')

    let build (args: Arg list) (output: Path) (FFmpegStateM m) : FFmpegArguments =
        let (_, s) =
            m
                { Inputs = []
                  Filters = []
                  Mappings = []
                  InnerNodeIndex = 0 }

        let filterComplex =
            { Filters = List.rev s.Filters
              Mappings = List.rev s.Mappings }

        { Args = args
          FilterComplex = Some filterComplex
          Inputs = List.rev s.Inputs
          Output = output }

type FFmpegBuilder() =
    member __.Bind(FFmpegStateM m, f) =
        FFmpegStateM(fun s ->
            let (a, s') = m s
            FilterComplexStateM.run (f a) s')

    member __.Return a = FilterComplexStateM.return_ a

    member __.ReturnFrom(FFmpegStateM m) = FFmpegStateM m

    member __.Zero() = FFmpegStateM(fun s -> ((), s))

    member __.Combine(FFmpegStateM m1, FFmpegStateM m2) =
        FFmpegStateM(fun s ->
            let (a, s') = m1 s
            let (b, s'') = m2 s'
            (a, s''))

    member __.Delay(f: unit -> FFmpegBuilderStateM<'a>) = f ()

    member this.For(xs: 'a seq, f: 'a -> FFmpegBuilderStateM<'b>) =
        xs |> Seq.fold (fun acc x -> this.Combine(acc, f x)) (this.Zero())

module FFmpegBuilder =
    let builder = FFmpegBuilder()

    type InputNode =
        { VInput: Node
          AInput: Node
          AVInput: Node }

        static member aInput(inputNode: InputNode) = inputNode.AInput
        static member vInput(inputNode: InputNode) = inputNode.VInput
        static member avInput(inputNode: InputNode) = inputNode.AVInput

    let inputNode name =
        FFmpegStateM(fun s ->
            let index = s.Inputs.Length

            { VInput = Node.VInput index
              AInput = Node.AInput index
              AVInput = Node.AVInput index },
            { s with Inputs = name :: s.Inputs })

    let inputNodeN (inputs: Input seq) =
        FFmpegStateM(fun s ->
            let index = s.Inputs.Length

            let nodes, newInputs =
                inputs
                |> Seq.indexed
                |> Seq.fold
                    (fun (nodes, inputs) (i, name) ->
                        let node =
                            { AInput = Node.AInput(index + i)
                              VInput = Node.VInput(index + i)
                              AVInput = Node.AVInput(index + i) }

                        node :: nodes, name :: inputs)
                    ([], s.Inputs)

            (nodes |> List.rev, { s with Inputs = newInputs }))

    let innerNode =
        FFmpegStateM(fun s ->
            let index = s.InnerNodeIndex
            Node.Inner index, { s with InnerNodeIndex = index + 1 })

    let innerNodeN length =
        FFmpegStateM(fun s ->
            let index = s.InnerNodeIndex
            let nodes = [ for i in 0 .. length - 1 -> Node.Inner(index + i) ]

            nodes,
            { s with
                InnerNodeIndex = index + length })

    let yieldFilter filter =
        FFmpegStateM(fun s ->
            let index = s.InnerNodeIndex

            let s' =
                { s with
                    Filters = filter :: s.Filters
                    Mappings = s.Mappings
                    InnerNodeIndex = index + 1 }

            (), s')

    let mapping node =
        FFmpegStateM(fun s ->
            let s' = { s with Mappings = node :: s.Mappings }

            (), s')

    let concatA (inputs: Node list) (output: Node) =
        yieldFilter (
            Filter.Create
                "concat"
                [ FArg.KV("n", inputs.Length.ToString()); FArg.KV("v", "0"); FArg.KV("a", "1") ]
                inputs
                [ output ]
        )

    let concatV (inputs: Node list) (output: Node) =
        yieldFilter (
            Filter.Create
                "concat"
                [ FArg.KV("n", inputs.Length.ToString()); FArg.KV("v", "1"); FArg.KV("a", "0") ]
                inputs
                [ output ]
        )

    let concatAV (inputs: Node list) (output: Node) =
        yieldFilter (
            Filter.Create
                "concat"
                [ FArg.KV("n", inputs.Length.ToString()); FArg.KV("v", "1"); FArg.KV("a", "1") ]
                inputs
                [ output ]
        )

    let mixAudio (inputs: Node list) (output: Node) =
        builder {
            let length = inputs.Length
            let volume = 1.0 / float length |> string
            let! inners = innerNodeN length

            for input, inner in Seq.zip inputs inners do
                do! yieldFilter (Filter.Create "volume" [ FArg.KV("volume", volume) ] [ input ] [ inner ])

            do! yieldFilter (Filter.Create "amix" [ FArg.KV("inputs", string length) ] inners [ output ])

            return output
        }

    let nullFilter (input: Node) (output: Node) =
        yieldFilter (Filter.Create "null" [] [ input ] [ output ])

    type Layer =
        { Pos: Pos
          Duration: (float * float) option
          Input: Node
          Shortest: bool }

    let overlay2 (layer: Layer) (background: Node) (output: Node) =
        builder {
            do!
                yieldFilter (
                    Filter.Create
                        "overlay"
                        (seq {
                            if layer.Shortest then
                                yield FArg.KV("shortest", "1")

                            yield FArg.KV("x", layer.Pos.X.ToString())
                            yield FArg.KV("y", layer.Pos.Y.ToString())

                            yield!
                                layer.Duration
                                |> Option.map (fun (b, e) -> FArg.KV("enable", sprintf "'between(t,%f,%f)'" b e))
                                |> Option.toList
                         }
                         |> List.ofSeq)
                        [ background; layer.Input ]
                        [ output ]
                )
        }

    let rec overlay (layers: Layer list) (background: Node) (output: Node) =
        match layers with
        | [] -> nullFilter background output
        | layer :: [] -> overlay2 layer background output
        | layer :: layers ->
            builder {
                let! layerOutput = innerNode

                do! overlay2 layer background layerOutput

                do! overlay layers layerOutput output
            }

    let trim (duration: float * float) (input: Node) (output: Node) =
        yieldFilter (
            Filter.Create
                "trim"
                [ FArg.KV("start", duration |> fst |> string)
                  FArg.KV("end", duration |> snd |> string) ]
                [ input ]
                [ output ]
        )

    let colorSource (r: int) (g: int) (b: int) (output: Node) =
        yieldFilter (Filter.Create "color" [ FArg.KV("c", $"#{r:X2}{g:X2}{b:X2}") ] [] [ output ])
