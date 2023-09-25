namespace FFmpeg

type Path = string

[<RequireQualifiedAccess>]
type NodeAVType =
    | A
    | V
    | AV

    member this.Compose() =
        match this with
        | A -> "a"
        | V -> "v"
        | AV -> "av"

[<RequireQualifiedAccess>]
type Node =
    | Input of index: int * avType: NodeAVType
    | Intermediate of name: string
    | Inner of index: int


    static member AInput index = Input(index, NodeAVType.A)
    static member VInput index = Input(index, NodeAVType.V)
    static member AVInput index = Input(index, NodeAVType.AV)

    member this.Name =
        match this with
        | Input(index, av) -> $"{index}:{av.Compose()}"
        | Intermediate name -> $"{name}"
        | Inner index -> $"__{index}__"

    member this.Compose() = "[" + this.Name + "]"

    member this.MappingName =
        match this with
        | Input(_, _) -> this.Name
        | Intermediate _
        | Inner _ -> $"[{this.Name}]"

[<RequireQualifiedAccess>]
type FArg =
    | V of string
    | KV of key: string * value: string

    member this.Compose() =
        match this with
        | V v -> v
        | KV(k, v) -> $"{k}={v}"

type Filter =
    { Inputs: Node list
      Outputs: Node list
      Name: string
      Args: FArg list }

    static member Create name args inputs outputs =
        { Inputs = inputs
          Outputs = outputs
          Name = name
          Args = args }

    member this.Compose() =
        let inputs =
            this.Inputs |> List.map (fun node -> node.Compose()) |> String.concat ""

        let outputs =
            this.Outputs |> List.map (fun node -> node.Compose()) |> String.concat ""

        let args =
            this.Args
            |> List.map (fun arg -> arg.Compose())
            |> String.concat ":"
            |> fun s -> if s = "" then "" else "=" + s

        inputs + this.Name + args + outputs
