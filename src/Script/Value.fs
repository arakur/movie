namespace Script

open FSharpPlus
open FSharpPlus.Data
open FSharpx.Collections
open System.Collections.Generic

type Measure = string

[<RequireQualifiedAccess>]
type Numeral =
    | Int of int * measure: Measure option
    | Float of float * measure: Measure option

    member this.Measure =
        match this with
        | Int(_, m)
        | Float(_, m) -> m

    static member tryAsInt this =
        match this with
        | Int(i, None) -> Ok i
        | _ -> Error "Invalid type; expected int."

    static member tryAsFloat this =
        match this with
        | Int(i, None) -> Ok(float i)
        | Float(f, None) -> Ok f
        | _ -> Error "Invalid type; expected float."

    static member tryAsFloatMeas measure this =
        match this with
        | Int(i, Some m) when m = measure -> Ok(float i)
        | Float(f, Some m) when m = measure -> Ok f
        | _ -> Error(sprintf "Invalid type; expected float with %s." measure)

    static member tryAdd lhs rhs =
        match lhs, rhs with
        | Int(i0, m0), Int(i1, m1) when m0 = m1 -> Ok(Int(i0 + i1, m0))
        | Float(f0, m0), Float(f1, m1) when m0 = m1 -> Ok(Float(f0 + f1, m0))
        | Int(i0, m0), Float(f1, m1) when m0 = m1 -> Ok(Float(float i0 + f1, m0))
        | Float(f0, m0), Int(i1, m1) when m0 = m1 -> Ok(Float(f0 + float i1, m0))
        | _ -> Error "Invalid type; expected same measure."

    static member trySub lhs rhs =
        match lhs, rhs with
        | Int(i0, m0), Int(i1, m1) when m0 = m1 -> Ok(Int(i0 - i1, m0))
        | Float(f0, m0), Float(f1, m1) when m0 = m1 -> Ok(Float(f0 - f1, m0))
        | Int(i0, m0), Float(f1, m1) when m0 = m1 -> Ok(Float(float i0 - f1, m0))
        | Float(f0, m0), Int(i1, m1) when m0 = m1 -> Ok(Float(f0 - float i1, m0))
        | _ -> Error "Invalid type; expected same measure."

    static member tryLt lhs rhs =
        match lhs, rhs with
        | Int(i0, m0), Int(i1, m1) when m0 = m1 -> Ok(i0 < i1)
        | Float(f0, m0), Float(f1, m1) when m0 = m1 -> Ok(f0 < f1)
        | Int(i0, m0), Float(f1, m1) when m0 = m1 -> Ok(float i0 < f1)
        | Float(f0, m0), Int(i1, m1) when m0 = m1 -> Ok(f0 < float i1)
        | _ -> Error "Invalid type; expected same measure."

[<RequireQualifiedAccess>]
type AssetType =
    | Speech
    | Appearance
    | Subtitle
    | Background
    //
    | Image
    | Video
    | Audio
    | TextBox

type AssetRef = { Type: AssetType; Id: string }

[<RequireQualifiedAccess>]
type Value =
    | Numeral of Numeral
    | String of string
    | Boolean of bool
    | BinaryOperator of string
    | BinaryOperatorLeftApplied of string * Value
    | Tuple of Value array
    | InnerOperator of string
    | InnerOperatorPartiallyApplied of string * arity: int * Value RevList
    | InnerOperatorApplied of string * Value array
    | AssetRef of AssetRef
    | PriorityRelation of lower: AssetRef * upper: AssetRef

    static member tryAsNumeral this =
        match this with
        | Numeral num -> Ok num
        | _ -> Error "Invalid type; expected numeral."

    static member tryAsInt this =
        this |> Value.tryAsNumeral |> Result.bind Numeral.tryAsInt

    static member tryAsFloat this =
        this |> Value.tryAsNumeral |> Result.bind Numeral.tryAsFloat

    static member tryAsFloatMeas measure this =
        this |> Value.tryAsNumeral |> Result.bind (Numeral.tryAsFloatMeas measure)

    static member tryAsFloatSec this =
        this |> Value.tryAsFloatMeas "s" |> Result.map (fun x -> x * 1.<Measure.sec>)

    static member tryAsFloatPx this =
        this |> Value.tryAsFloatMeas "px" |> Result.map (fun x -> x * 1.<Measure.px>)

    static member tryAsFloatPxToInt this =
        this |> Value.tryAsFloatMeas "px" |> Result.map (fun x -> int x * 1<Measure.px>)

    static member tryAsFloatPt this =
        this |> Value.tryAsFloatMeas "pt" |> Result.map (fun x -> x * 1.<Measure.pt>)

    static member tryAsFloatPtToInt this =
        this |> Value.tryAsFloatMeas "pt" |> Result.map (fun x -> int x * 1<Measure.pt>)

    static member tryAsString this =
        match this with
        | String str -> Ok str
        | _ -> Error "Invalid type; expected string."

    static member tryAsTuple this =
        match this with
        | Tuple tup -> Ok tup
        | _ -> Error "Invalid type; expected tuple."

    static member tryAsTupleWithSize size this =
        match this with
        | Tuple tup when tup.Length = size -> Ok tup
        | _ -> Error "Invalid type; expected tuple."

    static member tryAsInnerOperatorApplied this =
        match this with
        | InnerOperatorApplied(inner, args) -> Ok(inner, args)
        | _ -> Error "Invalid type; expected inner operator with full arguments."

    static member tryAsAssetRef this =
        match this with
        | AssetRef asset -> Ok asset
        | _ -> Error "Invalid type; expected asset reference."

    static member tryAsPriorityRelation this =
        match this with
        | PriorityRelation(lower, upper) -> Ok(lower, upper)
        | _ -> Error "Invalid type; expected asset reference."

    static member tryAdd lhs rhs =
        match lhs, rhs with
        | Numeral lhs, Numeral rhs -> Numeral.tryAdd lhs rhs |> Result.map Numeral
        | _ -> Error "Invalid type; expected numeral."

    static member trySub lhs rhs =
        match lhs, rhs with
        | Numeral lhs, Numeral rhs -> Numeral.trySub lhs rhs |> Result.map Numeral
        | _ -> Error "Invalid type; expected numeral."

    static member tryLt lhs rhs =
        match lhs, rhs with
        | Numeral lhs, Numeral rhs -> Numeral.tryLt lhs rhs |> Result.map Boolean
        | AssetRef asset0, AssetRef asset1 -> Ok(PriorityRelation(lower = asset0, upper = asset1))
        | _ -> Error "Invalid type; expected numeral."

    static member tryGt lhs rhs = Value.tryLt rhs lhs

module private InnerOperator =
    [<Literal>]
    let initialize = "initialize"

    [<Literal>]
    let addSpeaker = "add-speaker"

    [<Literal>]
    let appearance = "appearance"


    [<Literal>]
    let setStyle = "set-style"

    [<Literal>]
    let addImage = "add-image"

    [<Literal>]
    let addAudio = "add-audio"

    [<Literal>]
    let addVideo = "add-video"

    [<Literal>]
    let setPriority = "set-priority"

    [<Literal>]
    let remove = "remove"

    [<Literal>]
    let on = "on"

    [<Literal>]
    let hflip = "hflip"

    let innerOperators =
        [| initialize, 0
           addSpeaker, 1
           appearance, 0
           setStyle, 1
           addImage, 0
           addAudio, 0
           addVideo, 0
           setPriority, 0
           remove, 1
           on, 1
           hflip, 0 |]

module private BinaryOperators =
    [<Literal>]
    let add = "+"

    [<Literal>]
    let sub = "-"

    [<Literal>]
    let lt = "<"

    [<Literal>]
    let gt = ">"

    let binaryOperators =
        [| add, Value.tryAdd; sub, Value.trySub; lt, Value.tryLt; gt, Value.tryGt |]

type IEvalEnv =
    abstract member TryVariable: string -> Value option
    abstract member TryInnerOperator: string -> int option
    abstract member TryBinaryOperator: string -> (Value -> Value -> Result<Value, string>) option
    abstract member TryArityInnerOperator: string -> int option

module Value =
    let tryApply (env: IEvalEnv) (arg: Value) (f: Value) =
        match f with
        | Value.Numeral _
        | Value.String _
        | Value.Boolean _
        | Value.Tuple _
        | Value.AssetRef _
        | Value.PriorityRelation _ -> Error "Cannot apply."
        | Value.BinaryOperator op -> Ok <| Value.BinaryOperatorLeftApplied(op, arg)
        | Value.BinaryOperatorLeftApplied(op, lhs) ->
            monad {
                let! f =
                    env.TryBinaryOperator op
                    |> Option.toResultWith (sprintf "Binary operator '%s' not found." op)

                return! f lhs arg
            }
        | Value.InnerOperatorApplied _ -> Error "Too many arguments."
        | Value.InnerOperator op ->
            monad {
                let! arity =
                    env.TryArityInnerOperator op
                    |> Option.toResultWith (sprintf "Inner operator '%s' not found." op)

                match arity with
                | 0 -> return! Error "Cannot apply."
                | 1 -> return Value.InnerOperatorApplied(op, [| arg |])
                | _ -> return Value.InnerOperatorPartiallyApplied(op, arity, RevList.singleton arg)
            }
        | Value.InnerOperatorPartiallyApplied(op, arity, args) ->
            if args.Length + 1 = arity then
                Ok(Value.InnerOperatorApplied(op, args |> RevList.add arg |> RevList.toArray))
            else
                Ok(Value.InnerOperatorPartiallyApplied(op, arity, args |> RevList.add arg))
