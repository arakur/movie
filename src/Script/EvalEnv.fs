namespace Script

open FSharpPlus
open FSharpPlus.Data
open FSharpx.Collections
open Frame

type EvalEnv
    (
        variables: Map<string, Value>,
        innerOperators: Map<string, int>,
        binaryOperators: Map<string, Value -> Value -> Result<Value, string>>
    ) =
    static member prelude() : EvalEnv =
        let innerOperators = InnerOperator.innerOperators |> Map.ofSeq

        let variables =
            innerOperators
            |> Map.toSeq
            |> Seq.map (fun (name, arity) ->
                if arity = 0 then
                    name, Value.InnerOperatorApplied(name, [||])
                else
                    name, Value.InnerOperator name)
            |> Map.ofSeq

        let binaryOperators = BinaryOperators.binaryOperators |> Map.ofSeq

        EvalEnv(variables, innerOperators, binaryOperators)

    member val Variables = variables
    member val InnerOperators = innerOperators
    member val BinaryOperators = binaryOperators

    member this.WithVariable(var: string, value: Value) =
        EvalEnv(this.Variables.Add(var, value), this.InnerOperators, this.BinaryOperators)

    member this.WithInnerOperator(inner: string, arity: int) =
        EvalEnv(this.Variables, this.InnerOperators.Add(inner, arity), this.BinaryOperators)

    member this.WithBinaryOperator(bin: string, f: Value -> Value -> Result<Value, string>) =
        EvalEnv(this.Variables, this.InnerOperators, this.BinaryOperators.Add(bin, f))

    member this.WithInnerOperatorSynonym(var: string, inner: string) =
        let arity = this.InnerOperators.TryFind inner

        match arity with
        | None -> failwithf "An inner operator `%s` not found." inner
        | Some 0 -> this.WithVariable(var, Value.InnerOperatorApplied(inner, [||]))
        | Some _ -> this.WithVariable(var, Value.InnerOperator inner)

    member this.TryVariable(var: string) = this.Variables.TryFind var

    member this.TryBinaryOperator(op: string) = this.BinaryOperators.TryFind op

    member this.TryArityInnerOperator(op: string) = this.InnerOperators.TryFind op

    interface IEvalEnv with
        member this.TryVariable(var: string) = this.TryVariable var
        member this.TryInnerOperator(op: string) = this.TryArityInnerOperator op
        member this.TryBinaryOperator(op: string) = this.TryBinaryOperator op
        member this.TryArityInnerOperator(op: string) = this.TryArityInnerOperator op
