namespace Script

open FSharpPlus
open FSharpPlus.Data
open FSharpx.Collections

type RevList<'a> = { Contents: 'a list; Length: int }

module RevList =
    let empty<'a> = ({ Contents = []; Length = 0 }: RevList<'a>)

    let add<'a> v (this: RevList<'a>) =
        { Contents = v :: this.Contents
          Length = this.Length + 1 }

    let singleton<'a> (v: 'a) = { Contents = [ v ]; Length = 1 }

    let toList<'a> (this: RevList<'a>) = this.Contents |> List.rev

    let toArray<'a> (this: RevList<'a>) = this |> toList |> Array.ofList

module private ResultExt =
    let orElse<'a> (secondThunk: unit -> Result<'a, string>) (first: Result<'a, string>) =
        first
        |> Result.bindError (fun error -> secondThunk () |> Result.mapError (sprintf "%s && %s" error))

    let sequence<'a, 'b> (xs: Result<'a, 'b> seq) : Result<'a seq, 'b> =
        (Ok [], xs)
        ||> Seq.fold (fun acc x ->
            monad {
                let! acc = acc
                let! x = x
                return! Ok(acc |> List.cons x)
            })
        |> Result.map Seq.rev

module private ArrayExt =
    let tryAsEmpty<'a, 'e> (message: 'e) (xs: 'a array) : Result<unit, 'e> =
        match xs with
        | [||] -> Ok()
        | _ -> Error message

    let tryAsSingleton<'a, 'e> (message: 'e) (xs: 'a array) : Result<'a, 'e> =
        match xs with
        | [| x |] -> Ok x
        | _ -> Error message

    let tryAsTuple2<'a, 'e> (message: 'e) (xs: 'a array) : Result<'a * 'a, 'e> =
        match xs with
        | [| x0; x1 |] -> Ok(x0, x1)
        | _ -> Error message

    let tryAsTuple3<'a, 'e> (message: 'e) (xs: 'a array) : Result<'a * 'a * 'a, 'e> =
        match xs with
        | [| x0; x1; x2 |] -> Ok(x0, x1, x2)
        | _ -> Error message
