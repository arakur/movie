module private Parser.Expr

open Script
open FSharpPlus

let rec tryFrom (intermediate: IntermediateExpr) =
    match intermediate with
    | IntermediateExpr.Numeral(value, measure) -> Ok(Expr.Numeral(value, measure))
    | IntermediateExpr.String(value) -> Ok(Expr.String value)
    | IntermediateExpr.Variable(value) -> Ok(Expr.Variable value)
    | IntermediateExpr.App(func, args) ->
        monad {
            let! func' = tryFrom func

            let! args' =
                args
                |> Seq.fold
                    (fun acc arg ->
                        monad {
                            let! acc' = acc
                            let! arg' = tryFrom arg
                            return arg' :: acc'
                        })
                    (Ok [])
                |>> List.rev

            return Expr.App(func', args')
        }
    | IntermediateExpr.BinOpSeries(init: (IntermediateExpr * BinaryOperator) list, last) ->
        // TODO: Give this from outside.
        let binOpDictionary =
            [ [ BinaryOperator.Other "&&"; BinaryOperator.Other "||" ], Associativity.Left
              [ BinaryOperator.Other "="
                BinaryOperator.Other "<"
                BinaryOperator.Other ">"
                BinaryOperator.Other "<="
                BinaryOperator.Other ">=" ],
              Associativity.None
              [ BinaryOperator.Other "+"; BinaryOperator.Other "-" ], Associativity.Left
              [ BinaryOperator.Other "*"; BinaryOperator.Other "/" ], Associativity.Left ]

        let rec fold
            (dictionary: (BinaryOperator list * Associativity) list)
            (init: (IntermediateExpr * BinaryOperator) list, last: IntermediateExpr)
            : Result<Expr, string> =
            match dictionary with
            | [] ->
                match init with
                | [] -> tryFrom last
                | _ ->
                    Error(
                        snd init.Head
                        |> function
                            | BinaryOperator.Other s -> sprintf "Unknown binary operator `%s`." s
                    )
            | (operators, associativity) :: restDictionary ->
                let rec tryCollect current (init: (IntermediateExpr * BinaryOperator) list) =
                    match init with
                    | [] -> Ok([], (current, last))
                    | (e, op) :: rest when operators |> Seq.contains op ->
                        monad {
                            let! parts, lastPart = tryCollect [] rest
                            return (List.rev current, e, op) :: parts, lastPart
                        }
                    | (e, op) :: rest -> tryCollect ((e, op) :: current) rest

                tryCollect [] init
                |> bind (fun (parts, lastPart) ->
                    if parts = [] then
                        fold restDictionary lastPart
                    else
                        match associativity with
                        | Associativity.Left ->
                            parts
                            |> Seq.map (fun (init', last', op') ->
                                fold restDictionary (init', last') |> Result.map (fun expr -> expr, op'))
                            |> Seq.fold
                                (fun acc res ->
                                    monad {
                                        let! acc' = acc
                                        let! res' = res
                                        return res' :: acc'
                                    })
                                (Ok [])
                            |> bind (fun parts' ->
                                parts'
                                |> Seq.rev
                                |> Seq.reduce (fun (expr0, op0) (expr1, op1) ->
                                    Expr.BinaryOperator(op0, expr0, expr1), op1)
                                |> (fun (expr, op) ->
                                    monad {
                                        let! lastExpr = fold restDictionary lastPart
                                        return Expr.BinaryOperator(op, expr, lastExpr)
                                    }))
                        | Associativity.Right ->
                            parts
                            |> Seq.map (fun (init', last', op') ->
                                fold restDictionary (init', last') |> Result.map (fun expr -> expr, op'))
                            |> Seq.fold
                                (fun acc res ->
                                    monad {
                                        let! acc' = acc
                                        let! res' = res
                                        return res' :: acc'
                                    })
                                (Ok [])
                            |> bind (fun parts' ->
                                (fold restDictionary lastPart, parts')
                                ||> Seq.fold (fun acc (expr, op) ->
                                    monad {
                                        let! acc' = acc
                                        return Expr.BinaryOperator(op, expr, acc')
                                    }))
                        | Associativity.None ->
                            match parts with
                            | [ init', last', op' ] ->
                                monad {
                                    let! first = fold restDictionary (init', last')
                                    let! second = fold restDictionary lastPart
                                    return Expr.BinaryOperator(op', first, second)
                                }
                            | [] -> Error "Unreachable."
                            | _ ->
                                Error(
                                    sprintf
                                        "An operator `%s` is not associative."
                                        (parts.Head
                                         |> function
                                             | (_, _, BinaryOperator.Other s) -> s)
                                ))

        fold binOpDictionary (init, last)

    | IntermediateExpr.Tuple(contents) ->
        (contents, Ok [])
        ||> Seq.foldBack (fun content acc ->
            monad {
                let! acc = acc
                let! expr = tryFrom content
                return expr :: acc
            })
        |>> Expr.Tuple
