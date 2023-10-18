module private Parser.Expr

open FSharpPlus

let rec tryFrom (intermediate: IntermediateExpr) =
    match intermediate with
    | IntermediateExpr.Numeral(value, measure) -> Ok(Script.Expr.Numeral(value, measure))
    | IntermediateExpr.String(value) -> Ok(Script.Expr.String value)
    | IntermediateExpr.Variable(value) -> Ok(Script.Expr.Variable value)
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

            return Script.Expr.App(func', args')
        }
    | IntermediateExpr.BinOpSeries(init: (IntermediateExpr * Script.BinaryOperator) list, last) ->
        // TODO: Give this from outside.
        let binOpDictionary =
            [ [ Script.BinaryOperator.Other "&&"; Script.BinaryOperator.Other "||" ], Script.Associativity.Left
              [ Script.BinaryOperator.Other "="
                Script.BinaryOperator.Other "<"
                Script.BinaryOperator.Other ">"
                Script.BinaryOperator.Other "<="
                Script.BinaryOperator.Other ">=" ],
              Script.Associativity.None
              [ Script.BinaryOperator.Other "+"; Script.BinaryOperator.Other "-" ], Script.Associativity.Left
              [ Script.BinaryOperator.Other "*"; Script.BinaryOperator.Other "/" ], Script.Associativity.Left ]

        let rec fold
            (dictionary: (Script.BinaryOperator list * Script.Associativity) list)
            (init: (IntermediateExpr * Script.BinaryOperator) list, last: IntermediateExpr)
            : Result<Script.Expr, string> =
            match dictionary with
            | [] ->
                match init with
                | [] -> tryFrom last
                | _ -> Error((snd init.Head).Name |> sprintf "Unknown binary operator `%s`.")
            | (operators, associativity) :: restDictionary ->
                let rec tryCollect current (init: (IntermediateExpr * Script.BinaryOperator) list) =
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
                        | Script.Associativity.Left ->
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
                                    Script.Expr.BinaryOperator(op0, expr0, expr1), op1)
                                |> (fun (expr, op) ->
                                    monad {
                                        let! lastExpr = fold restDictionary lastPart
                                        return Script.Expr.BinaryOperator(op, expr, lastExpr)
                                    }))
                        | Script.Associativity.Right ->
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
                                        return Script.Expr.BinaryOperator(op, expr, acc')
                                    }))
                        | Script.Associativity.None ->
                            match parts with
                            | [ init', last', op' ] ->
                                monad {
                                    let! first = fold restDictionary (init', last')
                                    let! second = fold restDictionary lastPart
                                    return Script.Expr.BinaryOperator(op', first, second)
                                }
                            | [] -> Error "Unreachable."
                            | _ ->
                                Error(
                                    sprintf
                                        "An operator `%s` is not associative."
                                        (parts.Head
                                         |> function
                                             | _, _, op -> op.Name)
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
        |>> Script.Expr.Tuple
