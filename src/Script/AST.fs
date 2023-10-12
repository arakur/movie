namespace Script

[<RequireQualifiedAccess>]
type BinaryOperator =
    | Other of string

    member this.Name =
        match this with
        | Other s -> s

[<RequireQualifiedAccess>]
type Associativity =
    | Left
    | Right
    | None

type Talk = { Subtitle: string; Speech: string }

type Pattern = Variable of string

[<RequireQualifiedAccess>]
type Expr =
    | Numeral of string * measure: string option
    | String of string
    | Variable of string
    | App of Expr * args: Expr list
    | BinaryOperator of BinaryOperator * Expr * Expr
    | Tuple of Expr list

type Statement =
    | Do of Expr * block: Statement list option
    | At of Expr * block: Statement list option
    | Gets of Expr * Expr
    | BindsTo of Expr * Pattern
    | Talk of Talk

type AST = { Statements: Statement list }
