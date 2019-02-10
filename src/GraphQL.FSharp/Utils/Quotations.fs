[<AutoOpen>]
module GraphQL.FSharp.Utils.Quotations

open FSharp.Quotations
open FSharp.Quotations.Patterns

let (|WithValueTyped|_|) (expr: Expr<'t>) =
    match expr with
    | WithValue (value, ``type``, expr)
        when (value :? 't)
        && (``type`` = typeof<'t>)
        && (expr :? Expr<'t>) ->
        Some (value :?> 't, expr :?> Expr<'t>)
    | _ -> None
