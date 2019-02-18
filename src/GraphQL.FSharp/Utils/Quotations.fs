[<AutoOpen>]
module GraphQL.FSharp.Utils.Quotations

open FSharp.Quotations
open FSharp.Quotations.ExprShape
open FSharp.Quotations.Patterns

let (|WithValueTyped|_|) (expr: Expr<'t>) =
    match expr with
    | WithValue (value, ``type``, expr)
        when (value :? 't)
        && (``type`` = typeof<'t>)
        && (expr :? Expr<'t>) ->
        Some (value :?> 't, expr :?> Expr<'t>)
    | _ -> None

let rec internal (|DeepPattern|_|) (|Pattern|_|) (expr: Expr) =
    match expr with
    | Pattern value -> Some value
    | ShapeLambda (_, expr) -> (|DeepPattern|_|) (|Pattern|_|) expr
    | ShapeCombination (_, exprs) ->
        exprs
        |> List.tryPick ((|DeepPattern|_|) (|Pattern|_|))
    | ShapeVar _ -> None

let internal (|GetMethod|_|) expr =
    match expr with
    | Call (Some _, method, _) -> Some method
    | _ -> None

let internal (|GetProperty|_|) expr =
    match expr with
    | PropertyGet (Some _, prop, _) -> Some prop
    | _ -> None

let (|FieldName|_|) expr =
    expr
    |> (|DeepPattern|_|) (|GetProperty|_|)
    |> Option.map (fun prop -> prop.Name)

let (|MethodName|_|) expr =
    expr
    |> (|DeepPattern|_|) (|GetMethod|_|)
    |> Option.map (fun method -> method.Name)
