[<AutoOpen>]
module GraphQL.FSharp.Utils.Quotations

open System.Reflection
open FSharp.Quotations
open FSharp.Quotations.ExprShape
open FSharp.Quotations.Patterns

let rec internal (|DeepPattern|_|) (|Pattern|_|) (expr: Expr) =
    match expr with
    | ShapeLambda (_, expr) -> (|DeepPattern|_|) (|Pattern|_|) expr
    | ShapeCombination (_, exprs) ->
        exprs
        |> List.tryPick ((|DeepPattern|_|) (|Pattern|_|))
        |> Option.orElse ((|Pattern|_|) expr)
    | ShapeVar _ -> None

let internal (|GetMethod|_|) expr =
    match expr with
    | Call (Some _, method, _) -> Some method
    | _ -> None

let internal (|GetProperty|_|) expr =
    match expr with
    | PropertyGet (Some _, prop, _) -> Some prop
    | _ -> None

let internal hasNoArgs (method: MethodInfo) =
    let args = method.GetParameters ()
    let argLength = Array.length args
    argLength = 0 || (argLength = 1 && args.[0].ParameterType = typeof<unit>)

let (|FieldName|_|) expr =
    let propertyGetter =
        expr
        |> (|DeepPattern|_|) (|GetProperty|_|)
        |> Option.map (fun prop -> prop.Name)

    let getterMethod =
        expr
        |> (|DeepPattern|_|) (|GetMethod|_|)
        |> Option.bind (
            fun method ->
                if method.Name.StartsWith "Get" && hasNoArgs method
                then Some (method.Name.Substring "Get".Length)
                else None
        )

    propertyGetter
    |> Option.orElse getterMethod

let (|MethodName|_|) expr =
    expr
    |> (|DeepPattern|_|) (|GetMethod|_|)
    |> Option.map (fun method -> method.Name)
