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


let (|PropertyNameGetter|_|) expr =
    match expr with
    | PropertyGet (Some (Var sourceVar), prop, []) -> Some (prop.Name, sourceVar)
    | _ -> None

let (|PropertyNameBasic|_|) expr =
    match expr with
    | Lambda (lambdaVar, PropertyNameGetter (propName, sourceVar)) when lambdaVar = sourceVar -> Some propName
    | _ -> None

let (|MethodInnerLambda|_|) expr =
    match expr with
    | Call (Some (Var sourceVar), method, []) -> Some (method.Name, sourceVar)
    | Application (expr, value) when value = <@@ () @@> ->
        match expr with
        | PropertyNameGetter (propName, sourceVar) -> Some (propName, sourceVar)
        | PropertyGet (Some (Var sourceVar), prop, []) -> Some (prop.Name, sourceVar)
        | _ -> None
    | _ -> None

let (|MethodNameBasic|_|) expr =
    match expr with
    | Lambda (lambdaVar, expr) ->
        match expr with
        | MethodInnerLambda (name, sourceVar) when sourceVar = lambdaVar -> Some name
        | _ -> None
    | _ -> None

let (|FieldName|_|) expr =
    match expr with
    | PropertyNameBasic name -> Some name
    | MethodNameBasic name -> Some name
    | _ -> None

let (|AsyncFieldName|_|) expr = (|MethodNameBasic|_|) expr
