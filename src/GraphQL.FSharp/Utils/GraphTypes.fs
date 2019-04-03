[<AutoOpen>]
module GraphQL.FSharp.Utils.GraphTypes

open System
open System.Text.RegularExpressions
open GraphQL.Types

let prettyPrintNameConfig detailed (x: #IGraphType) =
    let rec run (x: IGraphType) =
        match x with
        | :? GraphQLTypeReference as x ->
            if detailed
            then sprintf "(%s ref)" x.TypeName
            else x.TypeName
        | :? NonNullGraphType as x ->
            x.ResolvedType
            |> run
            |> sprintf "%s!"
        | :? ListGraphType as x ->
            x.ResolvedType
            |> run
            |> sprintf "[%s]"
        | x -> sprintf "%s" x.Name
    run (x :> IGraphType)

let prettyPrintName x = prettyPrintNameConfig false x
let prettyPrintNameDetailed x = prettyPrintNameConfig true x

let typeName (``type``: Type) =
    Regex.Replace (
        input = ``type``.Name,
        pattern = @"^I(\w+)Grain$",
        replacement = "$1"
    )

let (|InterfaceGraphType|_|) (``type``: IGraphType) =
    match ``type`` with
    | :? IInterfaceGraphType as ``interface`` ->
        let interfaceType = ``interface``.GetType ()
        if interfaceType.IsGenericType
            && interfaceType.GetGenericTypeDefinition ()
                = typedefof<InterfaceGraphType<_>>
        then Some (``interface``, interfaceType.GenericTypeArguments.[0])
        else None
    | _ -> None

let (|ObjectGraphType|_|) (``type``: IGraphType) =
    match ``type`` with
    | :? IObjectGraphType as object ->
        let objectType = object.GetType ()
        if objectType.IsGenericType
            && objectType.GetGenericTypeDefinition ()
                = typedefof<ObjectGraphType<_>>
        then Some (object, objectType.GenericTypeArguments.[0])
        else None
    | _ -> None
