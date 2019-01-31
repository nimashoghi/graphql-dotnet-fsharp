[<AutoOpen>]
module GraphQL.FSharp.Builder.Schema

open System
open GraphQL.Types

let inline private set f (x: Schema) = f x; x

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

let abstractClasses ``type`` =
    let rec run (``type``: Type) = [
        let baseType = ``type``.BaseType
        if baseType <> null && baseType <> typeof<obj> && baseType.IsAbstract then
            yield baseType
            yield! run baseType
    ]
    run ``type``

let interfaces (``type``: Type) = [
    yield! ``type``.GetInterfaces ()
    yield! abstractClasses ``type``
]

let extends (object: Type) (``base``: Type) =
    interfaces object
    |> List.exists ((=) ``base``)

let handleInterfaces (types: IGraphType list) =
    let objects = types |> List.choose (|ObjectGraphType|_|)
    let interfaces = types |> List.choose (|InterfaceGraphType|_|)
    (objects, interfaces)
    ||> List.allPairs
    |> List.filter (fun ((_, object), (_, ``interface``)) -> extends object ``interface``)
    |> List.iter (fun ((object, _), (``interface``, _)) ->
        ``interface``.AddPossibleType object
        object.AddResolvedInterface ``interface``)

    types

type SchemaBuilder () =
    member __.Yield _ = new Schema ()

    [<CustomOperation "query">]
    member __.Query (schema, query: Query) =
        set (fun schema -> schema.Query <- query) schema

    [<CustomOperation "mutation">]
    member __.Mutation (schema, mutation: Mutation) =
        set (fun schema -> schema.Mutation <- mutation) schema

    [<CustomOperation "subscription">]
    member __.Subscription (schema, subscription: Subscription) =
        set (fun schema -> schema.Subscription <- subscription) schema

    [<CustomOperation "types">]
    member __.Types (schema, types: IGraphType list) =
        set (fun schema ->
            types
            |> handleInterfaces
            |> List.toArray
            |> schema.RegisterTypes) schema

    member __.Run (schema: Schema) =
        schema.Initialize ()
        schema

let schema = SchemaBuilder ()
