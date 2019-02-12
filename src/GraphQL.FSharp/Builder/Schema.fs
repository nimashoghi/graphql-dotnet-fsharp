module GraphQL.FSharp.BuilderSchema

open System
open GraphQL.Types

open GraphQL.FSharp.Types
open GraphQL.FSharp.Utils

let abstractClasses ``type`` =
    let rec run (``type``: Type) = [
        let baseType = ``type``.BaseType
        if baseType <> null && baseType <> typeof<obj> && baseType.IsAbstract then
            yield baseType
            yield! run baseType
    ]
    run ``type``

let baseTypes (``type``: Type) = [
    yield! ``type``.GetInterfaces ()
    yield! abstractClasses ``type``
]

let extends (object: Type) (``base``: Type) =
    baseTypes object
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

type SchemaBuilder (?value) =
    member __.Yield (_: unit) =
        value
        |> Option.defaultValue (new Schema ())

    [<CustomOperation "query">]
    member __.CustomOperation_Query (this: Schema, query: Query) =
        this.Query <- query
        this

    [<CustomOperation "mutation">]
    member __.CustomOperation_Mutation (this: Schema, mutation: Mutation) =
        this.Mutation <- mutation
        this

    [<CustomOperation "subscription">]
    member __.CustomOperation_Subscription (this: Schema, subscription: Subscription) =
        this.Subscription <- subscription
        this

    [<CustomOperation "types">]
    member __.CustomOperation_Types (this: Schema, types: IGraphType list) =
        types
        |> handleInterfaces
        |> List.toArray
        |> this.RegisterTypes

        this

    member __.Run (this: Schema) =
        this.Initialize ()
        this
