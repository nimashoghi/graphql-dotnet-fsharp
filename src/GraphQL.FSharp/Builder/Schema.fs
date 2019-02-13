module GraphQL.FSharp.BuilderSchema

open System
open GraphQL.Types

open GraphQL.FSharp.BuilderBase
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

type SchemaBuilderOperation = Schema -> Schema
type SchemaBuilderState = SchemaBuilderOperation list

type SchemaBuilderBase () =
    inherit ConfigurableBuilder<Schema> ()

    member __.Yield (_: unit) = [] : SchemaBuilderState

    [<CustomOperation "query">]
    member __.CustomOperation_Query (state: SchemaBuilderState, query: Query) =
        state
        |> unitOperation (fun this -> this.Query <- query)

    [<CustomOperation "mutation">]
    member __.CustomOperation_Mutation (state: SchemaBuilderState, mutation: Mutation) =
        state
        |> unitOperation (fun this -> this.Mutation <- mutation)

    [<CustomOperation "subscription">]
    member __.CustomOperation_Subscription (state: SchemaBuilderState, subscription: Subscription) =
        state
        |> unitOperation (fun this -> this.Subscription <- subscription)

    [<CustomOperation "types">]
    member __.CustomOperation_Types (state: SchemaBuilderState, types: IGraphType list) =
        state
        |> unitOperation (fun this ->
            types
            |> handleInterfaces
            |> List.toArray
            |> this.RegisterTypes
        )

type SchemaBuilder (?value) =
    inherit SchemaBuilderBase ()

    member __.Run (state: SchemaBuilderState) =
        value
        |> Option.defaultValue (new Schema ())
        |> apply state

type SchemaEditBuilder () =
    inherit SchemaBuilderBase ()

    member __.Run (state: SchemaBuilderState) = apply state
