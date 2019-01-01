module GraphQL.FSharp.Builder.Schema

open System
open GraphQL
open GraphQL.Types
open Iris.Option.Builders

open GraphQL.FSharp.Model

type IServiceProvider with
    /// **Description**
    ///   * Creates a new `IDependencyResolver` from this `IServiceProvider`.
    member this.ToDependencyResolver () = {
        new IDependencyResolver with
            member __.Resolve<'t> () = this.GetService typeof<'t> :?> 't
            member __.Resolve t = this.GetService t
    }

/// **Description**
///  * Creates a new SchemaInfo from the provided IServiceProvider
let create (provider: IServiceProvider) = new SchemaInfo(provider.ToDependencyResolver())

type SchemaWrapper = {
    Query: (SchemaInfo -> IObjectGraphType) option
    Mutation: (SchemaInfo -> IObjectGraphType) option
    Subscription: (SchemaInfo -> IObjectGraphType) option
    Types: (SchemaInfo -> IGraphType) list
}

/// **Description**
///   * Creates an empty SchemaInfo
let newSchema = {
    Query = None
    Mutation = None
    Subscription = None
    Types = []
}

// TODO: This is a hack, fix
let internal composeObjectGraphType (x: IGraphType) = x :?> IObjectGraphType

type SchemaBuilder() =
    /// **Description**
    ///   * Creates a new SchemaInfo
    member __.Yield _ = newSchema

    /// **Description**
    ///   * Registers the provided `Query` object.
    [<CustomOperation "query">]
    member __.Query (schema, query) = {schema with Query = Some (query >> composeObjectGraphType)}

    /// **Description**
    ///   * Registers the provided `Mutation` object.
    [<CustomOperation "mutation">]
    member __.Mutation (schema, mutation) = {schema with Mutation = Some (mutation >> composeObjectGraphType)}

    /// **Description**
    ///   * Registers the provided `Subscription` object.
    [<CustomOperation "subscription">]
    member __.Subscription (schema, subscription) = {schema with Subscription = Some (subscription >> composeObjectGraphType)}

    /// **Description**
    ///   * Registers the provided types.
    [<CustomOperation "types">]
    member __.Types (schema, types: (SchemaInfo -> IGraphType) list) =
        // TODO: Clean this up
        {schema with Types = schema.Types @ List.map (fun f -> f >> (fun graphType -> graphType :> IGraphType)) types}

    /// **Description**
    ///   * Processes the `schema` expressions' `SchemaInfo`.
    member __.Run {Query = query; Mutation = mutation; Subscription = subscription; Types = types} =
        fun (provider: IServiceProvider) ->
            let schema = new SchemaInfo(provider.ToDependencyResolver())

            for ``type`` in types do
                schema.WithType (``type`` schema)

            let mutable types = []

            maybeUnit {
                let! query = query
                let query = query schema
                schema.Query <- query
                types <- query :> IGraphType :: types
            }

            maybeUnit {
                let! mutation = mutation
                let mutation = mutation schema
                schema.Mutation <- mutation
                types <- mutation :> IGraphType :: types
            }

            maybeUnit {
                let! subscription = subscription
                let subscription = subscription schema
                schema.Subscription <- subscription
                types <- subscription :> IGraphType :: types
            }

            types
            |> List.toArray
            |> schema.WithTypes
            // TODO: Add custom register method to query registered types

            schema
