module GraphQL.FSharp.Builder.Schema

open GraphQL.Types

let inline private set f (x: Schema) = f x; x

type SchemaBuilder() =
    member __.Yield _ = new Schema()

    [<CustomOperation "query">]
    member __.Query (schema, query) =
        set (fun schema -> schema.Query <- query) schema

    [<CustomOperation "mutation">]
    member __.Mutation (schema, mutation) =
        set (fun schema -> schema.Mutation <- mutation) schema

    [<CustomOperation "subscription">]
    member __.Subscription (schema, subscription) =
        set (fun schema -> schema.Subscription <- subscription) schema

    [<CustomOperation "types">]
    member __.Types (schema, types: IGraphType list) =
        set (fun schema ->
            types
            |> List.toArray
            |> schema.RegisterTypes) schema

let schema = SchemaBuilder ()
