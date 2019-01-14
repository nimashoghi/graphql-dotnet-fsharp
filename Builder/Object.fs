[<AutoOpen>]
module GraphQL.FSharp.Builder.Object

open GraphQL.Types

open GraphQL.FSharp.Util

let inline private set f (x: ObjectGraphType<_>) = f x; x

type ObjectBuilder<'source>(?name) =
    inherit BuilderMetadataBase<ObjectGraphType<'source>>()

    [<CustomOperation "fields">]
    member __.Fields (object: ObjectGraphType<'source>, fields: TypedFieldType<'source> list) =
        set (fun x ->
            fields
            |> List.iter (x.AddField >> ignore)) object

    [<CustomOperation "implement">]
    member __.Implement (object: ObjectGraphType<'source>, ``interface``) =
        set (fun x -> x.AddResolvedInterface ``interface``) object

    member __.Run (object: ObjectGraphType<'source>) =
        match name with
        | Some name ->
            set (fun x -> x.Name <- name) object
        | None when typeof<'source> <> typeof<unit> ->
            set (fun x -> x.Name <- typeof<'source>.Name) object
        | None -> object

let object<'source> = ObjectBuilder<'source> ()
let query = ObjectBuilder<unit> "Query"
let mutation = ObjectBuilder<unit> "Mutation"
let subscription = ObjectBuilder<unit> "Subscription"
