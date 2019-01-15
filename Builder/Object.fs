[<AutoOpen>]
module GraphQL.FSharp.Builder.Object

open GraphQL.Types

open GraphQL.FSharp
open GraphQL.FSharp.Util

let inline private set f (x: ObjectGraphType<_>) = f x; x

type ObjectBuilder<'source>(?name) =
    inherit BuilderMetadataBase<ObjectGraphType<'source>>()

    [<CustomOperation "fields">]
    member __.Fields (object: ObjectGraphType<'source>, fields: TypedFieldType<'source> list) =
        set (fun x ->
            fields
            |> List.iter (x.AddField >> ignore)) object

    [<CustomOperation "interfaces">]
    member __.Interfaces (object: ObjectGraphType<'source>, ``interface``) =
        set (fun x ->
            ``interface``
            |> List.iter x.AddResolvedInterface) object

    member __.Run (object: ObjectGraphType<'source>) =
        Option.iter (fun name -> object.Name <- name) name
        object

let object<'source> = ObjectBuilder<'source> ()
let query = ObjectBuilder<obj> "Query"
let mutation = ObjectBuilder<obj> "Mutation"
let subscription = ObjectBuilder<obj> "Subscription"
