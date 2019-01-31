[<AutoOpen>]
module GraphQL.FSharp.Builder.Object

open GraphQL.Types

open GraphQL.FSharp.Types

type Query = ObjectGraphType<obj>
type Mutation = ObjectGraphType<obj>
type Subscription = ObjectGraphType<obj>

let inline private set f (x: ObjectGraphType<_>) = f x; x

type ObjectBuilder<'source> () =
    inherit BuilderMetadataBase<ObjectGraphType<'source>> ()

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

let object<'source> = ObjectBuilder<'source> ()
let query lst : Query =
    object<obj> {
        name "Query"
        fields lst
    }
let mutation lst : Mutation =
    object<obj> {
        name "Mutation"
        fields lst
    }
let subscription lst : Subscription =
    object<obj> {
        name "Subscription"
        fields lst
    }
