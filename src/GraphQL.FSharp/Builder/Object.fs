[<AutoOpen>]
module GraphQL.FSharp.Builder.Object

open GraphQL.Types

open GraphQL.FSharp.Builder.Base
open GraphQL.FSharp.Types

let inline private set f (x: #ObjectGraphType<_>) = f x; x

type ObjectGraphType<'source> with
    member this.Yield (_: unit) = ``yield`` this

    [<CustomOperation "name">]
    member __.CustomOperation_Name (this: ObjectGraphType<'source>, name) =
        this |> setName name

    [<CustomOperation "description">]
    member __.CustomOperation_Description (this: ObjectGraphType<'source>, description) =
        this |> setDescription description

    [<CustomOperation "deprecationReason">]
    member __.CustomOperation_DeprecationReason (this: ObjectGraphType<'source>, deprecationReason) =
        this |> setDeprecationReason deprecationReason

    [<CustomOperation "metadata">]
    member __.CustomOperation_Metadata (this: ObjectGraphType<'source>, metadata) =
        this |> setMetadata metadata

    [<CustomOperation "fields">]
    member __.CustomOperation_Fields (object: ObjectGraphType<'source>, fields: TypedFieldType<'source> list) =
        set (fun x ->
            fields
            |> List.iter (x.AddField >> ignore)) object

    [<CustomOperation "interfaces">]
    member __.CustomOperation_Interfaces (object: ObjectGraphType<'source>, ``interface``) =
        set (fun x ->
            ``interface``
            |> List.iter x.AddResolvedInterface) object

let object<'source> = builder (fun () -> ObjectGraphType<'source> ())
let query lst: Query =  builder (fun () ->
    object<obj> {
        name "Query"
        fields lst
    }
)
let mutation lst: Mutation =  builder (fun () ->
    object<obj> {
        name "Mutation"
        fields lst
    }
)
let subscription lst: Subscription =  builder (fun () ->
    object<obj> {
        name "Subscription"
        fields lst
    }
)
