[<AutoOpen>]
module GraphQL.FSharp.Builder.Interface

open GraphQL.Types

open GraphQL.FSharp.Types

let inline private set f (x: InterfaceGraphType<_>) = f x; x

type InterfaceBuilder<'source>() =
    inherit BuilderMetadataBase<InterfaceGraphType<'source>>()

    [<CustomOperation "fields">]
    member __.Fields (object: InterfaceGraphType<'source>, fields: TypedFieldType<'source> list) =
        set (fun x ->
            fields
            |> List.iter (x.AddField >> ignore)) object

// TODO: rename?
let ``interface``<'source> = InterfaceBuilder<'source> ()
