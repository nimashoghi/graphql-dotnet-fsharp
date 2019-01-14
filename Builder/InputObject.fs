[<AutoOpen>]
module GraphQL.FSharp.Builder.InputObject

open GraphQL.Types

open GraphQL.FSharp
open GraphQL.FSharp.Util

let inline private set f (x: InputObjectGraphType<_>) = f x; x

type InputObjectBuilder<'source>() =
    inherit BuilderMetadataBase<InputObjectGraphType<'source>>()

    [<CustomOperation "fields">]
    member __.Fields (object: InputObjectGraphType<'source>, fields: TypedFieldType<'source> list) =
        set (fun x ->
            fields
            |> List.iter (x.AddField >> ignore)) object

let input<'source> = InputObjectBuilder<'source> ()
