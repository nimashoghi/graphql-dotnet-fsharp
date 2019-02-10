module GraphQL.FSharp.BuilderInputObject

open GraphQL.Types

open GraphQL.FSharp.BuilderBase
open GraphQL.FSharp.Types

let inline private set f (x: #InputObjectGraphType<_>) = f x; x

type InputObjectBuilder<'source> () =
    member __.Yield (_: unit) = InputObjectGraphType<'source> ()

    [<CustomOperation "name">]
    member __.CustomOperation_Name (this: InputObjectGraphType<'source>, name) =
        this |> setName name

    [<CustomOperation "description">]
    member __.CustomOperation_Description (this: InputObjectGraphType<'source>, description) =
        this |> setDescription description

    [<CustomOperation "deprecationReason">]
    member __.CustomOperation_DeprecationReason (this: InputObjectGraphType<'source>, deprecationReason) =
        this |> setDeprecationReason deprecationReason

    [<CustomOperation "metadata">]
    member __.CustomOperation_Metadata (this: InputObjectGraphType<'source>, metadata) =
        this |> setMetadata metadata

    [<CustomOperation "fields">]
    member __.CustomOperation_Fields (object: InputObjectGraphType<'source>, fields: TypedFieldType<'source> list) =
        set (fun x ->
            fields
            |> List.iter (x.AddField >> ignore)
        ) object
