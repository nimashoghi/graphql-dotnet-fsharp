module GraphQL.FSharp.BuilderInterface

open GraphQL.Types

open GraphQL.FSharp.BuilderBase
open GraphQL.FSharp.Types

// TODO: Should we remove the type parameter?
let inline private set f (x: #InterfaceGraphType<_>) = f x; x

type InterfaceBuilder<'source> () =
    member __.Yield (_: unit) = InterfaceGraphType<'source> ()

    [<CustomOperation "name">]
    member __.CustomOperation_Name (this: InterfaceGraphType<'source>, name) =
        this |> setName name

    [<CustomOperation "description">]
    member __.CustomOperation_Description (this: InterfaceGraphType<'source>, description) =
        this |> setDescription description

    [<CustomOperation "deprecationReason">]
    member __.CustomOperation_DeprecationReason (this: InterfaceGraphType<'source>, deprecationReason) =
        this |> setDeprecationReason deprecationReason

    [<CustomOperation "metadata">]
    member __.CustomOperation_Metadata (this: InterfaceGraphType<'source>, metadata) =
        this |> setMetadata metadata

    [<CustomOperation "fields">]
    member __.CustomOperation_Fields (object: InterfaceGraphType<'source>, fields: TypedFieldType<'source> list) =
        set (fun x ->
            fields
            |> List.iter (x.AddField >> ignore)) object
