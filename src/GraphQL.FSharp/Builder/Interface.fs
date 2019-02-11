module GraphQL.FSharp.BuilderInterface

open GraphQL.Types

open GraphQL.FSharp.BuilderBase
open GraphQL.FSharp.Types

// TODO: Should we remove the type parameter?
type InterfaceBuilder<'source> (?value) =
    member __.Yield (_: unit) =
        value
        |> Option.defaultValue (InterfaceGraphType<'source> ())

    [<CustomOperation "name">]
    member __.CustomOperation_Name (this: InterfaceGraphType<'source>, name) =
        setName name this

    [<CustomOperation "description">]
    member __.CustomOperation_Description (this: InterfaceGraphType<'source>, description) =
        setDescription description this

    [<CustomOperation "deprecationReason">]
    member __.CustomOperation_DeprecationReason (this: InterfaceGraphType<'source>, deprecationReason) =
        setDeprecationReason deprecationReason this

    [<CustomOperation "metadata">]
    member __.CustomOperation_Metadata (this: InterfaceGraphType<'source>, metadata) =
        setMetadata metadata this

    [<CustomOperation "fields">]
    member __.CustomOperation_Fields (this: InterfaceGraphType<'source>, fields: TypedFieldType<'source> list) =
        fields
        |> List.iter (this.AddField >> ignore)

        this
