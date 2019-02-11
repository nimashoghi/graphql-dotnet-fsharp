module GraphQL.FSharp.BuilderInputObject

open GraphQL.Types

open GraphQL.FSharp.BuilderBase
open GraphQL.FSharp.Types

type InputObjectBuilder<'source> (?value) =
    member __.Yield (_: unit) =
        value
        |> Option.defaultValue (InputObjectGraphType<'source> ())

    [<CustomOperation "name">]
    member __.CustomOperation_Name (this: InputObjectGraphType<'source>, name) =
        setName name this

    [<CustomOperation "description">]
    member __.CustomOperation_Description (this: InputObjectGraphType<'source>, description) =
        setDescription description this

    [<CustomOperation "deprecationReason">]
    member __.CustomOperation_DeprecationReason (this: InputObjectGraphType<'source>, deprecationReason) =
        setDeprecationReason deprecationReason this

    [<CustomOperation "metadata">]
    member __.CustomOperation_Metadata (this: InputObjectGraphType<'source>, metadata) =
        setMetadata metadata this

    [<CustomOperation "fields">]
    member __.CustomOperation_Fields (this: InputObjectGraphType<'source>, fields: TypedFieldType<'source> list) =
        fields
        |> List.iter (this.AddField >> ignore)

        this
