module GraphQL.FSharp.BuilderObject

open GraphQL.Types

open GraphQL.FSharp.BuilderBase
open GraphQL.FSharp.Types

type ObjectBuilder<'source> (?value) =
    inherit ComplexBuilder<'source> ()

    // TODO: Make it so we return a new object here
    member __.Yield (_: unit) =
        value
        |> Option.defaultValue (ObjectGraphType<'source> ())

    [<CustomOperation "name">]
    member __.CustomOperation_Name (this: ObjectGraphType<'source>, name) =
        setName name this

    [<CustomOperation "description">]
    member __.CustomOperation_Description (this: ObjectGraphType<'source>, description) =
        setDescription description this

    [<CustomOperation "deprecationReason">]
    member __.CustomOperation_DeprecationReason (this: ObjectGraphType<'source>, deprecationReason) =
        setDeprecationReason deprecationReason this

    [<CustomOperation "metadata">]
    member __.CustomOperation_Metadata (this: ObjectGraphType<'source>, metadata) =
        setMetadata metadata this

    [<CustomOperation "fields">]
    member __.CustomOperation_Fields (this: ObjectGraphType<'source>, fields: TypedFieldType<'source> list) =
        fields
        |> List.iter (this.AddField >> ignore)

        this

    [<CustomOperation "interfaces">]
    member __.CustomOperation_Interfaces (this: ObjectGraphType<'source>, ``interface``) =
        ``interface``
        |> List.iter this.AddResolvedInterface

        this
