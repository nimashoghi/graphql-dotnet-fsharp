module GraphQL.FSharp.BuilderEnum

open GraphQL.Types

open GraphQL.FSharp.BuilderBase

type EnumerationBuilder (?value) =
    member __.Yield (_: unit) =
        value
        |> Option.defaultValue (EnumerationGraphType ())


    [<CustomOperation "name">]
    member __.CustomOperation_Name (this: EnumerationGraphType, name) =
        setName name this

    [<CustomOperation "description">]
    member __.CustomOperation_Description (this: EnumerationGraphType, description) =
        setDescription description this

    [<CustomOperation "deprecationReason">]
    member __.CustomOperation_DeprecationReason (this: EnumerationGraphType, deprecationReason) =
        setDeprecationReason deprecationReason this

    [<CustomOperation "metadata">]
    member __.CustomOperation_Metadata (this: EnumerationGraphType, metadata) =
        setMetadata metadata this

    [<CustomOperation "cases">]
    member __.CustomOperation_Cases (this: EnumerationGraphType, values) =
        values
        |> List.iter this.AddValue

        this
