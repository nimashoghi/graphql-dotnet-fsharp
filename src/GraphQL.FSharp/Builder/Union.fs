module GraphQL.FSharp.BuilderUnion

open GraphQL.Types

open GraphQL.FSharp.BuilderBase

type UnionBuilder (?value) =
    member __.Yield (_: unit) =
        value
        |> Option.defaultValue (UnionGraphType ())

    [<CustomOperation "name">]
    member __.CustomOperation_Name (this: UnionGraphType, name) =
        setName name this

    [<CustomOperation "description">]
    member __.CustomOperation_Description (this: UnionGraphType, description) =
        setDescription description this

    [<CustomOperation "deprecationReason">]
    member __.CustomOperation_DeprecationReason (this: UnionGraphType, deprecationReason) =
        setDeprecationReason deprecationReason this

    [<CustomOperation "metadata">]
    member __.CustomOperation_Metadata (this: UnionGraphType, metadata) =
        setMetadata metadata this

    [<CustomOperation "cases">]
    member __.CustomOperation_Cases (this: UnionGraphType, cases: IObjectGraphType list) =
        this.PossibleTypes <- cases

        this
