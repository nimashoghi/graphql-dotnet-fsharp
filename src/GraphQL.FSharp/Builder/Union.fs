module GraphQL.FSharp.BuilderUnion

open GraphQL.Types

open GraphQL.FSharp.BuilderBase

type UnionBuilderOperation = UnionGraphType -> UnionGraphType
type UnionBuilderState = UnionBuilderOperation list

type UnionBaseBuilder () =
    inherit ConfigurableBuilder<UnionGraphType> ()

    member __.Yield (_: unit) = [] : UnionBuilderState

    [<CustomOperation "name">]
    member __.CustomOperation_Name (state: UnionBuilderState, name) =
        state
        |> operation (setName name)

    [<CustomOperation "description">]
    member __.CustomOperation_Description (state: UnionBuilderState, description) =
        state
        |> operation (setDescription description)

    [<CustomOperation "deprecationReason">]
    member __.CustomOperation_DeprecationReason (state: UnionBuilderState, deprecationReason) =
        state
        |> operation (setDeprecationReason deprecationReason)

    [<CustomOperation "metadata">]
    member __.CustomOperation_Metadata (state: UnionBuilderState, metadata) =
        state
        |> operation (setMetadata metadata)

    [<CustomOperation "cases">]
    member __.CustomOperation_Cases (state: UnionBuilderState, cases: IObjectGraphType list) =
        state
        |> unitOperation (fun this -> this.PossibleTypes <- cases)

type UnionBuilder (?value) =
    inherit UnionBaseBuilder ()

    member __.Run (state: UnionBuilderState) =
        value
        |> Option.defaultValue (UnionGraphType ())
        |> apply state

type UnionEditBuilder () =
    inherit UnionBaseBuilder ()

    member __.Run (state: UnionBuilderState) = apply state
