module GraphQL.FSharp.BuilderEnum

open GraphQL.Types

open GraphQL.FSharp.BuilderBase

type EnumerationBuilderOperation = EnumerationGraphType -> EnumerationGraphType
type EnumerationBuilderState = EnumerationBuilderOperation list

type EnumerationBuilderBase () =
    inherit ConfigurableBuilder<EnumerationGraphType> ()

    member __.Yield (_: unit) = [] : EnumerationBuilderState

    [<CustomOperation "name">]
    member __.CustomOperation_Name (state: EnumerationBuilderState, name) =
        state
        |> operation (setName name)

    [<CustomOperation "description">]
    member __.CustomOperation_Description (state: EnumerationBuilderState, description) =
        state
        |> operation (setDescription description)

    [<CustomOperation "deprecationReason">]
    member __.CustomOperation_DeprecationReason (state: EnumerationBuilderState, deprecationReason) =
        state
        |> operation (setDeprecationReason deprecationReason)

    [<CustomOperation "metadata">]
    member __.CustomOperation_Metadata (state: EnumerationBuilderState, metadata) =
        state
        |> operation (setMetadata metadata)

    [<CustomOperation "cases">]
    member __.CustomOperation_Cases (state: EnumerationBuilderState, values) =
        state
        |> unitOperation (fun this ->
            values
            |> List.iter this.AddValue
        )

type EnumerationBuilder (?value) =
    inherit EnumerationBuilderBase ()

    member __.Run (state: EnumerationBuilderState) =
        value
        |> Option.defaultValue (EnumerationGraphType ())
        |> apply state

type EnumerationEditBuilder () =
    inherit EnumerationBuilderBase ()

    member __.Run (state: EnumerationBuilderState) = apply state
