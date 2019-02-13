module GraphQL.FSharp.BuilderInterface

open GraphQL.Types

open GraphQL.FSharp.BuilderBase
open GraphQL.FSharp.Types

type InterfaceBuilderOperation<'source> = InterfaceGraphType<'source> -> InterfaceGraphType<'source>
type InterfaceBuilderState<'source> = InterfaceBuilderOperation<'source> list

type InterfaceBuilderBase<'source> () =
    inherit ComplexBuilder<InterfaceGraphType<'source>> ()

    member __.Yield (_: unit) = [] : InterfaceBuilderState<'source>

    [<CustomOperation "name">]
    member __.CustomOperation_Name (state: InterfaceBuilderState<'source>, name) =
        state
        |> operation (setName name)

    [<CustomOperation "description">]
    member __.CustomOperation_Description (state: InterfaceBuilderState<'source>, description) =
        state
        |> operation (setDescription description)

    [<CustomOperation "deprecationReason">]
    member __.CustomOperation_DeprecationReason (state: InterfaceBuilderState<'source>, deprecationReason) =
        state
        |> operation (setDeprecationReason deprecationReason)

    [<CustomOperation "metadata">]
    member __.CustomOperation_Metadata (state: InterfaceBuilderState<'source>, metadata) =
        state
        |> operation (setMetadata metadata)

    [<CustomOperation "fields">]
    member __.CustomOperation_Fields (state: InterfaceBuilderState<'source>, fields: TypedFieldType<'source> list) =
        state
        |> unitOperation (fun this ->
            fields
            |> List.iter (this.AddField >> ignore)
        )

type InterfaceBuilder<'source> (?value) =
    inherit InterfaceBuilderBase<'source> ()

    member __.Run (state: InterfaceBuilderState<'source>) =
        value
        |> Option.defaultValue (InterfaceGraphType<'source> ())
        |> apply state

type InterfaceEditBuilder<'source> () =
    inherit InterfaceBuilderBase<'source> ()

    member __.Run (state: InterfaceBuilderState<'source>) = apply state
