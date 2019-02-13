module GraphQL.FSharp.BuilderInputObject

open GraphQL.Types

open GraphQL.FSharp.BuilderBase
open GraphQL.FSharp.Types

type InputObjectBuilderOperation<'source> = InputObjectGraphType<'source> -> InputObjectGraphType<'source>
type InputObjectBuilderState<'source> = InputObjectBuilderOperation<'source> list

type InputObjectBuilderBase<'source> () =
    inherit ComplexBuilder<InputObjectGraphType<'source>> ()

    member __.Yield (_: unit) = [] : InputObjectBuilderState<'source>

    [<CustomOperation "name">]
    member __.CustomOperation_Name (state: InputObjectBuilderState<'source>, name) =
        state
        |> operation (setName name)

    [<CustomOperation "description">]
    member __.CustomOperation_Description (state: InputObjectBuilderState<'source>, description) =
        state
        |> operation (setDescription description)

    [<CustomOperation "deprecationReason">]
    member __.CustomOperation_DeprecationReason (state: InputObjectBuilderState<'source>, deprecationReason) =
        state
        |> operation (setDeprecationReason deprecationReason)

    [<CustomOperation "metadata">]
    member __.CustomOperation_Metadata (state: InputObjectBuilderState<'source>, metadata) =
        state
        |> operation (setMetadata metadata)

    [<CustomOperation "fields">]
    member __.CustomOperation_Fields (state: InputObjectBuilderState<'source>, fields: TypedFieldType<'source> list) =
        state
        |> unitOperation (fun this ->
            fields
            |> List.iter (this.AddField >> ignore)
        )

type InputObjectBuilder<'source> (?value) =
    inherit InputObjectBuilderBase<'source> ()

    member __.Run (state: InputObjectBuilderState<'source>) =
        value
        |> Option.defaultValue (InputObjectGraphType<'source> ())
        |> apply state

type InputObjectEditBuilder<'source> () =
    inherit InputObjectBuilderBase<'source> ()

    member __.Run (state: InputObjectBuilderState<'source>) = apply state
