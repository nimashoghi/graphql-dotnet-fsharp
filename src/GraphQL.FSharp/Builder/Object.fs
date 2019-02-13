module GraphQL.FSharp.BuilderObject

open GraphQL.Types

open GraphQL.FSharp.BuilderBase
open GraphQL.FSharp.Types

type ObjectBuilderOperation<'source> = ObjectGraphType<'source> -> ObjectGraphType<'source>
type ObjectBuilderState<'source> = ObjectBuilderOperation<'source> list

type ObjectBuilderBase<'source> () =
    inherit ComplexBuilder<ObjectGraphType<'source>> ()

    // TODO: Make it so we return a new object here
    member __.Yield (_: unit) = [] : ObjectBuilderState<'source>

    [<CustomOperation "name">]
    member __.CustomOperation_Name (state: ObjectBuilderState<'source>, name) =
        state
        |> operation (setName name)

    [<CustomOperation "description">]
    member __.CustomOperation_Description (state: ObjectBuilderState<'source>, description) =
        state
        |> operation (setDescription description)

    [<CustomOperation "deprecationReason">]
    member __.CustomOperation_DeprecationReason (state: ObjectBuilderState<'source>, deprecationReason) =
        state
        |> operation (setDeprecationReason deprecationReason)

    [<CustomOperation "metadata">]
    member __.CustomOperation_Metadata (state: ObjectBuilderState<'source>, metadata) =
        state
        |> operation (setMetadata metadata)

    [<CustomOperation "fields">]
    member __.CustomOperation_Fields (state: ObjectBuilderState<'source>, fields: TypedFieldType<'source> list) =
        state
        |> unitOperation (fun this ->
            fields
            |> List.iter (this.AddField >> ignore)
        )

    [<CustomOperation "interfaces">]
    member __.CustomOperation_Interfaces (state: ObjectBuilderState<'source>, ``interface``) =
        state
        |> unitOperation (fun this ->
            ``interface``
            |> List.iter this.AddResolvedInterface
        )

type ObjectBuilder<'source> (?value) =
    inherit ObjectBuilderBase<'source> ()

    member __.Run (state: ObjectBuilderState<'source>) =
        value
        |> Option.defaultValue (ObjectGraphType<'source> ())
        |> apply state

type ObjectEditBuilder<'source> () =
    inherit ObjectBuilderBase<'source> ()

    member __.Run (state: ObjectBuilderState<'source>) = apply state
