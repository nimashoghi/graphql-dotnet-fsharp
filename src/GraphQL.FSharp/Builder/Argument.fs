module GraphQL.FSharp.BuilderArgument

open GraphQL.Types

open GraphQL.FSharp.BuilderBase
open GraphQL.FSharp.Inference
open GraphQL.FSharp.Types

// TODO: Fix problem with FSharp list type as an argument

type ArgumentBuilderOperation<'t> = TypedQueryArgument<'t> -> TypedQueryArgument<'t>
type ArgumentBuilderState<'t> = ArgumentBuilderOperation<'t> list

type ArgumentBuilderBase<'t> () =
    inherit ConfigurableBuilder<TypedQueryArgument<'t>> ()

    member __.Yield (_: unit) = [] : ArgumentBuilderState<'t>

    [<CustomOperation "name">]
    member __.CustomOperation_Name (state: ArgumentBuilderState<'t>, name) =
        operation (setName name) state

    [<CustomOperation "description">]
    member __.CustomOperation_Description (state: ArgumentBuilderState<'t>, description) =
        operation (setDescription description) state

    [<CustomOperation "defaultValue">]
    member __.CustomOperation_DefaultValue (state: ArgumentBuilderState<'t>, value: 't) =
        operation (setDefaultValue value) state

    [<CustomOperation "type">]
    member __.CustomOperation_Type (state: ArgumentBuilderState<'t>, ``type``) =
        operation (setResolvedType ``type``) state
    member __.CustomOperation_Type (state: ArgumentBuilderState<'t>, ``type``) =
        operation (fun this -> this |> setResolvedType (``type`` ())) state

type ArgumentBuilder<'t> (?``type``, ?value) =
    inherit ArgumentBuilderBase<'t> ()

    member __.Run (state: ArgumentBuilderState<'t>) =
        value
        |> Option.orElse (Option.map TypedQueryArgument<'t> ``type``)
        |> Option.defaultValue (TypedQueryArgument<'t> (createReference typeof<'t>))
        |> apply state

type ArgumentEditorBuilder<'t> () =
    inherit ArgumentBuilderBase<'t> ()

    member __.Run (state: ArgumentBuilderState<'t>) = apply state
