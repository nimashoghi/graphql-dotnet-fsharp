module GraphQL.FSharp.BuilderArgument

open GraphQL.FSharp.BuilderBase
open GraphQL.FSharp.Inference
open GraphQL.FSharp.Types

type ArgumentBuilderBase<'t> () =
    inherit TypedEntityBuilder<TypedQueryArgument<'t>> ()

type ArgumentBuilder<'t> (?``type``, ?value) =
    inherit ArgumentBuilderBase<'t> ()

    member __.Run (state: State<TypedQueryArgument<'t>>) =
        value
        |> Option.orElse (Option.map TypedQueryArgument<'t> ``type``)
        |> Option.defaultValue (TypedQueryArgument<'t> (createReference typeof<'t>))
        |> apply state

type ArgumentEditorBuilder<'t> () =
    inherit ArgumentBuilderBase<'t> ()

    member __.Run (state: State<TypedQueryArgument<'t>>) = apply state
