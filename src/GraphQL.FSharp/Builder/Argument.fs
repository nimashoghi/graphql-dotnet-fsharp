module GraphQL.FSharp.BuilderArgument

open GraphQL.Types

open GraphQL.FSharp.BuilderBase
open GraphQL.FSharp.Inference
open GraphQL.FSharp.Types

// TODO: Fix problem with FSharp list type as an argument

type ArgumentBuilder<'t> (?``type``, ?value) =
    member __.Yield (_: unit) =
        value
        |> Option.orElse (Option.map TypedQueryArgument<'t> ``type``)
        |> Option.defaultValue (TypedQueryArgument<'t> (createReference typeof<'t>))

    [<CustomOperation "name">]
    member __.CustomOperation_Name (this: TypedQueryArgument<'t>, name) =
        setName name this

    [<CustomOperation "description">]
    member __.CustomOperation_Description (this: TypedQueryArgument<'t>, description) =
        setDescription description this

    [<CustomOperation "defaultValue">]
    member __.CustomOperation_DefaultValue (this: TypedQueryArgument<'t>, value: 't) =
        setDefaultValue value this

    [<CustomOperation "type">]
    member __.CustomOperation_Type (this: TypedQueryArgument<'t>, ``type``) =
        setResolvedType ``type`` this
