module GraphQL.FSharp.BuilderArgument

open GraphQL.Types

open GraphQL.FSharp.BuilderBase
open GraphQL.FSharp.Inference
open GraphQL.FSharp.Types

// TODO: Fix problem with FSharp list type as an argument

let inline private set f (x: #QueryArgument) = f x; x

type ArgumentBuilder<'t> (?``type``) =
    member __.Yield (_: unit) =
        let ``type`` =
            ``type``
            |> Option.defaultValue (createReference typeof<'t>)
        TypedQueryArgument<'t> ``type``

    [<CustomOperation "name">]
    member __.CustomOperation_Name (this: TypedQueryArgument<'t>, name) =
        setName name this

    [<CustomOperation "description">]
    member __.CustomOperation_Description (this: TypedQueryArgument<'t>, description) =
        setDescription description this

    [<CustomOperation "defaultValue">]
    member __.CustomOperation_DefaultValue (this: TypedQueryArgument<'t>, value: 't) =
        setDefaultValue value this

    [<CustomOperation "typed">]
    member __.CustomOperation_Type (this: TypedQueryArgument<'t>, ``type``) =
        setResolvedType ``type`` this
