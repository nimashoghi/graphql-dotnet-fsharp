[<AutoOpen>]
module GraphQL.FSharp.DefineArgument

open GraphQL.FSharp.Inference
open GraphQL.FSharp.Types

type Define with
    static member Argument<'arg> (name, ?defaultValue: 'arg) =
        if isNull name || name = ""
        then invalidArg "name" "value cannot be null"

        let defaultValue =
            defaultValue
            |> Option.map box

        TypedQueryArgument<'arg> (
            createReference typeof<'arg>,
            Name = name,
            DefaultValue = Option.toObj defaultValue
        )
