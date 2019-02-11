[<AutoOpen>]
module GraphQL.FSharp.DefineArgument

open GraphQL.FSharp.Inference
open GraphQL.FSharp.Types

type Define with
    static member Argument<'arg> (name, ?defaultValue: 'arg) =
        if isNull name || name = ""
        then invalidArg "name" "value cannot be null"

        // TODO: Add test for NonNull checks
        let arg = TypedQueryArgument<'arg> (createReference typeof<'arg>)

        arg.Name <- name

        defaultValue
        |> Option.map box
        |> Option.iter (fun defaultValue -> arg.DefaultValue <- defaultValue)

        arg
