[<AutoOpen>]
module GraphQL.FSharp.DefineArgument

open GraphQL.Types

open GraphQL.FSharp.Inference

type Define with
    static member Argument<'arg> (name, ?defaultValue: 'arg) =
        if isNull name || name = ""
        then invalidArg "name" "value cannot be null"

        // TODO: Add test for NonNull checks
        let arg =
            createReference typeof<'arg>
            |> QueryArgument

        arg.Name <- name

        defaultValue
        |> Option.map box
        |> Option.iter (fun defaultValue -> arg.DefaultValue <- defaultValue)

        arg
