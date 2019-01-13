[<AutoOpen>]
module GraphQL.FSharp.Types.Argument

open GraphQL.Types

type Define with
    static member Argument (name, ``type``: IGraphType, ?defaultValue, ?description) =
        let argument = QueryArgument ``type`` |> setNameDescription name description
        Option.iter (fun defaultValue -> argument.DefaultValue <- box defaultValue) defaultValue
        argument
