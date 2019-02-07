[<AutoOpen>]
module GraphQL.FSharp.DefineEnumCase

open GraphQL.Types

// TODO: test this

type Define with
    static member EnumCase (name, value, ?description, ?deprecationReason) =
        if isNull name || name = ""
        then invalidArg "name" "name cannot be null or empty"

        if isNull value
        then invalidArg "value" "value cannot be null"

        let enumValue = EnumValueDefinition ()

        enumValue.Name <- name
        enumValue.Value <- box value
        description |> Option.iter (fun description -> enumValue.Description <- description)
        deprecationReason |> Option.iter (fun deprecationReason -> enumValue.DeprecationReason <- deprecationReason)

        enumValue
