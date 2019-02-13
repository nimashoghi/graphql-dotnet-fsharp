[<AutoOpen>]
module GraphQL.FSharp.DefineEnumCase

open GraphQL.Types

type Define with
    static member EnumCase (name, value, ?description, ?deprecationReason) =
        if isNull name || name = ""
        then invalidArg "name" "name cannot be null or empty"

        EnumValueDefinition (
            Name = name,
            Value = box value,
            Description = Option.toObj description,
            DeprecationReason = Option.toObj deprecationReason
        )
