[<AutoOpen>]
module GraphQL.FSharp.Types.Enum

open GraphQL.Types

type Define with
    static member EnumValue (name, value, ?description, ?deprecationReason) =
        let enumValue = EnumValueDefinition () |> setBasicProps name description deprecationReason
        enumValue.Value <- box value
        enumValue

    static member Enum (name, cases, ?description, ?deprecationReason) =
        let ``type`` = EnumerationGraphType () |> setBasicProps name description deprecationReason
        for case in cases do ``type``.AddValue case
        ``type``
