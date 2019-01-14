[<AutoOpen>]
module GraphQL.FSharp.Builder.Enum

open GraphQL.Types

let inline private set f (x: EnumerationGraphType) = f x; x

let makeEnumValue name value =
    let enumValue = EnumValueDefinition ()
    enumValue.Name <- name
    enumValue.Value <- box value
    enumValue

type EnumBuilder() =
    inherit BuilderMetadataBase<EnumerationGraphType>()

    [<CustomOperation "cases">]
    member __.Cases (enum, values: _ list) =
        set (fun enum ->
            values
            |> List.map (fun (name, value) -> makeEnumValue name value)
            |> List.iter enum.AddValue) enum

let enum = EnumBuilder ()
