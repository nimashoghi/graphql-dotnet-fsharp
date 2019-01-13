module GraphQL.FSharp.Builder.Enum

open GraphQL.Types

let inline private set f (x: EnumerationGraphType) = f x; x

type EnumValueBuilder() =
    inherit BuilderDeprecationReasonBase<EnumValueDefinition>()


type EnumBuilder() =
    inherit BuilderMetadataBase<EnumerationGraphType>()

    [<CustomOperation "cases">]
    member __.Cases (enum, values: _ list) =
        set (fun enum -> values |> List.map (fun value -> EnumValueDefinition()) |> List.iter enum.AddValue) enum

let enum = EnumBuilder ()
