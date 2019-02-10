module GraphQL.FSharp.BuilderEnum

open GraphQL.Types

open GraphQL.FSharp.BuilderBase

let inline private set f (x: #EnumerationGraphType) = f x; x

type EnumerationBuilder () =
    member __.Yield (_: unit) = EnumerationGraphType ()

    [<CustomOperation "name">]
    member __.CustomOperation_Name (this: EnumerationGraphType, name) =
        this |> setName name

    [<CustomOperation "description">]
    member __.CustomOperation_Description (this: EnumerationGraphType, description) =
        this |> setDescription description

    [<CustomOperation "deprecationReason">]
    member __.CustomOperation_DeprecationReason (this: EnumerationGraphType, deprecationReason) =
        this |> setDeprecationReason deprecationReason

    [<CustomOperation "metadata">]
    member __.CustomOperation_Metadata (this: EnumerationGraphType, metadata) =
        this |> setMetadata metadata

    [<CustomOperation "cases">]
    member __.CustomOperation_Cases (this: EnumerationGraphType, values) =
        set (fun this ->
            for value in values do this.AddValue value
        ) this
