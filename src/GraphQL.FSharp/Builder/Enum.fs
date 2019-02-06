[<AutoOpen>]
module GraphQL.FSharp.Builder.Enum

open GraphQL.Types

let inline private set f (x: EnumerationGraphType) = f x; x

type EnumerationGraphType with
    member this.Yield (_: unit) = this

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
    member __.CustomOperation_Cases (enum, values) =
        set (fun enum ->
            values
            |> List.iter (fun (name, value) -> enum.AddValue (name, null, box value))
        ) enum

let enum = EnumerationGraphType ()
