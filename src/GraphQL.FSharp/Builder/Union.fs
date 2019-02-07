[<AutoOpen>]
module GraphQL.FSharp.Builder.Union

open GraphQL.Types

open GraphQL.FSharp.Builder.Base

let inline private set f (x: #UnionGraphType) = f x; x

type UnionGraphType with
    member this.Yield (_: unit) = ``yield`` this

    [<CustomOperation "name">]
    member __.CustomOperation_Name (this: UnionGraphType, name) =
        this |> setName name

    [<CustomOperation "description">]
    member __.CustomOperation_Description (this: UnionGraphType, description) =
        this |> setDescription description

    [<CustomOperation "deprecationReason">]
    member __.CustomOperation_DeprecationReason (this: UnionGraphType, deprecationReason) =
        this |> setDeprecationReason deprecationReason

    [<CustomOperation "metadata">]
    member __.CustomOperation_Metadata (this: UnionGraphType, metadata) =
        this |> setMetadata metadata

    [<CustomOperation "cases">]
    member __.CustomOperation_Cases (union, cases: IObjectGraphType list) =
        set (fun union -> union.PossibleTypes <- cases) union

let union = builder (fun () -> UnionGraphType ())
