module GraphQL.FSharp.BuilderUnion

open GraphQL.Types

open GraphQL.FSharp.BuilderBase

let inline private set f (x: #UnionGraphType) = f x; x

type UnionBuilder () =
    member __.Yield (_: unit) = UnionGraphType ()

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
