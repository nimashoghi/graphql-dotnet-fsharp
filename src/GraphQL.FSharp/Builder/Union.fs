[<AutoOpen>]
module GraphQL.FSharp.Builder.Union

open GraphQL.Types

let inline private set f (x: UnionGraphType) = f x; x

type UnionBuilder () =
    inherit BuilderMetadataBase<UnionGraphType> ()

    [<CustomOperation "cases">]
    member __.Cases (union, cases: IObjectGraphType list) =
        set (fun union -> union.PossibleTypes <- cases) union

let union = UnionBuilder ()
