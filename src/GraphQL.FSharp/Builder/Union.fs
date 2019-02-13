module GraphQL.FSharp.BuilderUnion

open GraphQL.Types

open GraphQL.FSharp.BuilderBase

type UnionBaseBuilder () =
    inherit BasicGraphTypeBuilder<UnionGraphType> ()

    [<CustomOperation "cases">]
    member __.Cases (state: State<UnionGraphType>, cases) =
        state
        |> unitOperation (fun this -> this.PossibleTypes <- List.toSeq cases)

type UnionBuilder (?value) =
    inherit UnionBaseBuilder ()

    member __.Run (state: State<UnionGraphType>) =
        value
        |> Option.defaultValue (UnionGraphType ())
        |> apply state

type UnionEditBuilder () =
    inherit UnionBaseBuilder ()

    member __.Run (state: State<UnionGraphType>) = apply state
