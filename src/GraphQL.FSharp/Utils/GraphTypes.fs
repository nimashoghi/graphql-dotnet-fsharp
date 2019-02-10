[<AutoOpen>]
module GraphQL.FSharp.Utils.GraphTypes

open GraphQL.Types

let graphTypeNameConfig detailed (x: #IGraphType) =
    let rec run (x: IGraphType) =
        match x with
        | :? GraphQLTypeReference as x ->
            if detailed
            then sprintf "(%s ref)" x.TypeName
            else x.TypeName
        | :? NonNullGraphType as x ->
            x.ResolvedType
            |> run
            |> sprintf "%s!"
        | :? ListGraphType as x ->
            x.ResolvedType
            |> run
            |> sprintf "[%s]"
        | x -> sprintf "%s" x.Name
    run (x :> IGraphType)

let graphTypeName x = graphTypeNameConfig false x
let graphTypeNameDetailed x = graphTypeNameConfig true x
