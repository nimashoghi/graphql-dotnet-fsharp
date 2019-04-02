module GraphQL.FSharp.Builder.Utils

open System
open GraphQL.Types

open GraphQL.FSharp.Resolvers
open GraphQL.FSharp.Types

let withSource f (ctx: ResolveContext<_>) = f ctx.Source
let isInvalidType (``type``: IGraphType) = isNull ``type`` || Object.ReferenceEquals (``type``, invalidGraphType)

let inline makeNullable (x: ^t) =
    match Option.ofObj (^t: (member ResolvedType: IGraphType) x) with
    | Some graph when (graph :? NonNullGraphType) ->
        let nonNull = graph :?> NonNullGraphType
        (^t: (member set_ResolvedType: IGraphType -> unit) x, nonNull.ResolvedType)
    | _ -> ()
