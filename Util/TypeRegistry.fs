[<AutoOpen>]
module GraphQL.FSharp.Util.TypeRegistry

open System
open GraphQL.Types
open GraphQL.Utilities

let private registry = GraphTypeInstanceRegistry()

let get ``type`` =
    match registry.Get ``type`` with
    | null -> None
    | ``type`` -> Some ``type``

let register (sys, gql) =
    registry.Register (sys, gql)
