[<AutoOpen>]
module GraphQL.FSharp.Util.TypeRegistry

open System
open GraphQL.Utilities
open GraphQL.Types

// TODO: Refactor this
let private tryGet ``type`` =
    match GraphTypeTypeRegistry.Get ``type`` with
    | null -> None
    | ``type`` -> Some (Activator.CreateInstance ``type`` :?> IGraphType)

module Object =
    let private registry = GraphTypeInstanceRegistry ()

    let get ``type`` =
        match tryGet ``type``, registry.Get ``type`` with
        | Some ``type``, _ -> Some ``type``
        | _, null -> None
        | _, ``type`` -> Some ``type``

    let register (sys, gql) =
        registry.Register (sys, gql)

module InputObject =
    let private registry = GraphTypeInstanceRegistry ()

    let get ``type`` =
        match tryGet ``type``, registry.Get ``type`` with
        | Some ``type``, _ -> Some ``type``
        | _, null -> None
        | _, ``type`` -> Some ``type``

    let register (sys, gql) =
        registry.Register (sys, gql)
