module GraphQL.FSharp.Registry

open System
open GraphQL.Utilities
open GraphQL.Types

// TODO: Refactor this
let private tryGet ``type`` =
    GraphTypeTypeRegistry.Get ``type``
    |> Option.ofObj
    |> Option.map (fun ``type`` -> Activator.CreateInstance ``type`` :?> IGraphType)

module Object =
    let mutable typesToRegister: IGraphType list = []
    let private registry = GraphTypeInstanceRegistry ()

    let get ``type`` =
        match tryGet ``type``, registry.Get ``type`` with
        | Some ``type``, _ -> Some ``type``
        | _, null -> None
        | _, ``type`` -> Some ``type``

    let register (sys, gql) =
        typesToRegister <- gql :: typesToRegister
        registry.Register (sys, gql)

module InputObject =
    let private registry = GraphTypeInstanceRegistry ()

    let get ``type`` =
        match tryGet ``type``, registry.Get ``type`` with
        | Some ``type``, _ -> Some ``type``
        | _, null -> None
        | _, ``type`` -> Some ``type``

    let register (sys, gql) =
        Object.typesToRegister <- gql :: Object.typesToRegister
        registry.Register (sys, gql)
