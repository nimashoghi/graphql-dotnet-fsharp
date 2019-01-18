module GraphQL.FSharp.AutoImplementation.Interface

open GraphQL.Types

open GraphQL.FSharp.Inference
open GraphQL.FSharp.Registry

let Interface<'interface_> =
    if not typeof<'interface_>.IsInterface && not typeof<'interface_>.IsAbstract
    then invalidArg "interface_" "type parameter must be abstract"

    let ``interface`` = InterfaceGraphType<'interface_> ()
    setInfo typeof<'interface_> ``interface``

    addProperties inferObject ``interface``
    addMethods<'interface_> inferObject ``interface``

    getTypeAttribute<TypeAttribute> typeof<'interface_>
    |> Option.iter (updateType ``interface`` >> ignore)

    Object.register (typeof<'interface_>, ``interface``)

    ``interface``
