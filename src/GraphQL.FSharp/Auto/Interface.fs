module GraphQL.FSharp.AutoImplementation.Interface

open GraphQL.Types

open GraphQL.FSharp.Inference
open GraphQL.FSharp.Registry

let Interface<'interface_> =
    assert (typeof<'interface_>.IsInterface || typeof<'interface_>.IsAbstract)

    let ``interface`` = InterfaceGraphType<'interface_> ()
    setInfo typeof<'interface_> ``interface``

    addProperties inferObject ``interface``
    addMethodsComplex inferObject ``interface``

    getTypeAttribute<TypeAttribute> typeof<'interface_>
    |> Option.iter (updateType ``interface`` >> ignore)

    Object.register (typeof<'interface_>, ``interface``)

    ``interface``
