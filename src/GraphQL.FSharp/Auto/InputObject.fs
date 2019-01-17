module GraphQL.FSharp.AutoImplementation.InputObject

open GraphQL.Types

open GraphQL.FSharp.Inference
open GraphQL.FSharp.Registry

let InputObject<'object> =
    assert not typeof<'object>.IsInterface

    let object = InputObjectGraphType<'object> ()
    setInfo typeof<'object> object

    addProperties inferInput object
    addMethodsComplex inferInput object

    getTypeAttribute<TypeAttribute> typeof<'object>
    |> Option.iter (updateType object >> ignore)

    InputObject.register (typeof<'object>, object)

    object
