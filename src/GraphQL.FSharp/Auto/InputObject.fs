module GraphQL.FSharp.AutoInputObject

open GraphQL.Types

open GraphQL.FSharp.AutoBase
open GraphQL.FSharp.Inference
open GraphQL.FSharp.Registry

let InputObject<'object> =
    if typeof<'object>.IsInterface || typeof<'object>.IsAbstract
    then invalidArg "object" "type parameter cannot be abstract"

    let object = InputObjectGraphType<'object> ()
    setInfo typeof<'object> object

    addProperties inferInput object
    addMethods<'object> inferInput object

    getTypeAttribute<TypeAttribute> typeof<'object>
    |> Option.iter (updateType object >> ignore)

    InputObject.register (typeof<'object>, object)

    object
