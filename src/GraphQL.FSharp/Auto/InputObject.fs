module internal GraphQL.FSharp.AutoInputObject

open GraphQL.Types

open GraphQL.FSharp.AutoBase
open GraphQL.FSharp.Inference
open GraphQL.FSharp.Registry

let InputObject<'object> =
    if typeof<'object>.IsInterface || typeof<'object>.IsAbstract
    then invalidArg "object" "type parameter cannot be abstract"

    InputObjectGraphType<'object> ()
    |> setInfo typeof<'object>
    |> addProperties inferInput
    |> addMethods inferInput
    |> updateType typeof<'object>.TypeAttributes
    |> InputObject.register typeof<'object>
