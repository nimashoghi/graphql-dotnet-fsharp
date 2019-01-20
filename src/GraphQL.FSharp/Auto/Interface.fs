module internal GraphQL.FSharp.AutoInterface

open GraphQL.Types

open GraphQL.FSharp.AutoBase
open GraphQL.FSharp.Inference
open GraphQL.FSharp.Registry

let Interface<'object> =
    if not typeof<'object>.IsInterface && not typeof<'object>.IsAbstract
    then invalidArg "interface_" "type parameter must be abstract"

    InterfaceGraphType<'object> ()
    |> setInfo typeof<'object>
    |> addProperties inferObject
    |> addMethods inferObject
    |> updateType typeof<'object>.TypeAttributes
    |> Object.register typeof<'object>
