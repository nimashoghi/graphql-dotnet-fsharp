module internal GraphQL.FSharp.AutoInterface

open GraphQL.Types

open GraphQL.FSharp.AutoBase
open GraphQL.FSharp.Inference

let Interface<'object> =
    if not typeof<'object>.IsInterface && not typeof<'object>.IsAbstract
    then invalidArg "interface_" "type parameter must be abstract"

    InterfaceGraphType<'object> ()
    |> setInfo typeof<'object>
    |> addProperties createReference
    |> addMethods createReference
    |> updateType typeof<'object>.TypeAttributes
