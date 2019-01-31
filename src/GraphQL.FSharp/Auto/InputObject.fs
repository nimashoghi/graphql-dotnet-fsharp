module internal GraphQL.FSharp.AutoInputObject

open GraphQL.Types

open GraphQL.FSharp.AutoBase
open GraphQL.FSharp.Inference

let InputObject<'object> =
    if typeof<'object>.IsInterface || typeof<'object>.IsAbstract
    then invalidArg "object" "type parameter cannot be abstract"

    InputObjectGraphType<'object> ()
    |> setInfo typeof<'object>
    |> addProperties createReference
    |> addMethods createReference
    |> updateType typeof<'object>.TypeAttributes
