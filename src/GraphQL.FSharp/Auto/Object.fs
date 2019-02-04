module internal GraphQL.FSharp.AutoObject

open GraphQL.Types

open GraphQL.FSharp.AutoBase
open GraphQL.FSharp.Inference

let setIsTypeOf (object: ObjectGraphType<'object>) =
    object.IsTypeOf <- fun x -> x :? 'object
    object

let Object<'object> =
    if typeof<'object>.IsInterface || typeof<'object>.IsAbstract
    then invalidArg "'object" "Type parameter cannot be abstract"

    ObjectGraphType<'object> ()
    |> setInfo typeof<'object>
    |> addProperties createReference
    |> addMethods createReference
    |> updateType typeof<'object>.TypeAttributes
    |> setIsTypeOf
