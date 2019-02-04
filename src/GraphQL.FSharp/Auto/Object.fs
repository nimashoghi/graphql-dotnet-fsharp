module internal GraphQL.FSharp.AutoObject

open GraphQL.Types

open GraphQL.FSharp.AutoBase
open GraphQL.FSharp.Inference

let setIsTypeOf (object: ObjectGraphType<'object>) =
    object.IsTypeOf <- fun x -> x :? 'object
    object

let Object<'object> =
    ObjectGraphType<'object> (Name = typeof<'object>.Name)
    |> addProperties createReference
    |> addMethods createReference
    |> updateType typeof<'object>.TypeAttributes
    |> setIsTypeOf
