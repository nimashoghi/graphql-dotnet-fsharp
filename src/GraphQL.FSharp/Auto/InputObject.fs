module internal GraphQL.FSharp.AutoInputObject

open GraphQL.Types

open GraphQL.FSharp.AutoBase
open GraphQL.FSharp.Inference
open GraphQL.FSharp.NameTransformers
open GraphQL.FSharp.Utils.Attributes

let InputObject<'object> =
    if typeof<'object>.IsInterface || typeof<'object>.IsAbstract
    then invalidArg "object" "type parameter cannot be abstract"

    InputObjectGraphType<'object> (Name = transformTypeName typeof<'object> typeof<'object>.Name)
    |> addProperties createReference
    |> addMethods createReference
    |> updateType typeof<'object>.TypeAttributes
