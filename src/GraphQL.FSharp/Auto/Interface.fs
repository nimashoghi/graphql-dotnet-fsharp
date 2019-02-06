module internal GraphQL.FSharp.AutoInterface

open GraphQL.Types

open GraphQL.FSharp.AutoBase
open GraphQL.FSharp.Inference
open GraphQL.FSharp.NameTransformers
open GraphQL.FSharp.Utils.Attributes

let Interface<'object> =
    if not typeof<'object>.IsInterface && not typeof<'object>.IsAbstract
    then invalidArg "interface_" "type parameter must be abstract"

    InterfaceGraphType<'object> (Name = transformTypeName typeof<'object> typeof<'object>.Name)
    |> addProperties createReference
    |> addMethods createReference
    |> updateType typeof<'object>.TypeAttributes
