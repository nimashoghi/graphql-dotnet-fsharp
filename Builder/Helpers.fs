module GraphQL.FSharp.Builder.Helpers

open System
open Iris.Option.Builders
open GraphQL
open GraphQL.Types

open GraphQL.FSharp.Model

let isNullable = function
| Mandatory -> false
| _ -> true

// TODO: Handle option types
let setType (schema: SchemaInfo) (``type``: Type) nullable (field: FieldType) =
    try field.Type <- ``type``.GetGraphTypeFromType nullable
    with
    | :? ArgumentOutOfRangeException ->
        maybeUnit {
            let! graphType = schema.GetObject ``type`` nullable
            field.ResolvedType <- graphType
        }

let getType (schema: SchemaInfo) (``type``: Type) nullable =
    try
        ``type``.GetGraphTypeFromType nullable
        |> Activator.CreateInstance
        :?> IGraphType
    with
    | :? ArgumentOutOfRangeException -> Option.toObj (schema.GetObject ``type`` nullable)
