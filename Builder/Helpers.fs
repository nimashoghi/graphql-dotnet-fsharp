module GraphQL.FSharp.Builder.Helpers

open System
open System.Collections.Generic
open Apollo
open Iris.Option.Builders
open GraphQL
open GraphQL.Types

open GraphQL.FSharp.Model

let isNullable = function
| Mandatory -> false
| _ -> true

let setType (schema: SchemaImplementation) (``type``: Type) nullable (field: FieldType) =
    try field.Type <- ``type``.GetGraphTypeFromType nullable
    with
    | :? ArgumentOutOfRangeException ->
        maybeUnit {
            let! graphType = schema.GetObject ``type`` nullable
            field.ResolvedType <- graphType
        }

let getType (schema: SchemaImplementation) (``type``: Type) nullable =
    try ``type``.GetGraphTypeFromType nullable |> Activator.CreateInstance :?> IGraphType
    with
    | :? ArgumentOutOfRangeException -> Option.get (schema.GetObject ``type`` nullable)
