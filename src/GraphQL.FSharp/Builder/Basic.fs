[<AutoOpen>]
module GraphQL.FSharp.Builder.Basic

open System
open System.Collections.Generic
open FSharp.Utils
open FSharp.Utils.Reflection
open GraphQL.Types

open GraphQL.FSharp.Builder.Utils
open GraphQL.FSharp.Inference
open GraphQL.FSharp.Types

let inline optional (operation: ^args -> _ IOperation) (args: ^args voption) =
    match args with
    | ValueSome args -> flatten [operation args] :> _ IOperation
    | ValueNone -> flatten [] :> _ IOperation

let inline name value = Operation.ConfigureUnit <| fun target ->
    (^t: (member set_Name: string -> unit) target, value)

let inline deprecate value = Operation.ConfigureUnit <| fun target ->
    (^t: (member set_DeprecationReason: string -> unit) target, value)

let inline defaultValue value = Operation.CreateUnit Priority.DefaultValue <| fun (target: ^t) ->
    (^t: (member set_DefaultValue: obj -> unit) target, box value)
    makeNullable target

let shouldBeNullable (``type``: Type) =
    let (|ObjectType|_|) (``type``: Type) = if ``type`` = typeof<obj> then Some () else None
    match ``type`` with
    | ObjectType -> false
    | OptionType _
    | ResultType _ -> true
    | _ -> false

// TODO: Clean up later
let graphOrSystemTypeField (value: #IGraphType) = Operation.CreateUnit Priority.InferredGraphTypeField <| fun (field: Field<'field, 'arguments, 'source>) ->
    if (not << isNull) (box value) then
        field.ResolvedType <- processGraphType (shouldBeNullable typeof<'field>) value
    else if isInvalidType field.ResolvedType then
        field.ResolvedType <- inferField field typeof<'field>

    if field.HasMetadata "Validator" then makeNullable field

let inline graphOrSystemType (value: #IGraphType) ``type`` = Operation.CreateUnit Priority.InferredGraphType <| fun target ->
    if (not << isNull) (box value) then
        (^t: (member set_ResolvedType: IGraphType -> unit) target, processGraphType (shouldBeNullable ``type``) value)
    else if isInvalidType (^t: (member ResolvedType: IGraphType) target) then
        (^t: (member set_ResolvedType: IGraphType -> unit) target, infer ``type``)

let inline metadata value = Operation.ConfigureUnit <| fun target ->
    let metadata =
        (^t: (member Metadata: IDictionary<string, obj>) target)
        |> Option.ofObj
        |> Option.defaultValue (upcast Dictionary.empty)
    (^t: (member set_Metadata: IDictionary<string, obj> -> unit) target, Dictionary.merge metadata (Dictionary.ofList value))
