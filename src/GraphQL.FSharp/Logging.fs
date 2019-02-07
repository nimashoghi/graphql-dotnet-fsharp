[<AutoOpen>]
module GraphQL.FSharp.Logging

open System
open Serilog
open Serilog.Core

type Logger with
    member this.debug (lst: string list) = this.Debug (String.Join ("\n", lst))
    member this.error (lst: string list) = this.Error (String.Join ("\n", lst))
    member this.fatal (lst: string list) = this.Fatal (String.Join ("\n", lst))
    member this.information (lst: string list) = this.Information (String.Join ("\n", lst))
    member this.verbose (lst: string list) = this.Verbose (String.Join ("\n", lst))
    member this.warning (lst: string list) = this.Warning (String.Join ("\n", lst))

    member this.debugf format = Printf.ksprintf this.Debug format
    member this.errorf format = Printf.ksprintf this.Error format
    member this.fatalf format = Printf.ksprintf this.Fatal format
    member this.informationf format = Printf.ksprintf this.Information format
    member this.verbosef format = Printf.ksprintf this.Verbose format
    member this.warningf format = Printf.ksprintf this.Warning format

let Logger =
    LoggerConfiguration().WriteTo
        .Console()
        .CreateLogger()

open System.Reflection
open GraphQL.Types
open GraphQL.FSharp.Types
open GraphQL.FSharp.Utils

let generateIndentation level = new string [|for _ in 0 .. level * 4 -> ' '|]

let indentLevel level lst = lst |> List.map (sprintf "%s%s" (generateIndentation level))
let indent lst = indentLevel 1 lst

let getArguments (field: #FieldType) = [
    if not <| isNull field.Arguments
    then yield! field.Arguments
]

let logArgument (argument: #QueryArgument) = [
    yield sprintf "Argument '%s' has type '%s'" argument.Name (graphTypeNameDetailed argument.ResolvedType)
]

let logField (field: #FieldType) = [
    yield sprintf "Field '%s' of type '%s'" field.Name (graphTypeNameDetailed field.ResolvedType)
    yield "Arguments:"
    yield! getArguments field |> List.collect logArgument |> indent
]

let getFields (object: #IComplexGraphType) = [
    if not <| isNull object.Fields
    then yield! object.Fields
]

let logObject (object: #IComplexGraphType) = [
    yield sprintf "Object '%s'" object.Name
    yield "Fields:"
    yield! getFields object |> List.collect logField |> indent
]

let logOperation name (operation: #IComplexGraphType) = [
    if isNull operation then yield sprintf "%s: Null" name else
    yield! logObject operation
]

let logGraphType (``type``: #IGraphType) = [
    match ``type`` :> IGraphType with
    | :? IComplexGraphType as ``type`` ->
        yield! logObject ``type``
    | ``type`` ->
        yield sprintf "Graph type '%s' of system type '%s'" (graphTypeNameDetailed ``type``) (``type``.GetType().Name)
]

let additionalInstances (schema: Schema) = [
    yield!
        typeof<Schema>
            .GetField("_additionalInstances", BindingFlags.Instance ||| BindingFlags.NonPublic)
            .GetValue schema
        |> unbox<System.Collections.Generic.List<IGraphType>>
]

let logSchema (schema: Schema) = [
    yield "Schema"
    yield! logOperation "Query" schema.Query |> indent
    yield! logOperation "Mutation" schema.Mutation |> indent
    yield! logOperation "Subscription" schema.Subscription |> indent
    yield "Additional types:"
    yield! additionalInstances schema |> List.collect logGraphType |> indent
]
