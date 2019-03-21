[<AutoOpen>]
module GraphQL.FSharp.Builder.Operations

open System
open System.Collections.Generic
open System.Threading.Tasks
open FSharp.Quotations
open FSharp.Reflection
open FSharp.Utils
open GraphQL.Resolvers
open GraphQL.Subscription
open GraphQL.Types

open GraphQL.FSharp.Builder.Utils
open GraphQL.FSharp.Inference
open GraphQL.FSharp.Resolvers
open GraphQL.FSharp.Types
open GraphQL.FSharp.Utils.Quotations

type IOperation<'t> =
    abstract member Invoke: 't -> 't
    abstract member Priority: int

let (|Operation|) (operation: IOperation<_>) = operation.Invoke
let (|Priority|) (operation: IOperation<_>) = operation.Priority

type FlattenOperation<'t> (parameters: IOperation<'t> list) =
    member val Parameters = parameters

    interface IOperation<'t> with
        member __.Invoke target =
            parameters
            |> List.sortBy (|Priority|)
            |> List.fold (fun target (Operation f) -> f target) target
        member __.Priority = Int32.MinValue

let flatten parameters = FlattenOperation parameters

let processFlattening (parameters: IOperation<_> list) =
    parameters
    |> List.collect (
        fun operation -> [
            match operation with
            | :? FlattenOperation<_> as flattenOp -> yield! flattenOp.Parameters
            | operation -> yield operation
        ]
    )

let reduce initial parameters =
    parameters
    |> processFlattening
    |> List.sortBy (|Priority|)
    |> List.fold (fun object (Operation operation) -> operation object) initial

let reduceWith initial parameters = reduce (initial ()) parameters

let operation priority f =
    {
        new IOperation<_> with
            member __.Invoke x = f x
            member __.Priority = priority
    }
let operationUnit priority f = operation priority (fun x -> f x; x)

let configure f = operation 0 f
let configureUnit f = operationUnit 0 f

let inline name value = configureUnit <| fun target ->
    (^t: (member set_Name: string -> unit) target, value)

let inline deprecate value = configureUnit <| fun target ->
    (^t: (member set_DeprecationReason: string -> unit) target, value)

type DefaultValueHelper =
    | DefaultValueHelper

    static member ($) (DefaultValueHelper, field: Field<_,'t, _>): 't -> unit =
        fun value ->
            field.DefaultValue <- box value
            makeNullable field

    static member ($) (DefaultValueHelper, argument: Argument<'t>): 't -> unit =
        fun value ->
            argument.DefaultValue <- box value
            makeNullable argument

let inline defaultValue value = operationUnit 100 <| fun target -> (DefaultValueHelper $ target) value

let inline graphType (value: #IGraphType) = configureUnit <| fun target ->
    if box value |> isNull |> not
    then (^t: (member set_GraphType: IGraphType -> unit) target, (value :> IGraphType))

// TODO: Update this and fix the logic
let inline graphOrSystemType (value: #IGraphType) ``type`` = operationUnit Int32.MinValue <| fun target ->
    if box value |> isNull |> not
    then (^t: (member set_GraphType: IGraphType -> unit) target, (value :> IGraphType))
    else if isInvalidType (^t: (member ResolvedType: IGraphType) target)
    then (^t: (member set_ResolvedType: IGraphType -> unit) target, createReference ``type``)

let inline metadata value = configureUnit <| fun target ->
    let metadata =
        (^t: (member Metadata: IDictionary<string, obj>) target)
        |> Option.ofObj
        |> Option.defaultValue (upcast Dictionary.empty)
    (^t: (member set_Metadata: IDictionary<string, obj> -> unit) target, Dictionary.merge metadata (Dictionary.ofList value))

[<AutoOpen>]
module Documentation =
    let documentation operations = flatten operations

    let inline description value = configureUnit <| fun target ->
        (^t: (member set_Description: string -> unit) target, value)

    let argumentDocumentation (arguments: (string * string) list) = operationUnit 100 <| fun (field: Field<'arguments, 'field, 'source>) ->
        let queryArguments =
            field.Arguments
            |> Option.ofObj
            |> Option.defaultValue (QueryArguments ())

        arguments
        |> List.map (
            fun (key, value) ->
                let arg =
                    queryArguments
                    |> Seq.find (fun argument -> argument.Name = key)
                arg, value
        )
        |> List.iter (fun (arg, value) -> arg.Description <- value)

// TODO: Do not keep on adding arguments
(*
    TODO: Watch out for this case:
        Field [
            Validate (
                fun (args: {|Name: string|}) -> validation {
                    return
                        {|
                            args with
                                SomeNewParam = "" // TODO: Does SomeNewParam get added?
                        |}
                }
            )
        ]
*)
[<AutoOpen>]
module Field =
    let fieldArguments (arguments: Argument list) = configureUnit <| fun (field: Field<'arguments, 'field, 'source>) ->
        let queryArguments =
            field.Arguments
            |> Option.ofObj
            |> Option.map (Seq.cast<QueryArgument> >> Seq.toList)
            |> Option.defaultValue []

        arguments
        |> List.map (fun argument -> argument :> QueryArgument)
        |> (@) queryArguments
        |> List.groupBy (fun argument -> argument.Name)
        |> List.map (fun (_, list) -> List.last list)
        |> QueryArguments
        |> field.set_Arguments

    // TODO: Confirm priority values
    let validate (validator: 'arguments -> Result<'arguments, 'error list> Task) = operation 100 <| fun (field: Field<'arguments, 'field, 'source>) ->
        Field.validate validator field

    // TODO: Test this
    let subscribe (subscribe: ResolveEventStreamContext<'source> -> 'field IObservable Task) = configureUnit <| fun (field: Field<'arguments, 'field, 'source>) ->
        field.AsyncSubscriber <- AsyncEventStreamResolver<_, _> (Func<_, _> subscribe)

        if isNull field.Resolver then
            field.Resolver <-
                resolveAsync (
                    fun (ctx: ResolveContext<'source>) ->
                        Task.FromResult (unbox<'field> ctx.Source)
                )

    type Resolve internal () =
        member __.manual (resolver: ResolveContext<'source> -> 'field Task) =
            configureUnit <| fun (field: Field<'arguments, 'field, 'source>) ->
                field.Resolver <- resolveAsync resolver

        member __.property ([<ReflectedDefinition true>] expr: Expr<'source -> 'field Task>) =
            configure <| fun (field: Field<'arguments, 'field, 'source>) ->
                field
                |> Field.setField (|FieldName|_|) (withSource >> resolveAsync) expr

        member __.method ([<ReflectedDefinition true>] expr: Expr<'source -> 'arguments -> 'field Task>) =
            configure <| fun (field: Field<'arguments, 'field, 'source>) ->
                field
                |> Field.setField (|MethodName|_|) (Field.resolveMethod resolveAsync) expr
                |> Field.addArguments<'arguments, 'field, 'source>

        member __.contextMethod (resolver: ResolveContext<'source> -> 'arguments -> 'field Task) =
            configure <| fun (field: Field<'arguments, 'field, 'source>) ->
                field.Resolver <- (Field.resolveCtxMethodAsync resolveAsync) resolver

                field
                |> Field.addArguments<'arguments, 'field, 'source>

    let resolve = Resolve ()

[<AutoOpen>]
module Object =
    let fields (fields: Field<'source> list) = configureUnit <| fun (object: #ComplexGraphType<'source>) ->
        fields
        |> List.iter (object.AddField >> ignore)

[<AutoOpen>]
module Union =
    type UnionCase<'union> = {
        Init: Union<'union> -> Union<'union>
    }

    // TODO: optimize this
    let resolveUnion<'object, 'union> ``type`` (union: Union<'union>) =
        let previous =
            match union.ResolveType with
            | null -> fun _ -> null
            | f -> f.Invoke
        union.ResolveType <- fun object ->
            match previous object with
            | null when (object :? 'object) -> ``type``
            | _ -> null
        union

    let isValidUnion ``type`` = FSharpType.IsUnion ``type``

    let unionCase (case: 'object -> 'union) (``type``: Object<'object>) =
        assert not (isInvalidType ``type``)
        assert isValidUnion typeof<'union>

        {
            Init = fun union ->
                union.AddPossibleType ``type``
                resolveUnion<'object, 'union> ``type`` union
        }

    let unionCases (cases: UnionCase<'union> list) = configure <| fun (union: Union<'union>) ->
        cases
        |> List.fold (fun union case -> case.Init union) union

[<AutoOpen>]
module Enum =
    let inline value (value: ^value) = configureUnit <| fun target ->
        (^t: (member set_Value: ^value -> unit) target, value)

    let isValidEnum (``type``: Type) =
        ``type``.IsEnum
        || FSharpType.IsUnion ``type``

    let getEnumName<'enum> () = typeof<'enum>.Name

    let getEnumValueName (case: 'enum) =
        if typeof<'enum>.IsEnum
        then Enum.GetName (typeof<'enum>, box case)
        else
            let case =
                FSharpValue.GetUnionFields (case, typeof<'enum>)
                |> fst
            case.Name

    let enumCase (case: 'enum) (properties: IOperation<EnumerationValue<'enum>> list) =
        assert isValidEnum typeof<'enum>

        properties
        |> List.append [
            name (getEnumValueName case)
            configure (fun value -> value.Value <- box case; value)
        ]
        |> reduceWith EnumerationValue<'enum>

    let enumCases (values: EnumerationValue<'enum> list) = configureUnit <| fun (enum: Enumeration<'enum>) ->
        enum.Name <- getEnumName<'enum> ()
        List.iter enum.AddValue values

type CaseHelper =
    | CaseHelper

    static member ($) (CaseHelper, properties) = fun case -> enumCase case properties
    static member ($) (CaseHelper, graph) = fun case -> unionCase case graph

let inline case case properties = (CaseHelper $ properties) case

type CasesHelper =
    | CasesHelper

    static member ($) (CasesHelper, values) = enumCases values
    static member ($) (CasesHelper, properties) = unionCases properties

let inline cases properties = CasesHelper $ properties

[<AutoOpen>]
module Schema =
    let query (endpoints: Field<obj> list) = configureUnit <| fun (schema: #Schema) ->
        let object =
            Object<obj> (
                Name = "Query",
                Description = "The queries accepted in this GraphQL API."
            )

        endpoints
        |> List.iter (object.AddField >> ignore)
        schema.Query <- object

    let mutation (endpoints: Field<obj> list) = configureUnit <| fun (schema: #Schema) ->
        let object =
            Object<obj> (
                Name = "Mutation",
                Description = "The mutations accepted in this GraphQL API."
            )

        endpoints
        |> List.iter (object.AddField >> ignore)
        schema.Mutation <- object

    let subscription (endpoints: Field<obj> list) = configureUnit <| fun (schema: #Schema) ->
        let object =
            Object<obj> (
                Name = "Subscription",
                Description = "The subscriptions accepted in this GraphQL API."
            )

        endpoints
        |> List.iter (object.AddField >> ignore)
        schema.Subscription <- object

    let types (types: IGraphType list) = configureUnit <| fun (schema: #Schema) ->
        types
        |> Schema.handleInterfaces
        |> List.toArray
        |> schema.RegisterTypes

type ArgumentsHelper =
    | ArgumentsHelper

    static member ($) (ArgumentsHelper, properties) = argumentDocumentation properties
    static member ($) (ArgumentsHelper, properties) = fieldArguments properties

let inline arguments properties = ArgumentsHelper $ properties
