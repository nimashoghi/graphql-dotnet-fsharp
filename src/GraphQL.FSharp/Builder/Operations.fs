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
open GraphQL.FSharp.Resolvers
open GraphQL.FSharp.Types
open GraphQL.FSharp.Utils.Quotations

type IOperation<'t> =
    abstract member Invoke: 't -> 't
    abstract member Priority: int

type FlattenOperation<'t> (parameters: IOperation<'t> list) =
    member val Parameters = parameters

    interface IOperation<'t> with
        member __.Invoke _ = failwith "Not Implemented"
        member __.Priority = Int32.MinValue

let inline flatten parameters = FlattenOperation parameters

let inline processFlattening (parameters: IOperation<_> list) =
    parameters
    |> List.collect (
        fun operation ->
            match operation with
            | :? FlattenOperation<_> as flattenOp -> flattenOp.Parameters
            | operation -> [operation]
    )

let inline (|Operation|) (operation: IOperation<_>) = operation.Invoke
let inline (|Priority|) (operation: IOperation<_>) = operation.Priority

let inline reduce initial parameters =
    parameters
    |> processFlattening
    |> List.sortBy (|Priority|)
    |> List.fold (fun object (Operation operation) -> operation object) initial

let inline reduceWith initial parameters = reduce (initial ()) parameters

let inline operation priority f =
    {
        new IOperation< ^t> with
            member __.Invoke x = f x
            member __.Priority = priority
    }
let inline operationUnit priority f = operation priority (fun x -> f x; x)

let inline configure f = operation 0 f
let inline configureUnit f = operationUnit 0 f

let inline name value = configureUnit <| fun target ->
    (^t: (member set_Name: string -> unit) target, value)

let inline deprecate value = configureUnit <| fun target ->
    (^t: (member set_DeprecationReason: string -> unit) target, value)

type DefaultValueHelper =
    | DefaultValueHelper

    static member inline ($) (DefaultValueHelper, field: Field<_, ^t, _>): ^t -> unit =
        fun value ->
            field.DefaultValue <- box value
            makeNullable field

    static member inline ($) (DefaultValueHelper, argument: Argument< ^t>): ^t -> unit =
        fun value ->
            argument.DefaultValue <- box value
            makeNullable argument

let inline defaultValue value = operationUnit 100 <| fun target -> (DefaultValueHelper $ target) value

let inline graphType (value: #IGraphType) = configure <| fun target ->
    if box value |> isNull |> not
    then (^t: (member set_GraphType: IGraphType -> unit) target, (value :> IGraphType))
    target

let inline metadata value = configureUnit <| fun target ->
    let metadata = (^t: (member Metadata: IDictionary<string, obj>) target)
    (^t: (member set_Metadata: IDictionary<string, obj> -> unit) target, Dictionary.merge metadata (Dictionary.ofList value))

[<AutoOpen>]
module Documentation =
    let inline documentation operations = flatten operations

    let inline description value = configureUnit <| fun target ->
        (^t: (member set_Description: string -> unit) target, value)

    let inline argumentDocumentation (arguments: (string * string) list) = operationUnit 100 <| fun (field: Field<'arguments, 'field, 'source>) ->
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
    let inline fieldArguments (arguments: IOperation<QueryArguments> list) = configureUnit <| fun (field: Field<'arguments, 'field, 'source>) ->
        let queryArguments =
            field.Arguments
            |> Option.ofObj
            |> Option.defaultValue (QueryArguments ())

        arguments
        |> List.fold (fun arguments (Operation f) -> f arguments) queryArguments
        |> field.set_Arguments

    // TODO: Confirm priority values
    let inline validate (validator: 'arguments -> Result<'arguments, 'error list> Task) = operation 100 <| fun (field: Field<'arguments, 'field, 'source>) ->
        Field.validate validator field

    let inline subscribe (subscribe: ResolveEventStreamContext<'source> -> Task<IObservable<'field>>) = configureUnit <| fun (field: Field<'arguments, 'field, 'source>) ->
        field.AsyncSubscriber <- AsyncEventStreamResolver<_, _> (Func<_, _> subscribe)

        if isNull field.Resolver
        then field.Resolver <- resolve (fun (ctx: ResolveContext<'source>) -> unbox<'field> ctx.Source)

    type Resolve internal () =
        member inline __.manual (resolver: ResolveContext<'source> -> 'field) = configure <| fun (field: Field<'arguments, 'field, 'source>) ->
            field.Resolver <- resolve resolver

            field
            |> setType typeof<'field>

        member inline __.property ([<ReflectedDefinition true>] expr: Expr<'source -> Task<'field>>) =
            configure <| fun (field: Field<'arguments, 'field, 'source>) ->
                field
                |> Field.setField (|FieldName|_|) (withSource >> resolveAsync) expr
                |> setType typeof<'field>

        member inline __.method ([<ReflectedDefinition true>] expr: Expr<'source -> 'arguments -> Task<'field>>) =
            configure <| fun (field: Field<'arguments, 'field, 'source>) ->
                field
                |> Field.setField (|MethodName|_|) Field.resolveMethod expr
                |> Field.addArguments<'arguments, 'field, 'source>
                |> setType typeof<'field>

        member inline __.contextMethod (resolver: ResolveContext<'source> -> 'arguments -> Task<'field>) =
            configure <| fun (field: Field<'arguments, 'field, 'source>) ->
                field.Resolver <- Field.resolveCtxMethodAsync resolver

                field
                |> Field.addArguments<'arguments, 'field, 'source>
                |> setType typeof<'field>

    let resolve = Resolve ()

[<AutoOpen>]
module Object =
    let inline fields (fields: Field<'source> list) = configureUnit <| fun (object: #ComplexGraphType<'source>) ->
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

    // TODO: Implement
    let isValidUnion ``type`` = true

    let inline case' (case: 'object -> 'union) (``type``: Object<'object>) =
        assert not (isInvalidType ``type``)
        assert isValidUnion typeof<'object>

        {
            Init = fun union ->
                union.AddPossibleType ``type``
                resolveUnion<'object, 'union> ``type`` union
        }

    let inline cases' (cases: UnionCase<'union> list) = configure <| fun (union: Union<'union>) ->
        cases
        |> List.fold (fun union case -> case.Init union) union

[<AutoOpen>]
module Enum =
    let inline value (value: ^value) = configureUnit <| fun target ->
        (^t: (member set_Value: ^value -> unit) target, value)

    // TODO: Implement
    let isValidEnum ``type`` = true

    let getEnumName<'enum> () = typeof<'enum>.Name

    let getEnumValueName (case: 'enum) =
        if typeof<'enum>.IsEnum
        then Enum.GetName (typeof<'enum>, box case)
        else
            let case =
                FSharpValue.GetUnionFields (case, typeof<'enum>)
                |> fst
            case.Name

    let inline case' (case: 'enum) (properties: IOperation<EnumerationValue<'enum>> list) =
        properties
        |> List.append [
            name (getEnumValueName case)
            configure (fun value -> value.Value <- box case; value)
        ]
        |> reduceWith EnumerationValue<'enum>

    let inline cases' (values: EnumerationValue<'enum> list) = configureUnit <| fun (enum: Enumeration<'enum>) ->
        enum.Name <- getEnumName<'enum> ()
        List.iter enum.AddValue values

type CaseHelper =
    | CaseHelper

    static member inline ($) (CaseHelper, properties) = fun case -> Enum.case' case properties
    static member inline ($) (CaseHelper, graph) = fun case -> Union.case' case graph

let inline case case properties = (CaseHelper $ properties) case

type CasesHelper =
    | CasesHelper

    static member inline ($) (CasesHelper, values) = Enum.cases' values
    static member inline ($) (CasesHelper, properties) = Union.cases' properties

let inline cases properties = CasesHelper $ properties

[<AutoOpen>]
module Schema =
    let inline query (endpoints: Field<obj> list) = configureUnit <| fun (schema: #Schema) ->
        let object =
            Object<obj> (
                Name = "Query",
                Description = "The queries accepted in this GraphQL API."
            )

        endpoints
        |> List.iter (object.AddField >> ignore)
        schema.Query <- object

    let inline mutation (endpoints: Field<obj> list) = configureUnit <| fun (schema: #Schema) ->
        let object =
            Object<obj> (
                Name = "Mutation",
                Description = "The mutations accepted in this GraphQL API."
            )

        endpoints
        |> List.iter (object.AddField >> ignore)
        schema.Mutation <- object

    let inline subscription (endpoints: Field<obj> list) = configureUnit <| fun (schema: #Schema) ->
        let object =
            Object<obj> (
                Name = "Subscription",
                Description = "The subscriptions accepted in this GraphQL API."
            )

        endpoints
        |> List.iter (object.AddField >> ignore)
        schema.Subscription <- object

    let inline types (types: IGraphType list) = configureUnit <| fun (schema: #Schema) ->
        types
        |> Schema.handleInterfaces
        |> List.toArray
        |> schema.RegisterTypes

type ArgumentsHelper =
    | ArgumentsHelper

    static member inline ($) (ArgumentsHelper, properties) = argumentDocumentation properties
    static member inline ($) (ArgumentsHelper, properties) = fieldArguments properties

let inline arguments properties = ArgumentsHelper $ properties
