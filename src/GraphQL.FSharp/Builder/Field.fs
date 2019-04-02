[<AutoOpen>]
module GraphQL.FSharp.Builder.Field

open System
open System.Reactive.Linq
open System.Reflection
open System.Threading.Tasks
open FSharp.Quotations
open FSharp.Reflection
open FSharp.Utils
open FSharp.Utils.Quotations
open FSharp.Utils.Reflection
open FSharp.Utils.Tasks
open GraphQL.Types

open GraphQL.FSharp.Builder.Utils
open GraphQL.FSharp.Inference
open GraphQL.FSharp.Resolvers
open GraphQL.FSharp.Types
open GraphQL.FSharp.Utils.Quotations

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

let arguments (arguments: Argument list) = Operation.ConfigureUnit <| fun (field: Field<'field, 'arguments, 'source>) ->
    let queryArguments =
        field.Arguments
        |> Option.ofObj
        |> Option.map (Seq.cast<QueryArgument> >> Seq.toList)
        |> Option.defaultValue []
    let arguments =
        arguments
        |> List.map (fun argument -> argument :> QueryArgument)
        |> (@) queryArguments
    arguments
    |> List.uniqueBy (fun argument -> argument.Name)
    |> QueryArguments
    |> field.set_Arguments

[<Struct>]
type Validator<'arguments> = Validator of Validator: ('arguments -> Result<'arguments, obj list> ValueTask)

let validate (validator: 'arguments -> Result<'arguments, 'error list> ValueTask) = Operation.CreateUnit Priority.Validation <| fun (field: Field<'field, 'arguments, 'source>) ->
    if typeof<'arguments> = typeof<obj> then failwith "Argument type cannot be null!" else
    field.Metadata.["Validator"] <-
        box (
            Validator (
                fun arguments -> vtask {
                    match! validator arguments with
                    | Ok value -> return Ok value
                    | Error errors ->
                        return
                            errors
                            |> List.map box
                            |> Error
                }
            )
        )

// TODO: What about nested objects? e.g. {|Prop = {|Name = "hi"}|}
let internal addArguments () = Operation.ConfigureUnit <| fun (field: Field<'field, 'arguments, 'source>) ->
    if typeof<'arguments> = typeof<obj> then () else

    if isNull field.Arguments
    then field.Arguments <- QueryArguments ()

    FSharpType.GetRecordFields typeof<'arguments>
    |> Array.filter (
        fun recordField ->
            field.Arguments
            |> Seq.tryFind (fun argument -> argument.Name = recordField.Name)
            |> Option.isNone
    )
    |> Array.map (fun field ->
        Argument (
            ResolvedType = createReference field.PropertyType,
            Name = field.Name
        )
    )
    |> Array.iter field.Arguments.Add

let internal makeArgumentArray<'arguments, 'source> (fields: PropertyInfo [] Lazy) (ctx: ResolveContext<'source>) =
    if typeof<'arguments> = typeof<obj> then [||] else

    let (Lazy fields) = fields
    fields
    |> Array.map (fun field ->
        ctx.GetArgument (
            argumentType = field.PropertyType,
            name = field.Name
        )
        |> Option.ofObj
        |> Option.orElseWith (fun () ->
            if not <| ctx.HasArgument field.Name
            then None
            else Some ctx.Arguments.[field.Name]
        )
        |> Option.toObj
    )

// TODO: Take a look at null (which leads to unbox throwing)
let internal makeArgumentRecord<'arguments> (constructor: (obj [] -> obj) Lazy) (arguments: obj [])  =
    if typeof<'arguments> = typeof<obj> then unbox<'arguments> null else

    let (Lazy constructor) = constructor

    arguments
    |> constructor
    |> unbox<'arguments>

let internal makeArguments<'arguments, 'source> fields constructor ctx =
    if typeof<'arguments> = typeof<obj> then unbox<'arguments> null else

    makeArgumentArray<'arguments, 'source> fields ctx
    |> makeArgumentRecord<'arguments> constructor

let internal getRecordInfo<'arguments> () =
    let fields = lazy (FSharpType.GetRecordFields typeof<'arguments>)
    let constructor = lazy (FSharpValue.PreComputeRecordConstructor typeof<'arguments>)
    let reader = lazy (FSharpValue.PreComputeRecordReader typeof<'arguments>)
    fields, constructor, reader

let internal resolveHelper
    (resolver: ResolveContext<'source> -> ResolvedValue<'field, 'error> ValueTask)
    (field: Field<'field, 'arguments, 'source>) =
    field.Resolver <- resolveAsync resolver

let internal susbscribeHelper
    (resolver: ResolveContext<'source> -> ResolvedValue<ResolvedValue<'field, 'error> IObservable, 'error> ValueTask)
    (field: Field<'field, 'arguments, 'source>) =
    field.AsyncSubscriber <- resolveSubscription resolver
    field.Resolver <- resolve (fun ctx -> ctx.Source)

let internal getValidator (field: Field<'field, 'arguments, 'source>) =
    if not <| field.HasMetadata "Validator" then None else
    field.Metadata.["Validator"]
    |> tryUnbox<Validator<'arguments>>
    |> Option.map (fun (Validator validator) -> validator)

// TODO: Subscribe should only have endpoint method
let inline internal subscribeEndpoint makeNull (innerCtor: _ -> ResolvedValue<_, _>) (resolver: 'arguments -> _) =
    let fields, constructor, _ = getRecordInfo<'arguments> ()
    Operation.ConfigureUnit <| fun (field: Field<'field, 'arguments, obj>) ->
        if makeNull then makeNullable field
        let validator = getValidator field
        susbscribeHelper (
            fun ctx -> vtask {
                let arguments = makeArguments<'arguments, _> fields constructor ctx
                match validator with
                | Some validator ->
                    match! validator arguments with
                    | Ok arguments ->
                        let! (value: _ IObservable) = resolver arguments
                        return ResultValue (Ok (value.Select innerCtor))
                    | Error errors -> return ResultValue (Error (errors |> List.map unbox<_>))
                | None ->
                    let! (value: _ IObservable) = resolver arguments
                    return ValueValue (value.Select innerCtor)
            }
        ) field

type Subscribe internal () =
    member __.endpoint (resolver: 'arguments -> 'field IObservable ValueTask) = subscribeEndpoint false ValueValue resolver
    member __.endpointOption (resolver: 'arguments -> 'field option IObservable ValueTask) = subscribeEndpoint true OptionValue resolver
    member __.endpointVOption (resolver: 'arguments -> 'field voption IObservable ValueTask) = subscribeEndpoint true ValueOptionValue resolver
    member __.endpointResult (resolver: 'arguments -> Result<'field, 'error list> IObservable ValueTask) = subscribeEndpoint true ResultValue resolver

let subscribe = Subscribe ()

let internal setName (|NamePattern|_|) expr (field: Field<_, _, _>) =
    match expr with
    | WithValueTyped (_, expr) ->
        match expr with
        | NamePattern name ->
            if String.IsNullOrEmpty field.Name
            then field.Name <- name
        | _ -> ()
    | _ -> ()

let internal getValue expr =
    match expr with
    | WithValueTyped (value, _) -> value
    | _ -> failwith "Could not extract value out of expression!"

let inline internal resolveContext makeNull (innerCtor: _ -> ResolvedValue<_, _>) resolver =
    Operation.ConfigureUnit <| fun (field: Field<'field, 'arguments, 'source>) ->
        if makeNull then makeNullable field
        resolveHelper (
            fun ctx -> vtask {
                let! value = resolver ctx
                return innerCtor value
            }
        ) field

let inline internal resolveEndpoint makeNull (innerCtor: _ -> ResolvedValue<_, _>) resolver =
    let fields, constructor, _ = getRecordInfo<'arguments> ()
    Operation.ConfigureUnit <| fun (field: Field<'field, 'arguments, 'source>) ->
        if makeNull then makeNullable field
        let validator = getValidator field
        resolveHelper (
            fun ctx -> vtask {
                let arguments = makeArguments<'arguments, 'source> fields constructor ctx
                match validator with
                | Some validator ->
                    match! validator arguments with
                    | Ok arguments ->
                        let! value = resolver arguments
                        return innerCtor value
                    | Error errors -> return ResultValue (Error (errors |> List.map unbox<_>))
                | None ->
                    let! value = resolver arguments
                    return innerCtor value
            }
        ) field

let inline internal resolveProperty makeNull (innerCtor: _ -> ResolvedValue<_, _>) expr =
    Operation.ConfigureUnit <| fun (field: Field<'field, obj, 'source>) ->
        if makeNull then makeNullable field
        setName (|FieldName|_|) expr field
        let resolver = getValue expr
        resolveHelper (
            fun ctx -> vtask {
                let! value = resolver ctx.Source
                return innerCtor value
            }
        ) field

let inline internal resolveMethod makeNull (innerCtor: _ -> ResolvedValue<_, _>) expr =
    let fields, constructor, _ = getRecordInfo<'arguments> ()
    Operation.ConfigureUnit <| fun (field: Field<'field, 'arguments, 'source>) ->
        if makeNull then makeNullable field
        setName (|MethodName|_|) expr field
        let validator = getValidator field
        let resolver = getValue expr
        resolveHelper (
            fun ctx -> vtask {
                let arguments = makeArguments<'arguments, 'source> fields constructor ctx
                match validator with
                | Some validator ->
                    match! validator arguments with
                    | Ok arguments ->
                        let! value = resolver ctx.Source arguments
                        return innerCtor value
                    | Error errors -> return ResultValue (Error (errors |> List.map unbox<_>))
                | None ->
                    let! value = resolver ctx.Source arguments
                    return innerCtor value
            }
        ) field

type Resolve internal () =
    member __.context (resolver: ResolveContext<'source> -> 'field ValueTask) = resolveContext false ValueValue resolver
    member __.contextOption (resolver: ResolveContext<'source> -> 'field option ValueTask) = resolveContext true OptionValue resolver
    member __.contextVOption (resolver: ResolveContext<'source> -> 'field voption ValueTask) = resolveContext true ValueOptionValue resolver
    member __.contextResult (resolver: ResolveContext<'source> -> Result<'field, 'error list> ValueTask) = resolveContext true ResultValue resolver

    member __.property ([<ReflectedDefinition true>] expr: Expr<'source -> 'field ValueTask>) = resolveProperty false ValueValue expr
    member __.propertyOption ([<ReflectedDefinition true>] expr: Expr<'source -> 'field option ValueTask>) = resolveProperty true OptionValue expr
    member __.propertyVOption ([<ReflectedDefinition true>] expr: Expr<'source -> 'field voption ValueTask>) = resolveProperty true ValueOptionValue expr
    member __.propertyResult ([<ReflectedDefinition true>] expr: Expr<'source -> Result<'field, 'error list> ValueTask>) = resolveProperty true ResultValue expr

    member __.method ([<ReflectedDefinition true>] expr: Expr<'source -> 'arguments -> 'field ValueTask>) = resolveMethod false ValueValue expr
    member __.methodOption ([<ReflectedDefinition true>] expr: Expr<'source -> 'arguments -> 'field option ValueTask>) = resolveMethod true OptionValue expr
    member __.methodVOption ([<ReflectedDefinition true>] expr: Expr<'source -> 'arguments -> 'field voption ValueTask>) = resolveMethod true ValueOptionValue expr
    member __.methodResult ([<ReflectedDefinition true>] expr: Expr<'source -> 'arguments -> Result<'field, 'error list> ValueTask>) = resolveMethod true ResultValue expr

    member __.endpoint (resolver: 'arguments -> 'field ValueTask) = resolveEndpoint false ValueValue resolver
    member __.endpointOption (resolver: 'arguments -> 'field option ValueTask) = resolveEndpoint true OptionValue resolver
    member __.endpointVOption (resolver: 'arguments -> 'field voption ValueTask) = resolveEndpoint true ValueOptionValue resolver
    member __.endpointResult (resolver: 'arguments -> Result<'field, 'error list> ValueTask) = resolveEndpoint true ResultValue resolver

let resolve = Resolve ()

exception IncorrectFieldTypeException of Type: Type

let internal ensureCorrectType () = Operation.CreateUnit Priority.EnsureCorrectType <| fun (_: Field<'field, _, _>) ->
    match typeof<'field> with
    | OptionType _
    | ValueOptionType _
    | ResultType _
    | NullableType _
    | ObservableType _
    | TaskType _
    | ValueTaskType _ -> raise (IncorrectFieldTypeException typeof<'field>)
    | _ -> ()
