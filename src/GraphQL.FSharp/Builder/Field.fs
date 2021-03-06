[<AutoOpen>]
module GraphQL.FSharp.Builder.Field

open System
open System.Collections.Generic
open System.Reactive.Linq
open System.Reflection
open System.Threading.Tasks
open FSharp.Quotations
open FSharp.Reflection
open FSharp.Utils
open FSharp.Utils.Quotations
open FSharp.Utils.Tasks
open GraphQL.Types

open GraphQL.FSharp.Inference
open GraphQL.FSharp.Resolvers
open GraphQL.FSharp.Types
open GraphQL.FSharp.Utils

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

open GraphQL.Resolvers

[<AutoOpen>]
module internal Arguments =
    let rec makeAnonymousType name (``type``: Type) =
        let obj =
            InputObject<obj> (
                Name = name
            )

        for prop in FSharpType.GetRecordFields ``type`` do
            Field<obj> (
                Name = prop.Name,
                ResolvedType = makeTypeOf (sprintf "%s%s" name prop.Name) prop.PropertyType,
                Resolver = FuncFieldResolver<obj> (fun ctx -> prop.GetValue ctx.Source)
            )
            |> obj.AddField
            |> ignore

        obj

    and makeTypeOf name (``type``: Type) =
        if isAnonymous ``type``
        then makeAnonymousType name ``type`` :> IGraphType
        else infer ``type``

    let addArguments () = Operation.ConfigureUnit <| fun (field: Field<'field, 'arguments, 'source>) ->
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
        |> Array.map (
            fun field ->
                Argument (
                    ResolvedType = makeTypeOf field.Name field.PropertyType,
                    Name = field.Name
                )
        )
        |> Array.iter field.Arguments.Add

    let internal (|Dict|_|) (object: obj) =
        match object with
        | :? Dictionary<string, obj> as dictionary when (not << isNull) dictionary -> Some dictionary
        | _ -> None

    exception NoArgumentFieldFoundException of Argument: string * Field: string

    // TODO: Change logic to use precomputaed stuff
    let rec internal getArgumentAnonymousRecord (dictionary: Dictionary<string, obj>) ``type`` =
        assert FSharpType.IsRecord ``type``

        let values =
            FSharpType.GetRecordFields ``type``
            |> Array.map (
                fun field ->
                    match dictionary.TryGetValue field.Name, field.PropertyType with
                    | (true, Dict value), (AnonymousRecord as ``type``) -> getArgumentAnonymousRecord value ``type``
                    | (true, value), _ -> value
                    | _ -> raise (NoArgumentFieldFoundException (``type``.Name, field.Name))
            )

        FSharpValue.MakeRecord (``type``, values)

    let internal getArgument (ctx: ResolveContext<'source>) name ``type`` =
        match ``type`` with
        | AnonymousRecord ->
            match ctx.GetArgument (typeof<Dictionary<string, obj>>, name, null) with
            | Dict dictionary -> getArgumentAnonymousRecord dictionary ``type``
            | _ -> ctx.GetArgument (``type``, name)
        | _ -> ctx.GetArgument (``type``, name)

    let makeArgumentArray<'arguments, 'source> (fields: PropertyInfo [] Lazy) (ctx: ResolveContext<'source>) =
        if typeof<'arguments> = typeof<obj> then [||] else

        let (Lazy fields) = fields
        fields
        |> Array.map (
            fun field ->
                getArgument ctx field.Name field.PropertyType
                // ctx.GetArgument (
                //     argumentType = field.PropertyType,
                //     name = field.Name
                // )
                // |> Option.ofObj
                // |> Option.orElseWith (fun () ->
                //     if not <| ctx.HasArgument field.Name
                //     then None
                //     else Some ctx.Arguments.[field.Name]
                // )
                // |> Option.toObj
            )

    let makeArgumentRecord<'arguments> (constructor: (obj [] -> obj) Lazy) (arguments: obj [])  =
        if typeof<'arguments> = typeof<obj> then Unchecked.unbox<'arguments> null else

        let (Lazy constructor) = constructor

        arguments
        |> constructor
        |> unbox<'arguments>

    let makeArguments<'arguments, 'source> fields constructor ctx =
        if typeof<'arguments> = typeof<obj> then Unchecked.unbox<'arguments> null else

        makeArgumentArray<'arguments, 'source> fields ctx
        |> makeArgumentRecord<'arguments> constructor

    let getRecordInfo<'arguments> () =
        let fields = lazy (FSharpType.GetRecordFields typeof<'arguments>)
        let constructor = lazy (FSharpValue.PreComputeRecordConstructor typeof<'arguments>)
        fields, constructor

let internal getValidator (field: Field<'field, 'arguments, 'source>) =
    if not <| field.HasMetadata "Validator" then None else

    field.Metadata.["Validator"]
    |> tryUnbox<Validator<'arguments>>
    |> Option.map (fun (Validator validator) -> validator)

let subscribeContext (resolver: ResolveContext<'source> -> 'arguments -> 'field IObservable ValueTask) = Operation.ConfigureUnit <| fun (field: Field<'field, 'arguments, obj>) ->
    let fields, constructor = getRecordInfo<'arguments> ()
    let validator = getValidator field
    field.AsyncSubscriber <-
        resolveSubscription (
            fun ctx ->
                let arguments = makeArguments<'arguments, _> fields constructor ctx
                let resolver = resolver ctx
                vtask {
                    match validator with
                    | Some validator ->
                        match! validator arguments with
                        | Ok arguments -> return! resolver arguments
                        | Error errors ->
                            errors
                            |> List.map handleError
                            |> ctx.Errors.AddRange
                            return Observable.Empty ()
                    | None -> return! resolver arguments
                }
        )
    field.Resolver <- resolve (fun ctx -> ctx.Source)

let subscribe (resolver: 'arguments -> 'field IObservable ValueTask) = subscribeContext (fun _ arguments -> resolver arguments)

let internal setName (|NamePattern|_|) expr = Operation.ConfigureUnit <| fun (field: Field<_, _, _>) ->
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

let inline internal resolveHelper (resolver: ResolveContext<'source> -> 'arguments -> 'field ValueTask) =
    let fields, constructor = getRecordInfo<'arguments> ()
    Operation.ConfigureUnit <| fun (field: Field<'field, 'arguments, 'source>) ->
        let validator = getValidator field
        field.Resolver <-
            resolveAsync (
                fun ctx ->
                    let resolver = resolver ctx
                    let arguments = makeArguments<'arguments, 'source> fields constructor ctx
                    vtask {
                        match validator with
                        | Some validator ->
                            match! validator arguments with
                            | Ok arguments -> return! resolver arguments
                            | Error errors ->
                                errors
                                |> List.map handleError
                                |> ctx.Errors.AddRange
                                return Unchecked.defaultof<_>
                        | None -> return! resolver arguments
                    }
            )

let inline internal resolveEndpoint resolver = resolveHelper (fun _ arguments -> resolver arguments)
let inline internal resolveEndpointContext resolver = resolveHelper (fun ctx arguments -> resolver ctx arguments)
let inline internal resolveMethod resolver = resolveHelper (fun ctx arguments -> resolver ctx.Source arguments)
let inline internal resolveProperty resolver = resolveHelper (fun ctx _ -> resolver ctx.Source)

type Resolve internal () =
    member __.context (resolver: ResolveContext<'source> -> 'field ValueTask) = resolveHelper (fun ctx _ -> resolver ctx)
    member __.endpoint (resolver: 'arguments -> 'field ValueTask) = resolveEndpoint resolver
    member __.endpointContext (resolver: ResolveContext<'source> -> 'arguments -> 'field ValueTask) = resolveEndpointContext resolver
    member __.method ([<ReflectedDefinition true>] expr: Expr<'source -> 'arguments -> 'field ValueTask>) =
        flatten [
            setName (|MethodName|_|) expr
            resolveMethod (getValue expr)
        ]
    member __.property ([<ReflectedDefinition true>] expr: Expr<'source -> 'field ValueTask>) =
        flatten [
            setName (|FieldName|_|) expr
            resolveProperty (getValue expr)
        ]

let resolve = Resolve ()
