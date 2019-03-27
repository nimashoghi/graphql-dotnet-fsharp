module GraphQL.FSharp.Builder.Utils

open System
open System.Reflection
open System.Threading.Tasks
open FSharp.Quotations
open FSharp.Utils.Quotations
open FSharp.Utils.Tasks
open FSharp.Reflection
open GraphQL.Resolvers
open GraphQL.Types

open GraphQL.FSharp.Inference
open GraphQL.FSharp.Resolvers
open GraphQL.FSharp.Types
open GraphQL.FSharp.Utils

let withSource f (ctx: ResolveContext<_>) = f ctx.Source
let isInvalidType (``type``: IGraphType) = isNull ``type`` || Object.ReferenceEquals (``type``, invalidGraphType)

let inline makeNullable (x: ^t) =
    match Option.ofObj (^t: (member ResolvedType: IGraphType) x) with
    | Some graph when (graph :? NonNullGraphType) ->
        let nonNull = graph :?> NonNullGraphType
        (^t: (member set_ResolvedType: IGraphType -> unit) x, nonNull.ResolvedType)
    | _ -> ()

module Field =
    let setResolver resolver (field: Field<'field, 'arguments, 'source>) =
        field.Resolver <- resolver
        field

    let addArguments (field: Field<'field, 'arguments, 'source>) =
        if typeof<'arguments> = typeof<obj> then field else

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

        field

    let makeArgumentArray<'arguments, 'source> (fields: PropertyInfo [] Lazy) (ctx: ResolveContext<'source>) =
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

    // TODO: Take a look at null
    let makeArgumentRecord<'arguments> (constructor: (obj [] -> obj) Lazy) (arguments: obj [])  =
        if typeof<'arguments> = typeof<obj> then unbox<'arguments> null else

        let (Lazy constructor) = constructor

        arguments
        |> constructor
        |> unbox<'arguments>

    let makeArgumentArrayFromRecord (Lazy reader) (record: 'arguments) =
        if typeof<'arguments> = typeof<obj> then [||] else
        reader (box record)

    let makeArguments<'arguments, 'source> fields constructor ctx =
        if typeof<'arguments> = typeof<obj> then unbox<'arguments> null else

        makeArgumentArray<'arguments, 'source> fields ctx
        |> makeArgumentRecord<'arguments> constructor

    let getRecordInfo<'arguments> () =
        let fields = lazy (FSharpType.GetRecordFields typeof<'arguments>)
        let constructor = lazy (FSharpValue.PreComputeRecordConstructor typeof<'arguments>)
        let reader = lazy (FSharpValue.PreComputeRecordReader typeof<'arguments>)
        fields, constructor, reader

    let validateField
        (validator: 'arguments -> Result<'arguments, 'error list> Task)
        (field: Field<'field, 'arguments, 'source>) =
        let fields, constructor, reader = getRecordInfo<'arguments> ()
        let oldResolver = field.Resolver :?> AsyncResolver<'source, 'field>
        field.Resolver <-
            resolveAsync (
                fun (ctx: ResolveContext<'source>) -> task {
                    let argumentArray = makeArgumentArray<'arguments, 'source> fields ctx
                    let argumentRecord = makeArgumentRecord<'arguments> constructor argumentArray
                    let! validatedArguments = validator argumentRecord

                    match validatedArguments with
                    | Ok arguments ->
                        let validatedArgumentArray = makeArgumentArrayFromRecord reader arguments
                        let (Lazy fields) = fields
                        Array.zip validatedArgumentArray fields
                        |> Array.iter (fun (value, prop) -> ctx.Arguments.[prop.Name] <- value)
                        return! oldResolver.Resolver ctx
                    | Error errors ->
                        errors
                        |> List.map (box >> string >> GraphQL.ExecutionError)
                        |> ctx.Errors.AddRange
                        return null
                }
            )
        makeNullable field
        field

    let validateSubscription
        (validator: 'arguments -> Result<'arguments, 'error list> Task)
        (field: Field<'field, 'arguments, 'source>) =
        let fields, constructor, reader = getRecordInfo<'arguments> ()
        let oldSubscriber = field.AsyncSubscriber :?> AsyncStreamResolver<'source, 'field>
        field.AsyncSubscriber <-
            resolveSubscriberAsync (
                fun (ctx: ResolveContext<'source>) -> task {
                    let argumentArray = makeArgumentArray<'arguments, 'source> fields ctx
                    let argumentRecord = makeArgumentRecord<'arguments> constructor argumentArray
                    let! validatedArguments = validator argumentRecord

                    match validatedArguments with
                    | Ok arguments ->
                        let validatedArgumentArray = makeArgumentArrayFromRecord reader arguments
                        let (Lazy fields) = fields
                        Array.zip validatedArgumentArray fields
                        |> Array.iter (fun (value, prop) -> ctx.Arguments.[prop.Name] <- value)
                        return! oldSubscriber.Resolver ctx
                    | Error errors ->
                        errors
                        |> List.map (box >> string >> GraphQL.ExecutionError)
                        |> ctx.Errors.AddRange
                        return null
                }
            )
        makeNullable field
        field

    let (|NormalField|SubsriptionField|) (field: Field<_, _, _>) =
        if (not << isNull) field.AsyncSubscriber
        then SubsriptionField
        else NormalField

    let validate validator field =
        match field with
        | NormalField -> validateField validator field
        | SubsriptionField -> validateSubscription validator field

    let resolveMethod resolver (f: 'source -> 'arguments -> _) =
        let fields, constructor, _ = getRecordInfo<'arguments>()
        resolver (
            fun (ctx: ResolveContext<'source>) ->
                f ctx.Source (makeArguments<'arguments, 'source> fields constructor ctx)
        )

    let resolveEndpoint resolver (f: 'arguments -> _) =
        let fields, constructor, _ = getRecordInfo<'arguments>()
        resolver (
            fun (ctx: ResolveContext<'source>) ->
                f (makeArguments<'arguments, 'source> fields constructor ctx)
        )

    let resolveCtxMethodAsync resolver (f: ResolveContext<'source> -> 'arguments -> _) =
        let fields, constructor, _ = getRecordInfo<'arguments> ()
        resolver (
            fun (ctx: ResolveContext<'source>) ->
                f ctx (makeArguments<'arguments, 'source> fields constructor ctx)
        )

    let setField (|FieldName|_|) resolver (expr: Expr<_ -> _>) (field: Field<_, _, _>) =
        let f, name =
            match expr with
            | WithValueTyped (f, expr) ->
                match expr with
                | FieldName name -> f, Some name
                | _ -> f, None
            | _ -> invalidArg "setField" "The expression passed to setField must have a value with it!"

        if isNull field.Name || field.Name = ""
        then Option.iter field.set_Name name

        field.Resolver <- resolver f
        field

    let setFieldSubscriber (|FieldName|_|) resolver (expr: Expr<_ -> _>) (field: Field<_, _, _>) =
        let f, name =
            match expr with
            | WithValueTyped (f, expr) ->
                match expr with
                | FieldName name -> f, Some name
                | _ -> f, None
            | _ -> invalidArg "setField" "The expression passed to setField must have a value with it!"

        if isNull field.Name || field.Name = ""
        then Option.iter field.set_Name name

        field.AsyncSubscriber <- resolver f
        field

    let setSubscriber subscriber (field: Field<'field, 'arguments, 'source>) =
        field.AsyncSubscriber <- subscriber
        field

    let setSubscriptionResolver (field: Field<'field, 'arguments, 'source>) =
        field.Resolver <- FuncFieldResolver<_> (fun ctx -> ctx.Source)
        field

module Schema =
    let abstractClasses ``type`` =
        let rec run (``type``: Type) = [
            let baseType = ``type``.BaseType
            if not (isNull baseType)
            && baseType <> typeof<obj>
            && baseType.IsAbstract then
                yield baseType
                yield! run baseType
        ]
        run ``type``

    let baseTypes (``type``: Type) = [
        yield! ``type``.GetInterfaces ()
        yield! abstractClasses ``type``
    ]

    let extends (object: Type) (``base``: Type) =
        baseTypes object
        |> List.exists ((=) ``base``)

    let handleInterfaces (types: IGraphType list) =
        let objects = types |> List.choose (|ObjectGraphType|_|)
        let interfaces = types |> List.choose (|InterfaceGraphType|_|)
        List.allPairs objects interfaces
        |> List.filter (fun ((_, object), (_, ``interface``)) -> extends object ``interface``)
        |> List.iter (
            fun ((object, _), (``interface``, _)) ->
                ``interface``.AddPossibleType object
                object.AddResolvedInterface ``interface``
        )

        types
