module GraphQL.FSharp.Builder.Utils

open System
open System.Reflection
open System.Threading.Tasks
open FSharp.Quotations
open FSharp.Utils.Quotations
open FSharp.Utils.Tasks
open FSharp.Reflection
open GraphQL.Types

open GraphQL.FSharp.Inference
open GraphQL.FSharp.Resolvers
open GraphQL.FSharp.Types
open GraphQL.FSharp.Utils

let withSource f (ctx: ResolveContext<_>) = f ctx.Source

let inline makeNullable (x: ^t) =
    match Option.ofObj (^t: (member ResolvedType: IGraphType) x) with
    | Some graph when (graph :? NonNullGraphType) ->
        let nonNull = graph :?> NonNullGraphType
        (^t: (member set_ResolvedType: IGraphType -> unit) x, nonNull.ResolvedType)
    | _ -> ()

module Field =
    let addArguments<'arguments, 'field, 'source> (field: Field<'arguments, 'field, 'source>) =
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

    let makeArgumentRecord<'arguments> (constructor: (obj [] -> obj) Lazy) (arguments: obj [])  =
        if typeof<'arguments> = typeof<obj> then unbox<'arguments> null else

        let (Lazy constructor) = constructor

        arguments
        |> constructor
        |> unbox<'arguments>

    let makeArguments<'arguments, 'source> fields constructor ctx =
        if typeof<'arguments> = typeof<obj> then unbox<'arguments> null else

        makeArgumentArray<'arguments, 'source> fields ctx
        |> makeArgumentRecord<'arguments> constructor

    let getRecordInfo<'arguments> () =
        let fields = lazy (FSharpType.GetRecordFields typeof<'arguments>)
        let constructor = lazy (FSharpValue.PreComputeRecordConstructor typeof<'arguments>)
        fields, constructor

    let validate
        (validator: 'arguments -> Result<'arguments, 'error list> Task)
        (field: Field<'arguments, 'field, 'source>) =
        let fields, constructor = getRecordInfo<'arguments> ()
        // FIXME: This does not work with sync methods
        let oldResolver = field.Resolver :?> AsyncResolver<'source, 'field>
        field.Resolver <-
            resolveAsync (
                fun (ctx: ResolveContext<'source>) -> task {
                    let argumentArray = makeArgumentArray<'arguments, 'source> fields ctx
                    let argumentRecord = makeArgumentRecord<'arguments> constructor argumentArray
                    let! validatedArguments = validator argumentRecord

                    match validatedArguments with
                    | Ok _ ->
                        let (Lazy fields) = fields
                        Array.zip argumentArray fields
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

    let resolveMethod resolver (f: 'source -> 'arguments -> _) =
        let fields, constructor = getRecordInfo<'arguments>()
        resolver (
            fun (ctx: ResolveContext<'source>) ->
                f ctx.Source (makeArguments<'arguments, 'source> fields constructor ctx)
        )

    let resolveCtxMethodAsync resolver (f: ResolveContext<'source> -> 'arguments -> _) =
        let fields, constructor = getRecordInfo<'arguments> ()
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


module Schema =
    let abstractClasses ``type`` =
        let rec run (``type``: Type) = [
            let baseType = ``type``.BaseType
            if not(isNull baseType) && baseType <> typeof<obj> && baseType.IsAbstract then
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
        |> List.iter (fun ((object, _), (``interface``, _)) ->
            ``interface``.AddPossibleType object
            object.AddResolvedInterface ``interface``
        )

        types

module Union =
    let makePropField infer (prop: PropertyInfo) =
        Field (
            Name = prop.Name,
            Resolver = (withSource >> resolve) prop.GetValue,
            ResolvedType = infer prop.PropertyType
        )

    let addFields<'source>
        (case: UnionCaseInfo)
        (object: Object<obj>) =
        case.GetFields ()
        |> Array.map (makePropField createReference)
        |> Array.iter (object.AddField >> ignore)

        object

    let setIsTypeOf<'source>
        (case: UnionCaseInfo)
        (object: Object<obj>) =
        object.IsTypeOf <- (fun x ->
            match x with
            | :? 'source ->
                FSharpValue.GetUnionFields (x, typeof<'source>)
                |> (fun (case, _) -> case.Tag)
                |> ((=) case.Tag)
            | _ -> false)

        object

    let addTag (tag: int) (object: Object<obj>) =
        let field =
            EventStreamFieldType (
                Name = "Tag"
            )
        field.ResolvedType <- NonNullGraphType (IntGraphType ())
        field.Resolver <- resolve (fun _ -> tag)
        object.AddField field |> ignore

        object

    let makeUnionCase<'source> (case: UnionCaseInfo) =
        Object<obj> (
            Name = sprintf "%s%s" typeof<'source>.Name case.Name
        )
        |> addTag case.Tag
        |> addFields<'source> case
        |> setIsTypeOf<'source> case

    let addPossibleTypes (union: Union<'source>) =
        if typeof<'source> = typeof<obj> then () else

        assert (FSharpType.IsUnion typeof<'source>)

        // TODO: Start using FSharpValue.Precompute...
        let cases =
            FSharpType.GetUnionCases typeof<'source>
            |> Array.map(makeUnionCase<'source> >> (fun object -> object :> IObjectGraphType))

        union.PossibleTypes <- cases

module Enum =
    let addEnumValues (enum: Enumeration<'t>) =
        if typeof<'t>.IsEnum then
            Enum.GetNames typeof<'t>
            |> Array.map(fun name ->
                EnumValueDefinition (
                    Name = name,
                    Value = Enum.Parse (typeof<'t>, name)
                )
            )
            |> Array.iter enum.AddValue
        elif FSharpType.IsUnion typeof<'t> then
            FSharpType.GetUnionCases typeof<'t>
            |> Array.map(fun case ->
                if (not << Array.isEmpty) <| case.GetFields () then
                    failwith "Union case cannot have fields!"
                else
                    EnumValueDefinition(
                        Name = case.Name,
                        Value = FSharpValue.MakeUnion(case, [||])
                    )
            )
            |> Array.iter enum.AddValue
        else failwith "Invalid enum type!"

let inline setGraphType value (x: ^t) =
    (^t: (member set_GraphType: IGraphType -> unit) x, value)
    x

let isInvalidType (``type``: IGraphType) = isNull ``type`` || Object.ReferenceEquals (``type``, invalidGraphType)

let inline setType systemType (source: ^t) =
    let resolvedType = (^t: (member ResolvedType: IGraphType) source)
    let setResolvedType ``type`` = (^t: (member set_ResolvedType: IGraphType -> unit) (source, ``type``))
    if isInvalidType resolvedType then setResolvedType (createReference systemType)
    source

module Argument =
    let inline trySetType graphType systemType (x: ^t) =
        if graphType <> __
        then setGraphType graphType x
        else setType systemType x

    let makeArguments arguments =
        arguments
        |> List.map (fun arg -> arg :> QueryArgument)
        |> List.toArray
        |> QueryArguments
