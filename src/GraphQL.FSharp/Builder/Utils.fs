module internal GraphQL.FSharp.BuilderUtils

open System
open System.Reflection
open System.Threading.Tasks
open FSharp.Quotations
open FSharp.Reflection
open GraphQL.Types

open GraphQL.FSharp.BuilderBase
open GraphQL.FSharp.Inference
open GraphQL.FSharp.Resolvers
open GraphQL.FSharp.Types
open GraphQL.FSharp.Utils

let withSource f (ctx: ResolveContext<_>) = f ctx.Source

module Field =
    let addArguments<'arguments, 'field, 'source> (state: State<Field<'arguments, 'field, 'source>>) =
        let operation (field: Field<'arguments, 'field, 'source>) =
            if typeof<'arguments> = typeof<obj> then field else

            if isNull field.Arguments
            then field.Arguments <- QueryArguments ()

            FSharpType.GetRecordFields typeof<'arguments>
            |> Array.map (fun field ->
                Argument (
                    ResolvedType = createReference field.PropertyType,
                    Name = field.Name
                )
            )
            |> Array.iter field.Arguments.Add

            field

        operation @@ state

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

    let validate (validator: 'arguments -> Validation.Validation<'arguments, 'error>): Operation<Field<'arguments, 'field, 'source>> =
        let fields, constructor = getRecordInfo<'arguments> ()
        operation 10 (
            fun field ->
                let oldResolver = field.Resolver
                field.Resolver <-
                    resolve (
                        fun (ctx: ResolveContext<'source>) ->
                            let argumentArray = makeArgumentArray<'arguments, 'source> fields ctx
                            let argumentRecord = makeArgumentRecord<'arguments> constructor argumentArray
                            let validatedArguments = validator argumentRecord

                            match validatedArguments with
                            | Ok _ ->
                                let (Lazy fields) = fields
                                Array.zip argumentArray fields
                                |> Array.iter (fun (value, prop) -> ctx.Arguments.[prop.Name] <- value)

                                oldResolver.Resolve ctx.AsObjectContext
                            | Error errors ->
                                errors
                                |> List.map (box >> string >> GraphQL.ExecutionError)
                                |> ctx.Errors.AddRange

                                null
                    )
                makeNullable field
                field
        )

    let resolveMethod (f: 'source -> 'arguments -> 'field) =
        let fields, constructor = getRecordInfo<'arguments> ()
        resolve (
            fun (ctx: ResolveContext<'source>) ->
                f ctx.Source (makeArguments<'arguments, 'source> fields constructor ctx)
        )

    let resolveCtxMethod (f: ResolveContext<'source> -> 'arguments -> 'field) =
        let fields, constructor = getRecordInfo<'arguments> ()
        resolve (
            fun (ctx: ResolveContext<'source>) ->
                f ctx (makeArguments<'arguments, 'source> fields constructor ctx)
        )

    let resolveCtxMethodAsync (f: ResolveContext<'source> -> 'arguments -> Task<'field>) =
        let fields, constructor = getRecordInfo<'arguments> ()
        resolveAsync (
            fun (ctx: ResolveContext<'source>) ->
                f ctx (makeArguments<'arguments, 'source> fields constructor ctx)
        )

    let setField (|FieldName|_|) resolver (expr: Expr<_ -> _>) (state: State<Field<_, _, _>>) =
        let operation (field: Field<_, _, _>) =
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

        operation @@ state

module Schema =
    let abstractClasses ``type`` =
        let rec run (``type``: Type) = [
            let baseType = ``type``.BaseType
            if baseType <> null && baseType <> typeof<obj> && baseType.IsAbstract then
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
            |> Array.map makeUnionCase<'source>
            |> Array.map (fun object -> object :> IObjectGraphType)

        union.PossibleTypes <- cases

module internal Enum =
    let addEnumValues (enum: Enumeration<'t>) =
        if typeof<'t>.IsEnum then
            Enum.GetNames typeof<'t>
            |> Array.map (fun name ->
                EnumValueDefinition (
                    Name = name,
                    Value = Enum.Parse (typeof<'t>, name)
                )
            )
            |> Array.iter enum.AddValue
        elif FSharpType.IsUnion typeof<'t> then
            FSharpType.GetUnionCases typeof<'t>
            |> Array.map (fun case ->
                if (not << Array.isEmpty) <| case.GetFields () then
                    failwith "Union case cannot have fields!"
                else
                    EnumValueDefinition (
                        Name = case.Name,
                        Value = FSharpValue.MakeUnion (case, [||])
                    )
            )
            |> Array.iter enum.AddValue
        else failwith "Invalid enum type!"
