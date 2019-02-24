namespace GraphQL.FSharp

open System
open System.Threading.Tasks
open FSharp.Quotations
open GraphQL.Conversion
open GraphQL.Resolvers
open GraphQL.Subscription
open GraphQL.Types

open GraphQL.FSharp.BuilderBase
open GraphQL.FSharp.BuilderUtils
open GraphQL.FSharp.Resolvers
open GraphQL.FSharp.Types
open GraphQL.FSharp.Utils

type ArgumentBuilder<'t> (``type``) =
    inherit TypedEntityBuilder<Argument<'t>> ()

    member __.Yield (_: unit) =
        Argument<'t> (?``type`` = Option.ofObj ``type``)

type DirectiveBuilder () =
    inherit EntityBuilder<Directive> ()

    member __.Yield (_: unit) = Directive ()

    [<CustomOperation "arguments">]
    member __.Arguments (state: Directive, arguments) =
        setArguments arguments state

    [<CustomOperation "locations">]
    member __.Locations (state: Directive, locations) =
        state.PossibleLocations <- locations
        state

[<AutoOpen>]
module internal EnumHelpers =
    open FSharp.Reflection

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

type EnumerationBuilder () =
    inherit BasicGraphTypeBuilder<Enumeration> ()

    member __.auto<'source> (?Name, ?Description, ?Metadata) =
        let graph: Enumeration<'source> =
            Enumeration<'source> (
                Name = typeof<'source>.Name
            )

        addEnumValues graph

        Name |> Option.iter (fun name -> graph.Name <- name)
        Description |> Option.iter (fun description -> graph.Description <- description)
        Metadata |> Option.iter (fun metadata -> graph.Metadata <- dict metadata)

        graph

    member __.Yield (_: unit) = Enumeration ()

    [<CustomOperation "cases">]
    member __.Cases (state: Enumeration, values) =
        values
        |> List.iter state.AddValue

        state

[<AutoOpen>]
module FieldHelpers =
    open FSharp.Reflection
    open GraphQL.FSharp.Inference

    let addArguments<'arguments, 'field, 'source> (field: Field<'field, 'source>) =
        if typeof<'arguments> = typeof<obj> then field else

        assert (Option.isSome <| (|AnonymousType|_|) typeof<'arguments>)

        if isNull field.Arguments
        then field.Arguments <- QueryArguments ()

        FSharpType.GetRecordFields typeof<'arguments>
        |> Array.map (fun field ->
            Argument (
                ``type`` = createReference field.PropertyType,
                Name = field.Name
            )
        )
        |> Array.iter field.Arguments.Add

        field

    let makeArguments<'arguments, 'source> (ctx: ResolveContext<'source>) =
        if typeof<'arguments> = typeof<obj> then unbox<'arguments> null else

        assert (Option.isSome <| (|AnonymousType|_|) typeof<'arguments>)

        let fields =
            FSharpType.GetRecordFields typeof<'arguments>
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

        FSharpValue.MakeRecord (
            recordType = typeof<'arguments>,
            values = fields
        )
        |> unbox<'arguments>

    let resolveMethod (f: 'source -> 'arguments -> 'field) =
        resolve (
            fun (ctx: ResolveContext<'source>) ->
                f ctx.Source (makeArguments<'arguments, 'source> ctx)
        )

    let resolveCtxMethod (f: ResolveContext<'source> -> 'arguments -> 'field) =
        resolve (
            fun (ctx: ResolveContext<'source>) ->
                f ctx (makeArguments<'arguments, 'source> ctx)
        )

    let resolveCtxMethodAsync (f: ResolveContext<'source> -> 'arguments -> Task<'field>) =
        resolveAsync (
            fun (ctx: ResolveContext<'source>) ->
                f ctx (makeArguments<'arguments, 'source> ctx)
        )

    let setField (|FieldName|_|) resolver (state: Field<_, _>) (expr: Expr<_ -> _>) =
        let f, name =
            match expr with
            | WithValueTyped (f, expr) ->
                match expr with
                | FieldName name -> f, Some name
                | _ -> f, None
            | _ -> invalidArg "setField" "The expression passed to setField must have a value with it!"

        match name, state.Name with
        | Some name, stateName when (isNull stateName || stateName = "") -> state.Name <- name
        | _ -> ()

        state.Resolver <- resolver f

        state

    let withSource f (ctx: ResolveContext<_>) = f ctx.Source

type FieldBuilder<'field, 'source> (``type``, ?name) =
    inherit TypedFieldBuilder<Field<'field, 'source>> ()

    member __.Yield (_: unit) =
        Field<'field, 'source> (
            ?``type`` = Option.ofObj ``type``,
            Name = Option.toObj name
        )

    [<CustomOperation "arguments">]
    member __.Arguments (state: Field<'field, 'source>, arguments) =
        setArguments (QueryArguments (List.toSeq arguments)) state

    [<CustomOperation "prop">]
    member __.Property (state: Field<'field, 'source>, [<ReflectedDefinition true>] expr: Expr<'source -> 'field>) =
        setField (|FieldName|_|) (withSource >> resolve) state expr
        |> Field.setType<'field, 'source>

    [<CustomOperation "propAsync">]
    member __.PropertyAsync (state: Field<'field, 'source>, [<ReflectedDefinition true>] expr: Expr<'source -> Task<'field>>) =
        setField (|FieldName|_|) (withSource >> resolveAsync) state expr
        |> Field.setType<'field, 'source>

    [<CustomOperation "method">]
    member __.Method (state: Field<'field, 'source>, [<ReflectedDefinition true>] expr: Expr<'source -> 'arguments -> 'field>) =
        setField (|MethodName|_|) resolveMethod state expr
        |> addArguments<'arguments, 'field, 'source>
        |> Field.setType<'field, 'source>

    [<CustomOperation "methodAsync">]
    member __.MethodAsync (state: Field<'field, 'source>, [<ReflectedDefinition true>] expr: Expr<'source -> 'arguments -> Task<'field>>) =
        setField (|MethodName|_|) resolveMethod state expr
        |> addArguments<'arguments, 'field, 'source>
        |> Field.setType<'field, 'source>

    [<CustomOperation "resolve">]
    member __.Resolve (state: Field<'field, 'source>, resolver: ResolveContext<'source> -> 'arguments -> 'field) =
        state.Resolver <- resolveCtxMethod resolver

        state
        |> addArguments<'arguments, 'field, 'source>
        |> Field.setType<'field, 'source>

    [<CustomOperation "resolveAsync">]
    member __.ResolveAsync (state: Field<'field, 'source>, resolver: ResolveContext<'source> -> 'arguments -> Task<'field>) =
        state.Resolver <- resolveCtxMethodAsync resolver

        state
        |> addArguments<'arguments, 'field, 'source>
        |> Field.setType<'field, 'source>

    [<CustomOperation "subscribe">]
    member __.Subscribe (state: Field<'field, 'source>, subscribe: ResolveEventStreamContext<'source> -> IObservable<'field>) =
        state.Subscriber <- EventStreamResolver<_, _> (Func<_, _> subscribe)
        state.Resolver <- resolve (fun (ctx: ResolveContext<'source>) -> unbox<'field> ctx.Source)

        state
        |> Field.setType<'field, 'source>

    [<CustomOperation "subscribeAsync">]
    member __.SubscribeAsync (state: Field<'field, 'source>, subscribe: ResolveEventStreamContext<'source> -> Task<IObservable<'field>>) =
        state.AsyncSubscriber <- AsyncEventStreamResolver<_, _> (Func<_, _> subscribe)
        state.Resolver <- resolve (fun (ctx: ResolveContext<'source>) -> unbox<'field> ctx.Source)

        state
        |> Field.setType<'field, 'source>

type InputObjectBuilder<'source> () =
    inherit ComplexGraphTypeBuilder<InputObject<'source>, 'source> ()

    member __.Yield (_: unit) = InputObject<'source> (Name = typeof<'source>.Name)

type InterfaceBuilder<'source> () =
    inherit ComplexGraphTypeBuilder<Interface<'source>, 'source> ()

    member __.Yield (_: unit) = Interface<'source> (Name = typeof<'source>.Name)

type ObjectBuilder<'source> () =
    inherit ComplexGraphTypeBuilder<Object<'source>, 'source> ()

    member __.Yield (_: unit) = Object<'source> (Name = typeof<'source>.Name)

    [<CustomOperation "interfaces">]
    member __.Interfaces (state: Object<'source>, interfaces) =
        interfaces
        |> List.iter state.AddResolvedInterface

        state

type SchemaBuilder () =
    inherit ConfigureBuilder<Schema> ()

    member __.Yield (_: unit) =
        new Schema (
            FieldNameConverter =
                {
                    new IFieldNameConverter with
                        member __.NameFor (name, _) = name
                }
        )

    [<CustomOperation "query">]
    member __.Query (state: Schema, query: Query) =
        state.Query <- query
        state

    [<CustomOperation "mutation">]
    member __.Mutation (state: Schema, mutation: Mutation) =
        state.Mutation <- mutation
        state

    [<CustomOperation "subscription">]
    member __.Subscription (state: Schema, subscription: Subscription) =
        state.Subscription <- subscription
        state

    [<CustomOperation "types">]
    member __.Types (state: Schema, types) =
        types
        |> Schema.handleInterfaces
        |> List.toArray
        |> state.RegisterTypes

        state


[<AutoOpen>]
module internal UnionUtils =
    open System.Reflection
    open FSharp.Reflection
    open GraphQL.FSharp.Inference

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

type UnionBuilder () =
    inherit BasicGraphTypeBuilder<Union> ()

    member __.auto<'source> (?Name, ?Description, ?Metadata) =
        let graph: Union<'source> =
            Union<'source> (
                Name = typeof<'source>.Name
            )

        addPossibleTypes graph

        Name |> Option.iter (fun name -> graph.Name <- name)
        Description |> Option.iter (fun description -> graph.Description <- description)
        Metadata |> Option.iter (fun metadata -> graph.Metadata <- dict metadata)

        graph

    member __.Yield (_: unit) = Union ()

    [<CustomOperation "cases">]
    member __.Cases (state: Union, cases) =
        state.PossibleTypes <- List.toSeq cases

        state
