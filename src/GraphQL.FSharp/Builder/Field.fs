module GraphQL.FSharp.BuilderField

open System
open System.Threading.Tasks
open FSharp.Quotations
open GraphQL.Types
open GraphQL.Subscription
open GraphQL.Resolvers

open GraphQL.FSharp.BuilderBase
open GraphQL.FSharp.Inference
open GraphQL.FSharp.Resolvers
open GraphQL.FSharp.Types
open GraphQL.FSharp.Utils

let resolverForSubscription<'field, 'source> (field: TypedFieldType<'source>) =
    field.Resolver <-
        FuncFieldResolver<_, _> (
            Func<_, _> (fun (ctx: ResolveFieldContext<'source>) ->
                unbox<'field> ctx.Source
            )
        )

    field

let setFieldType<'field, 'source> (field: TypedFieldType<'source>) =
    if isNull field.ResolvedType
    then field.ResolvedType <- createReference typeof<'field>
    field

let setDefaultValue (value: 'field) (field: TypedFieldType<'source>) =
    field.DefaultValue <- box value
    field
    |> setFieldType<'field, 'source>

let internal getResovledType (field: TypedFieldType<_>) =
    field.ResolvedType
    |> Option.ofObj
    |> Option.orElse (
        field.Type
        |> Option.ofObj
        |> Option.bind (fun ``type`` ->
            match Activator.CreateInstance ``type`` with
            | :? IGraphType as graphType -> Some graphType
            | _ -> None
        )
    )

let handleNonNullTypes (field: TypedFieldType<_>) =
    if not <| isNull field.DefaultValue then
        match getResovledType field with
        | Some nonNull when (nonNull :? NonNullGraphType) ->
            field.ResolvedType <- (nonNull :?> NonNullGraphType).ResolvedType
        | _ -> ()
    field

let createNewField<'source> () = TypedFieldType<'source> ()
let createNewEndpoint<'source> name = TypedFieldType<'source> (Name = name)
let createNewFieldOf ``type`` = TypedFieldType<obj> (ResolvedType = ``type``)

type FieldBuilderBase<'source> () =
    inherit TypedFieldBuilder<TypedFieldType<'source>> ()

    [<CustomOperation "arguments">]
    member __.Arguments (state: State<TypedFieldType<'source>>, arguments) =
        state
        |> operation (fun this ->
            let arguments = QueryArguments (List.toSeq arguments)
            setArguments arguments this
        )

    [<CustomOperation "get">]
    member __.Get (state: State<TypedFieldType<'source>>, [<ReflectedDefinition true>] expr: Expr<'source -> 'field>) =
        state
        |> operation (fun this ->
            let f, name =
                match expr with
                | WithValueTyped (f, FieldName name) -> f, name
                | _ -> invalidArg "getter" "Could not find field name and/or getter function from async getter expression. Get only supports simple expressions. Use resolve instead."

            this.Name <- name
            this.Resolver <- (withSource >> resolve) f

            this
            |> setFieldType<'field, 'source>
        )

    [<CustomOperation "getAsync">]
    member __.GetAsync (state: State<TypedFieldType<'source>>, [<ReflectedDefinition true>] expr: Expr<'source -> Task<'field>>) =
        state
        |> operation (fun this ->
            let f, name =
                match expr with
                | WithValueTyped (f, FieldName name) -> f, name
                | _ -> invalidArg "getter" "Could not find field name and/or getter function from async getter expression. GetAsync only supports simple expressions. Use resolveAsync instead."

            this.Name <- name
            this.Resolver <- (withSource >> resolveAsync) f

            this
            |> setFieldType<'field, 'source>
        )

    [<CustomOperation "resolve">]
    member __.Resolve (state: State<TypedFieldType<'source>>, resolver: ResolveFieldContext<'source> -> 'field) =
        state
        |> operation (fun this ->
            this.Resolver <- resolve resolver

            this
            |> setFieldType<'field, 'source>
        )

    [<CustomOperation "resolveAsync">]
    member __.ResolveAsync (state: State<TypedFieldType<'source>>, resolver: ResolveFieldContext<'source> -> Task<'field>) =
        state
        |> operation (fun this ->
            this.Resolver <- resolveAsync resolver

            this
            |> setFieldType<'field, 'source>
        )

    [<CustomOperation "subscribe">]
    member __.Subscribe (state: State<TypedFieldType<'source>>, subscribe: ResolveEventStreamContext<'source> -> IObservable<'field>) =
        state
        |> operation (fun this ->
            this.Subscriber <- EventStreamResolver<_, _> (Func<_, _> subscribe)

            this
            |> setFieldType<'field, 'source>
            |> resolverForSubscription<'field, 'source>
        )

    [<CustomOperation "subscribeAsync">]
    member __.SubscribeAsync (state: State<TypedFieldType<'source>>, subscribe: ResolveEventStreamContext<'source> -> Task<IObservable<'field>>) =
        state
        |> operation (fun this ->
            this.AsyncSubscriber <- AsyncEventStreamResolver<_, _> (Func<_, _> subscribe)

            this
            |> setFieldType<'field, 'source>
            |> resolverForSubscription<'field, 'source>
        )

type FieldBuilder<'source> (?``type``, ?name, ?value) =
    inherit FieldBuilderBase<'source> ()

    member __.Run (state: State<TypedFieldType<'source>>) =
        let state = handleNonNullTypes :: state
        value
        |> Option.defaultValue (
            TypedFieldType<'source> (
                ResolvedType = Option.toObj ``type``,
                Name = Option.toObj name
            )
        )
        |> apply state

type FieldEditBuilder<'source> () =
    inherit FieldBuilderBase<'source> ()

    member __.Run (state: State<TypedFieldType<'source>>) = apply (handleNonNullTypes :: state)
