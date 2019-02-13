module GraphQL.FSharp.BuilderField

open System
open System.Threading.Tasks
open FSharp.Quotations
open GraphQL.Types
open GraphQL.Subscription
open GraphQL.Resolvers

open GraphQL.FSharp
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

// TODO: Add support for field.Type as well as field.ResolvedType
let handleNonNullTypes (field: TypedFieldType<_>) =
    if not <| isNull field.DefaultValue then
        match field.ResolvedType with
        | :? NonNullGraphType as ``type`` ->
            field.ResolvedType <- ``type``.ResolvedType
        | _ -> ()
    field

let createNewField<'source> () = TypedFieldType<'source> ()
let createNewEndpoint<'source> name = TypedFieldType<'source> (Name = name)
let createNewFieldOf ``type`` = TypedFieldType<obj> (ResolvedType = ``type``)

type FieldBulderOperation<'source> = TypedFieldType<'source> -> TypedFieldType<'source>
type FieldBulderState<'source> = FieldBulderOperation<'source> list

type FieldBuilderBase<'source> () =
    inherit ConfigurableBuilder<TypedFieldType<'source>> ()

    member __.Yield (_: unit) = [] : FieldBulderState<'source>

    [<CustomOperation "name">]
    member __.CustomOperation_Name (state: FieldBulderState<'source>, name) =
        state
        |> operation (setName name)

    [<CustomOperation "description">]
    member __.CustomOperation_Description (state: FieldBulderState<'source>, description) =
        state
        |> operation (setDescription description)

    [<CustomOperation "deprecationReason">]
    member __.CustomOperation_DeprecationReason (state: FieldBulderState<'source>, deprecationReason) =
        state
        |> operation (setDeprecationReason deprecationReason)

    [<CustomOperation "metadata">]
    member __.CustomOperation_Metadata (state: FieldBulderState<'source>, metadata) =
        state
        |> operation (setMetadata metadata)

    [<CustomOperation "ofType">]
    member __.CustomOperation_Type (state: FieldBulderState<'source>, ``type``) =
        state
        |> unitOperation (fun this -> this.Type <- ``type``)

    member __.CustomOperation_Type (state: FieldBulderState<'source>, ``type``) =
        state
        |> unitOperation (fun this -> this.ResolvedType <- ``type``)

    member __.CustomOperation_Type (state: FieldBulderState<'source>, ``type``) =
        state
        |> unitOperation (fun this -> this.ResolvedType <- ``type`` ())

    [<CustomOperation "defaultValue">]
    member __.CustomOperation_DefaultValue (state: FieldBulderState<'source>, value) =
        state
        |> operation (setDefaultValue value)

    [<CustomOperation "arguments">]
    member __.CustomOperation_Arguments (state: FieldBulderState<'source>, arguments) =
        state
        |> operation (fun this ->
            let arguments = QueryArguments (List.toSeq arguments)
            setArguments arguments this
        )

    [<CustomOperation "get">]
    member __.CustomOperation_Get (state: FieldBulderState<'source>, [<ReflectedDefinition true>] expr: Expr<'source -> 'field>) =
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
    member __.CustomOperation_GetAsync (state: FieldBulderState<'source>, [<ReflectedDefinition true>] expr: Expr<'source -> Task<'field>>) =
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
    member __.CustomOperation_Resolve (state: FieldBulderState<'source>, resolver: ResolveFieldContext<'source> -> 'field) =
        state
        |> operation (fun this ->
            this.Resolver <- resolve resolver

            this
            |> setFieldType<'field, 'source>
        )

    [<CustomOperation "resolveAsync">]
    member __.CustomOperation_ResolveAsync (state: FieldBulderState<'source>, resolver: ResolveFieldContext<'source> -> Task<'field>) =
        state
        |> operation (fun this ->
            this.Resolver <- resolveAsync resolver

            this
            |> setFieldType<'field, 'source>
        )

    [<CustomOperation "subscribe">]
    member __.CustomOperation_Subscribe (state: FieldBulderState<'source>, subscribe: ResolveEventStreamContext<'source> -> IObservable<'field>) =
        state
        |> operation (fun this ->
            this.Subscriber <- EventStreamResolver<_, _> (Func<_, _> subscribe)

            this
            |> setFieldType<'field, 'source>
            |> resolverForSubscription<'field, 'source>
        )

    [<CustomOperation "subscribeAsync">]
    member __.CustomOperation_SubscribeAsync (state: FieldBulderState<'source>, subscribe: ResolveEventStreamContext<'source> -> Task<IObservable<'field>>) =
        state
        |> operation (fun this ->
            this.AsyncSubscriber <- AsyncEventStreamResolver<_, _> (Func<_, _> subscribe)

            this
            |> setFieldType<'field, 'source>
            |> resolverForSubscription<'field, 'source>
        )

type FieldBuilder<'source> (?``type``, ?name, ?value) =
    inherit FieldBuilderBase<'source> ()

    member __.Run (state: FieldBulderState<'source>) =
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

    member __.Run (state: FieldBulderState<'source>) = apply (handleNonNullTypes :: state)
