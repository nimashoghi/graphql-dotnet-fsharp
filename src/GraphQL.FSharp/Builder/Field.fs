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

[<Literal>]
let HasDefaultValueMetadataName = "HasDefaultValue"

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

let hasDefaultValue (field: TypedFieldType<_>) =
    match field.Metadata.TryGetValue HasDefaultValueMetadataName with
    | true, value when unbox<bool> value -> true
    | _ -> false

// TODO: Add support for field.Type as well as field.ResolvedType
let handleNonNullTypes (field: TypedFieldType<_>) =
    if hasDefaultValue field then
        match field.ResolvedType with
        | :? NonNullGraphType as ``type`` ->
            field.ResolvedType <- ``type``.ResolvedType
        | _ -> ()
    field

let createNewField<'source> () = TypedFieldType<'source> ()
let createNewEndpoint<'source> name = TypedFieldType<'source> (Name = name)
let createNewFieldOf ``type`` = TypedFieldType<obj> (ResolvedType = ``type``)

type FieldBuilder<'source> (?``type``, ?name, ?value) =
    member __.Yield (_: unit) =
        value
        |> Option.defaultValue (
            TypedFieldType<'source> (
                ResolvedType = Option.toObj ``type``,
                Name = Option.toObj name
            )
        )

    [<CustomOperation "name">]
    member __.CustomOperation_Name (this: TypedFieldType<'source>, name) =
        setName name this

    [<CustomOperation "description">]
    member __.CustomOperation_Description (this: TypedFieldType<'source>, description) =
        setDescription description this

    [<CustomOperation "deprecationReason">]
    member __.CustomOperation_DeprecationReason (this: TypedFieldType<'source>, deprecationReason) =
        setDeprecationReason deprecationReason this

    [<CustomOperation "metadata">]
    member __.CustomOperation_Metadata (this: TypedFieldType<'source>, metadata) =
        setMetadata metadata this

    [<CustomOperation "ofType">]
    member __.CustomOperation_Type (this: TypedFieldType<'source>, ``type``) =
        this.Type <- ``type``
        this

    member __.CustomOperation_Type (this: TypedFieldType<'source>, ``type``) =
        this.ResolvedType <- ``type``
        this

    member __.CustomOperation_Type (this: TypedFieldType<'source>, ``type``) =
        this.ResolvedType <- ``type`` ()
        this

    [<CustomOperation "defaultValue">]
    member __.CustomOperation_DefaultValue (this: TypedFieldType<'source>, ``default``: 'field) =
        this.DefaultValue <- box ``default``
        this.Metadata.[HasDefaultValueMetadataName] <- true

        this
        |> setFieldType<'field, 'source>

    [<CustomOperation "arguments">]
    member __.CustomOperation_Arguments (this: TypedFieldType<'source>, arguments) =
        this.Arguments <- QueryArguments (List.toSeq arguments)
        this

    [<CustomOperation "get">]
    member __.CustomOperation_Get (this: TypedFieldType<'source>, [<ReflectedDefinition true>] expr: Expr<'source -> 'field>) =
        let fieldName, getterFn =
            match expr with
            | WithValueTyped (getterFn, FieldName value) -> value, getterFn
            | _ -> invalidArg "getter" "Could not find field name and/or getter function from async getter expression. Get only supports simple expressions. Use resolve instead."

        this.Name <- fieldName
        this.Resolver <- (withSource >> resolve) getterFn

        this
        |> setFieldType<'field, 'source>

    [<CustomOperation "getAsync">]
    member __.CustomOperation_GetAsync (this: TypedFieldType<'source>, [<ReflectedDefinition true>] expr: Expr<'source -> Task<'field>>) =
        let fieldName, getterFn =
            match expr with
            | WithValueTyped (getterFn, AsyncFieldName value) -> value, getterFn
            | _ -> invalidArg "getter" "Could not find field name and/or getter function from async getter expression. GetAsync only supports simple expressions. Use resolve instead."

        this.Name <- fieldName
        this.Resolver <- (withSource >> resolveAsync) getterFn

        this
        |> setFieldType<'field, 'source>

    [<CustomOperation "resolve">]
    member __.CustomOperation_Resolve (this: TypedFieldType<'source>, resolver: ResolveFieldContext<'source> -> 'field) =
        this.Resolver <- resolve resolver

        this
        |> setFieldType<'field, 'source>

    [<CustomOperation "resolveAsync">]
    member __.CustomOperation_ResolveAsync (this: TypedFieldType<'source>, resolver: ResolveFieldContext<'source> -> Task<'field>) =
        this.Resolver <- resolveAsync resolver

        this
        |> setFieldType<'field, 'source>

    [<CustomOperation "subscribe">]
    member __.CustomOperation_Subscribe (this: TypedFieldType<'source>, subscribe: ResolveEventStreamContext<'source> -> IObservable<'field>) =
        this.Subscriber <- EventStreamResolver<_, _> (Func<_, _> subscribe)

        this
        |> setFieldType<'field, 'source>
        |> resolverForSubscription<'field, 'source>

    [<CustomOperation "subscribeAsync">]
    member __.CustomOperation_SubscribeAsync (this: TypedFieldType<'source>, subscribe: ResolveEventStreamContext<'source> -> Task<IObservable<'field>>) =
        this.AsyncSubscriber <- AsyncEventStreamResolver<_, _> (Func<_, _> subscribe)

        this
        |> setFieldType<'field, 'source>
        |> resolverForSubscription<'field, 'source>


    member __.Run (this: TypedFieldType<'source>) =
        (logField >> Logger.information) this

        this
        |> handleNonNullTypes
