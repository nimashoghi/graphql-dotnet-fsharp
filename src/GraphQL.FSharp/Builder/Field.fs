[<AutoOpen>]
module GraphQL.FSharp.Builder.Field

open System
open System.Threading.Tasks
open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Linq.RuntimeHelpers
open GraphQL.Types
open GraphQL.Subscription
open GraphQL.Resolvers

open GraphQL.FSharp.Inference
open GraphQL.FSharp.Resolvers
open GraphQL.FSharp.Types

let inline private set f (x: TypedFieldType<_>) = f x; x

let (|PropertyNameGetter|_|) expr =
    match expr with
    | PropertyGet (Some (Var sourceVar), prop, []) -> Some (prop.Name, sourceVar)
    | _ -> None

let (|PropertyNameBasic|_|) expr =
    match expr with
    | Lambda (lambdaVar, PropertyNameGetter (propName, sourceVar)) when lambdaVar = sourceVar -> Some propName
    | _ -> None

let (|MethodInnerLambda|_|) expr =
    match expr with
    | Call (Some (Var sourceVar), method, []) -> Some (method.Name, sourceVar)
    | Application (expr, value) when value = <@@ () @@> ->
        match expr with
        | PropertyNameGetter (propName, sourceVar) -> Some (propName, sourceVar)
        | PropertyGet (Some (Var sourceVar), prop, []) -> Some (prop.Name, sourceVar)
        | _ -> None
    | _ -> None

let (|MethodNameBasic|_|) expr =
    match expr with
    | Lambda (lambdaVar, expr) ->
        match expr with
        | MethodInnerLambda (name, sourceVar) when sourceVar = lambdaVar -> Some name
        | _ -> None
    | _ -> None

let (|FieldName|_|) expr =
    match expr with
    | PropertyNameBasic name -> Some name
    | MethodNameBasic name -> Some name
    | _ -> None

let (|AsyncFieldName|_|) expr = (|MethodNameBasic|_|) expr

[<Literal>]
let FieldTypeMetadataName = "FieldType"

[<Literal>]
let HasDefaultValueMetadataName = "HasDefaultValue"

let setFieldType<'field, 'source> (field: TypedFieldType<'source>) =
    field.Metadata.[FieldTypeMetadataName] <- box typeof<'field>
    field

let getFieldType (field: TypedFieldType<_>) =
    match field.Metadata.TryGetValue FieldTypeMetadataName with
    | true, (:? Type as ``type``) when isNull field.Type && isNull field.ResolvedType -> Some ``type``
    | _ -> None

let hasDefaultValue (field: TypedFieldType<_>) =
    match field.Metadata.TryGetValue HasDefaultValueMetadataName with
    | true, value when unbox<bool> value -> true
    | _ -> false

type FieldBuilder<'source>(?ofType) =
    inherit BuilderMetadataBase<TypedFieldType<'source>>()

    [<CustomOperation "ofType">]
    member __.Type (field: TypedFieldType<'source>, ``type``) =
        set (fun x -> x.Type <- ``type``) field
    member __.Type (field: TypedFieldType<'source>, ``type``) =
        set (fun x -> x.ResolvedType <- ``type``) field

    [<CustomOperation "defaultValue">]
    member __.DefaultValue (field: TypedFieldType<'source>, ``default``: 'field) =
        field
        |> setFieldType<'field, _>
        |> set (fun x ->
            x.DefaultValue <- ``default``
            x.Metadata.[HasDefaultValueMetadataName] <- true
        )

    [<CustomOperation "arguments">]
    member __.Arguments (field: TypedFieldType<'source>, arguments: _ list) =
        set (fun x -> x.Arguments <- QueryArguments arguments) field

    [<CustomOperation "get">]
    member __.Get (field: TypedFieldType<'source>, [<ReflectedDefinition>] getter: Expr<'source -> 'field>) =
        let getterFunc =
            LeafExpressionConverter
                .QuotationToLambdaExpression(<@ Func<_, _> %getter @>)
                .Compile()
        let fieldName =
            match getter with
            | FieldName value -> value
            | _ -> invalidArg "getter" "Could not find field name from getter expression. Get only supports simple expressions. Use resolve instead."
        let resolver = Resolver.ResolveSource getterFunc.Invoke
        field
        |> setFieldType<'field, _>
        |> set (fun x ->
            x.Name <- fieldName
            x.Resolver <- resolver
        )

    [<CustomOperation "getAsync">]
    member __.GetAsync (field: TypedFieldType<'source>, [<ReflectedDefinition>] getter: Expr<'source -> Task<'field>>) =
        let getterFn =
            LeafExpressionConverter
                .QuotationToLambdaExpression(<@ Func<_, _> %getter @>)
                .Compile()
                .Invoke
        let fieldName =
            match getter with
            | AsyncFieldName value -> value
            | _ -> invalidArg "getter" "Could not find field name from async getter expression. GetAsync only supports simple expressions. Use resolve instead."
        field
        |> setFieldType<'field, _>
        |> set (fun x ->
            x.Name <- fieldName
            x.Resolver <- Resolver.ResolveAsyncSource getterFn
        )

    [<CustomOperation "resolve">]
    member inline __.Resolve (field: TypedFieldType<'source>, resolver: ResolveFieldContext<'source> -> 'field) =
        field
        |> setFieldType<'field, _>
        |> set (fun x -> x.Resolver <- Resolver.Resolve resolver)

    [<CustomOperation "resolveAsync">]
    member __.ResolveAsync (field: TypedFieldType<'source>, resolver: ResolveFieldContext<'source> -> Task<'field>) =
        field
        |> setFieldType<'field, _>
        |> set (fun x -> x.Resolver <- Resolver.ResolveAsync resolver)

    // TODO: Test this
    [<CustomOperation "subscribe">]
    member __.Subscribe (field: TypedFieldType<'source>, subscribe: ResolveEventStreamContext<'source> -> IObservable<'field>) =
        field
        |> setFieldType<'field, _>
        |> set (fun x -> x.Subscriber <- EventStreamResolver<_, _> (Func<_, _> subscribe))

    // TODO: Test this
    [<CustomOperation "subscribeAsync">]
    member __.SubscribeAsync (field: TypedFieldType<'source>, subscribe: ResolveEventStreamContext<'source> -> Task<IObservable<'field>>) =
        field
        |> setFieldType<'field, _>
        |> set (fun x -> x.AsyncSubscriber <- AsyncEventStreamResolver<_, _> (Func<_, _> subscribe))

    member __.Run (field: TypedFieldType<'source>) =
        let hasDefaultValue = hasDefaultValue field
        Option.iter (fun ofType ->
            field.ResolvedType <-
                if hasDefaultValue
                then ofType :> IGraphType
                else NonNullGraphType ofType :> IGraphType) ofType

        field
        |> getFieldType
        |> Option.iter (fun ``type`` ->
            field.ResolvedType <-
                not hasDefaultValue
                |> createReferenceConfigure ``type``)

        field

let field<'source when 'source: (new: unit -> 'source)> = FieldBuilder<'source> ()
// TODO: Add tests for fieldOf
let fieldOf ofType = FieldBuilder<obj> ofType
