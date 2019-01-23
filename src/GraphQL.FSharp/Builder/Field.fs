[<AutoOpen>]
module GraphQL.FSharp.Builder.Field

open System
open System.Collections.Generic
open System.Threading.Tasks
open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Linq.RuntimeHelpers
open GraphQL.Types
open GraphQL.Subscription
open GraphQL.Resolvers

open GraphQL.FSharp.Types
open GraphQL.FSharp.Inference

let inline private set f (x: TypedFieldType<_>) = f x; x

let getFieldName expr =
    match expr with
    | Lambda (lambdaVar, expr) ->
        match expr with
        | PropertyGet (Some (Var soruceVar), prop, []) when soruceVar = lambdaVar -> Some prop.Name
        | _ -> None
    | _ -> None

let setFieldType<'field, 'source> (field: TypedFieldType<'source>) =
    field.Metadata.["FieldType"] <- box typeof<'field>
    field

let getFieldType (field: TypedFieldType<_>) =
    match field.Metadata.TryGetValue "FieldType" with
    | true, (:? Type as ``type``) when field.Type = null && field.ResolvedType = null -> Some ``type``
    | _ -> None

let hasDefaultValue (field: TypedFieldType<_>) =
    match field.Metadata.TryGetValue "HasDefaultValue" with
    | true, value when unbox<bool> value = true -> true
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
            x.Metadata.["HasDefaultValue"] <- true
        )

    [<CustomOperation "args">]
    member __.Args (field: TypedFieldType<'source>, arguments: _ list) =
        set (fun x -> x.Arguments <- QueryArguments arguments) field

    [<CustomOperation "get">]
    member __.Get (field: TypedFieldType<'source>, [<ReflectedDefinition>] getter: Expr<'source -> 'field>) =
        let resolver =
            LeafExpressionConverter.QuotationToLambdaExpression <@ Func<_, _> %getter @>
            |> ExpressionFieldResolver
        let fieldName =
            match getFieldName getter with
            | Some value -> value
            | None -> invalidArg "getter" "Could not find field name from getter expression. Get only supports simple expressions. Use resolve instead."
        field
        |> setFieldType<'field, _>
        |> set (fun x ->
            x.Name <- fieldName
            x.Resolver <- resolver
        )

    [<CustomOperation "resolve">]
    member __.Resolve (field: TypedFieldType<'source>, resolver: ResolveFieldContext<'source> -> 'field) =
        field
        |> setFieldType<'field, _>
        |> set (fun x -> x.Resolver <- FuncFieldResolver<_, _> (Func<_, _> resolver))

    [<CustomOperation "resolveAsync">]
    member __.ResolveAsync (field: TypedFieldType<'source>, resolver: ResolveFieldContext<'source> -> 'field Task) =
        field
        |> setFieldType<'field, _>
        |> set (fun x -> x.Resolver <- AsyncFieldResolver<_, _> (Func<_, _> resolver))

    [<CustomOperation "subscribe">]
    member __.Subscribe (field: TypedFieldType<'source>, subscribe: ResolveEventStreamContext<'source> -> 'field IObservable) =
        field
        |> setFieldType<'field, _>
        |> set (fun x -> x.Subscriber <- EventStreamResolver<_, _> (Func<_, _> subscribe))

    [<CustomOperation "subscribeAsync">]
    member __.SubscribeAsync (field: TypedFieldType<'source>, subscribe: ResolveEventStreamContext<'source> -> 'field IObservable Task) =
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
                |> inferObjectConfigure ``type``)

        field

let field<'source when 'source: (new: unit -> 'source)> = FieldBuilder<'source> ()
// TODO: Add tests for fieldOf
let fieldOf ofType = FieldBuilder<obj> ofType
