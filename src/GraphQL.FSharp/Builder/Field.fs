[<AutoOpen>]
module GraphQL.FSharp.Builder.Field

open System
open System.Threading.Tasks
open FSharp.Control.Tasks.V2
open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns
open FSharp.Linq.RuntimeHelpers
open GraphQL.Types
open GraphQL.Subscription
open GraphQL.Resolvers

open GraphQL.FSharp.Types
open GraphQL.FSharp.Inference

let inline private set f (x: TypedFieldType<_>) = f x; x

let (|FieldNameGetter|_|) expr =
    match expr with
    | PropertyGet (Some (Var sourceVar), prop, []) -> Some (prop.Name, sourceVar)
    | _ -> None

let (|FieldName|_|) expr =
    match expr with
    | Lambda (lambdaVar, FieldNameGetter (propName, sourceVar)) when lambdaVar = sourceVar -> Some propName
    | _ -> None

let (|AsyncMethodInnerLambda|_|) expr =
    match expr with
    | Call (Some (Var sourceVar), method, []) -> Some (method.Name, sourceVar)
    | Application (expr, value) when value = <@@ () @@> ->
        match expr with
        | FieldNameGetter (propName, sourceVar) -> Some (propName, sourceVar)
        | PropertyGet (Some (Var sourceVar), prop, []) -> Some (prop.Name, sourceVar)
        | _ -> None
    | _ -> None

let (|AsyncMethodNameBasic|_|) expr =
    match expr with
    | Lambda (lambdaVar, expr) ->
        match expr with
        | AsyncMethodInnerLambda (name, sourceVar) when sourceVar = lambdaVar -> Some name
        | _ -> None
    | _ -> None

let (|AsyncMethodNameComputationExpression|_|) expr =
    match expr with
    | Lambda (lambdaVar, Application (Lambda (_, expr), taskExpr)) when taskExpr = <@@ task @@> ->
        match expr with
        | SpecificCall <@ task.Run @> (_, _, [SpecificCall <@ task.Delay @> (_, _, [Lambda (_, inner)])]) ->
            match inner with
            | SpecificCall <@ task.Return @> (_, _, [AsyncMethodInnerLambda (name, sourceVar)])
                when sourceVar = lambdaVar -> Some name
            | SpecificCall <@ task.ReturnFrom @> (_, _, [AsyncMethodInnerLambda (name, sourceVar)])
                when sourceVar = lambdaVar -> Some name
            | Call (_, method, args) when method.Name.EndsWith "ReturnFrom" ->
                match args with
                | [_; AsyncMethodInnerLambda (name, sourceVar)] when sourceVar = lambdaVar -> Some name
                | _ -> None
            | _ -> None
        | _ -> None
    | _ -> None

let (|AsyncFieldName|_|) expr =
    printfn "%A" expr
    match expr with
    | AsyncMethodNameBasic name -> Some name
    | AsyncMethodNameComputationExpression name -> Some name
    | _ -> None

[<Literal>]
let FieldTypeMetadataName = "FieldType"

[<Literal>]
let HasDefaultValueMetadataName = "HasDefaultValue"

let setFieldType<'field, 'source> (field: TypedFieldType<'source>) =
    field.Metadata.[FieldTypeMetadataName] <- box typeof<'field>
    field

let getFieldType (field: TypedFieldType<_>) =
    match field.Metadata.TryGetValue FieldTypeMetadataName with
    | true, (:? Type as ``type``) when field.Type = null && field.ResolvedType = null -> Some ``type``
    | _ -> None

let hasDefaultValue (field: TypedFieldType<_>) =
    match field.Metadata.TryGetValue HasDefaultValueMetadataName with
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
        let resolver =
            {
                new IFieldResolver with
                    member __.Resolve ctx =
                        match ctx.Source with
                        | :? 'source as source ->
                            getterFunc.Invoke source
                            |> box
                        | _ -> invalidArg "ctx.Source" "Source object needs to be of type 'source."
            }
        field
        |> setFieldType<'field, _>
        |> set (fun x ->
            x.Name <- fieldName
            x.Resolver <- resolver
        )

    [<CustomOperation "getAsync">]
    member __.GetAsync (field: TypedFieldType<'source>, [<ReflectedDefinition>] getter: Expr<'source -> 'field Task>) =
        let getterFunc =
            LeafExpressionConverter
                .QuotationToLambdaExpression(<@ Func<_, _> %getter @>)
                .Compile()
        let fieldName =
            match getter with
            | AsyncFieldName value -> value
            | _ -> invalidArg "getter" "Could not find field name from async getter expression. GetAsync only supports simple expressions. Use resolve instead."
        let resolver =
            {
                new IFieldResolver with
                    member __.Resolve ctx =
                        match ctx.Source with
                        | :? 'source as source ->
                            getterFunc.Invoke source
                            |> box
                        | _ -> invalidArg "ctx.Source" "Source object needs to be of type 'source."
            }
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

    // TODO: Test this
    [<CustomOperation "subscribe">]
    member __.Subscribe (field: TypedFieldType<'source>, subscribe: ResolveEventStreamContext<'source> -> 'field IObservable) =
        field
        |> setFieldType<'field, _>
        |> set (fun x -> x.Subscriber <- EventStreamResolver<_, _> (Func<_, _> subscribe))

    // TODO: Test this
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
