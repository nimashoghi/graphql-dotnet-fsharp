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

type FieldBuilder<'source>(?ofType) =
    inherit BuilderMetadataBase<TypedFieldType<'source>>()

    [<CustomOperation "ofType">]
    member __.Type (field: TypedFieldType<'source>, ``type``) =
        set (fun x -> x.Type <- ``type``) field
    member __.Type (field: TypedFieldType<'source>, ``type``) =
        set (fun x -> x.ResolvedType <- ``type``) field

    [<CustomOperation "defaultValue">]
    member __.DefaultValue (field: TypedFieldType<'source>, ``default``: 'field) =
        set (fun x -> x.DefaultValue <- ``default``) field

    [<CustomOperation "args">]
    member __.Args (field: TypedFieldType<'source>, arguments: _ list) =
        set (fun x -> x.Arguments <- QueryArguments arguments) field

    [<CustomOperation "get">]
    // Should i not set name here?
    member __.Get (field: TypedFieldType<'source>, [<ReflectedDefinition>] getter: Expr<'source -> 'field>) =
        let resolver =
            LeafExpressionConverter.QuotationToLambdaExpression <@ Func<_, _> %getter @>
            |> ExpressionFieldResolver
        set (fun x ->
            x.Name <- Option.get (getFieldName getter)
            x.Resolver <- resolver) field

    [<CustomOperation "resolve">]
    member __.Resolve (field: TypedFieldType<'source>, resolver: ResolveFieldContext<'source> -> 'field) =
        let resolver = FuncFieldResolver<_, _> (Func<_, _> resolver)
        set (fun x -> x.Resolver <- resolver) field

    member __.Resolve (field: TypedFieldType<'source>, resolver: ResolveFieldContext<'source> -> 'field Task) =
        let resolver = AsyncFieldResolver<_, _> (Func<_, _> resolver)
        set (fun x -> x.Resolver <- resolver) field

    [<CustomOperation "subscribe">]
    member __.Subscribe (field: TypedFieldType<'source>, subscribe: ResolveEventStreamContext<'source> -> 'field IObservable) =
        let subscriber = EventStreamResolver<_, _> (Func<_, _> subscribe)
        set (fun x -> x.Subscriber <- subscriber) field

    // TODO: Should there be subscribe and subscribeAsync?
    // [<CustomOperation "subscribeAsync">]
    member __.Subscribe (field: TypedFieldType<'source>, subscribe: ResolveEventStreamContext<'source> -> 'field IObservable Task) =
        let subscriber = AsyncEventStreamResolver<_, _> (Func<_, _> subscribe)
        set (fun x -> x.AsyncSubscriber <- subscriber) field

    member __.Run (field: TypedFieldType<'source>) =
        Option.iter (fun ofType -> field.ResolvedType <- ofType) ofType

        if shouldInferField field
        then inferField field
        else field

let field<'source> = FieldBuilder<'source> ()
let fieldOf ofType = FieldBuilder<obj> ofType
