[<AutoOpen>]
module GraphQL.FSharp.Builder.Field

open System
open FSharp.Quotations
open FSharp.Linq.RuntimeHelpers
open GraphQL.Types
open GraphQL.Subscription
open GraphQL.Resolvers

let inline private set f (x: EventStreamFieldType) = f x; x

[<AutoOpen>]
module private TypeInference =
    open System.Collections.Generic
    open GraphQL.Utilities

    let (|Nullable|_|) (``type``: Type) =
        if ``type``.IsGenericType && ``type``.GetGenericTypeDefinition() = typedefof<Nullable<_>>
        then Some``type``.GenericTypeArguments.[0]
        else None

    let (|ArrayType|_|) (``type``: Type) =
        if ``type``.IsArray
        then Some (``type``.GetElementType ())
        else None

    let (|EnumerableType|_|) (``type``: Type) =
        ``type``
            .GetInterfaces()
            |> Array.tryFind (fun ``interface`` ->
                ``interface``.IsGenericType &&
                ``interface``.GetGenericTypeDefinition () = typedefof<IEnumerable<_>>)
            |> Option.map (fun ``interface`` -> ``interface``.GenericTypeArguments.[0])

    let (|Enumerable|_|) (``type``: Type) =
        match ``type`` with
        | ArrayType underlyingType
        | EnumerableType underlyingType -> Some underlyingType
        | _ -> None

    // TODO: Check nullable stuff
    let rec infer (``type``: Type) =
        match ``type`` with
        | Nullable underlyingType -> infer underlyingType
        | Enumerable underlyingType -> ListGraphType (infer underlyingType) :> IGraphType
        // TODO: clean up
        | underlyingType -> GraphTypeInstanceRegistry().Get underlyingType

    let getReturnType (resolver: IFieldResolver) =
        resolver
            .GetType()
            .GetInterfaces()
            |> Array.tryFind (fun ``interface`` ->
                ``interface``.IsGenericType &&
                ``interface``.GetGenericTypeDefinition() = typedefof<IFieldResolver<_>>)
            |> Option.map (fun ``interface`` -> ``interface``.GenericTypeArguments.[0])

    let (|TypedResolver|_|) resolver =
        if resolver = null
        then None
        else getReturnType resolver

    let shouldInferField (field: FieldType) = field.Type = null && field.ResolvedType = null

    let inferField (field: FieldType) =
        match field.Resolver with
        | TypedResolver retn ->
            field.ResolvedType <- infer retn
            field
        | _ -> field

type FieldBuilder<'source>() =
    inherit BuilderMetadataBase<EventStreamFieldType>()

    [<CustomOperation "type">]
    member __.Type (field, ``type``) =
        set (fun x -> x.Type <- ``type``) field
    member __.Type (field, ``type``) =
        set (fun x -> x.ResolvedType <- ``type``) field

    [<CustomOperation "defaultValue">]
    member __.DefaultValue (field, ``default``) =
        set (fun x -> x.DefaultValue <- ``default``) field

    [<CustomOperation "arguments">]
    member __.Arguments (field, arguments: _ list) =
        set (fun x -> x.Arguments <- QueryArguments arguments) field

    [<CustomOperation "get">]
    member __.Get (field, [<ReflectedDefinition>] getter: Expr<'source -> 'property>) =
        let resolver =
            LeafExpressionConverter.QuotationToLambdaExpression <@ Func<_, _> %getter @>
            |> ExpressionFieldResolver
        set (fun x -> x.Resolver <- resolver) field

    [<CustomOperation "resolve">]
    member __.Resolve (field, resolver: ResolveFieldContext<'source> -> _) =
        let resolver = FuncFieldResolver<_, _> (Func<_, _> resolver)
        set (fun x -> x.Resolver <- resolver) field

    member __.Resolve (field, resolver: ResolveFieldContext<'source> -> _) =
        let resolver = AsyncFieldResolver<_, _> (Func<_, _> resolver)
        set (fun x -> x.Resolver <- resolver) field

    [<CustomOperation "subscribe">]
    member __.Subscribe (field, subscribe: ResolveEventStreamContext<'source> -> _) =
        let subscriber = EventStreamResolver<_, _> (Func<_, _> subscribe)
        set (fun x -> x.Subscriber <- subscriber) field

    // TODO: Should there be subscribe and subscribeAsync?
    // [<CustomOperation "subscribeAsync">]
    member __.Subscribe (field, subscribeAsync: ResolveEventStreamContext<'source> -> _) =
        let asyncSubscriber = AsyncEventStreamResolver<_, _> (Func<_, _> subscribeAsync)
        set (fun x -> x.AsyncSubscriber <- asyncSubscriber) field

    member __.Run (field: FieldType) =
        if shouldInferField field
        then inferField field
        else field

let field<'source> = FieldBuilder<'source> ()
