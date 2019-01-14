[<AutoOpen>]
module GraphQL.FSharp.Util.Inference

open System
open System.Collections.Generic
open GraphQL.Resolvers
open GraphQL.Types

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

let (|String|_|) ``type`` = if ``type`` = typeof<string> then Some () else None

let (|Enumerable|_|) (``type``: Type) =
    match ``type`` with
    | String -> None
    | ArrayType underlyingType
    | EnumerableType underlyingType -> Some underlyingType
    | _ -> None

// TODO: Check nullable stuff
let rec infer (``type``: Type) =
    match ``type`` with
    | Nullable underlyingType -> infer underlyingType
    | Enumerable underlyingType -> ListGraphType (infer underlyingType) :> IGraphType
    // TODO: clean up
    | underlyingType -> Option.toObj (TypeRegistry.get underlyingType)

let getReturnType (resolver: IFieldResolver) =
    resolver
        .GetType()
        .GetInterfaces()
        |> Array.tryFind (fun ``interface`` ->
            ``interface``.IsGenericType &&
            ``interface``.GetGenericTypeDefinition() = typedefof<IFieldResolver<_>>)
        // TODO: What about IFieldResolver of Task<TReturnType>
        |> Option.map (fun ``interface`` -> ``interface``.GenericTypeArguments.[0])

let (|TypedResolver|_|) resolver =
    if resolver = null
    then None
    else getReturnType resolver

let shouldInferField (field: TypedFieldType<'source>) = field.Type = null && field.ResolvedType = null

let inferField (field: TypedFieldType<'source>) =
    match field.Resolver with
    | TypedResolver retn ->
        field.ResolvedType <- infer retn
        field
    | _ -> field
