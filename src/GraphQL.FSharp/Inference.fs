module GraphQL.FSharp.Inference

open System
open System.Collections.Generic
open GraphQL.Resolvers
open GraphQL.Types

open GraphQL.FSharp.Registry
open GraphQL.FSharp.Types

let (|Option|_|) (``type``: Type) =
    if ``type``.IsGenericType &&
        ``type``.GetGenericTypeDefinition () = typedefof<option<_>>
    then Some ``type``.GenericTypeArguments.[0]
    else None

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
let rec infer get (``type``: Type) =
    match ``type`` with
    | Nullable underlyingType
    | Option underlyingType ->
        infer get underlyingType
    | Enumerable underlyingType ->
        infer get underlyingType
        |> ListGraphType
        :> IGraphType
    | underlyingType ->
        get underlyingType
        |> Option.toObj

let inferObject ``type`` = infer Object.get ``type``
let inferInput ``type`` = infer InputObject.get ``type``

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
        field.ResolvedType <- inferObject retn
        field
    | _ -> field

// TODO: Add proper validation using the result type
let (|Result|_|) (``type``: Type) =
    if ``type``.IsGenericType &&
        ``type``.GetGenericTypeDefinition () = typedefof<Result<_, _>>
    then Some (``type``.GenericTypeArguments.[0], ``type``.GenericTypeArguments.[1])
    else None