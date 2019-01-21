module GraphQL.FSharp.Inference

open System
open System.Collections.Generic
open System.Threading.Tasks
open GraphQL.Types

open GraphQL.FSharp.Registry

let (|Option|_|) (``type``: Type) =
    if ``type``.IsGenericType &&
        ``type``.GetGenericTypeDefinition () = typedefof<option<_>>
    then Some ``type``.GenericTypeArguments.[0]
    else None

// TODO: Add proper validation using the result type
let (|Result|_|) (``type``: Type) =
    if ``type``.IsGenericType &&
        ``type``.GetGenericTypeDefinition () = typedefof<Result<_, _>>
    then Some (``type``.GenericTypeArguments.[0], ``type``.GenericTypeArguments.[1])
    else None

let (|Nullable|_|) (``type``: Type) =
    if ``type``.IsGenericType && ``type``.GetGenericTypeDefinition() = typedefof<Nullable<_>>
    then Some``type``.GenericTypeArguments.[0]
    else None

let (|Observable|_|) (``type``: Type) =
    ``type``.GetInterfaces ()
    |> Array.tryFind (fun ``interface`` ->
        ``interface``.IsGenericType
        && ``interface``.GetGenericTypeDefinition () = typedefof<IObservable<_>>)

let (|Task|_|) (``type``: Type) =
    if ``type``.IsGenericType && typeof<Task>.IsAssignableFrom ``type``
    then Some ``type``.GenericTypeArguments.[0]
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
let rec infer checkNullability get (``type``: Type) =
    let graphType, isNull =
        match ``type`` with
        | Observable underlyingType -> infer checkNullability get underlyingType, false
        | Task underlyingType -> infer checkNullability get underlyingType, false
        | Nullable underlyingType
        | Option underlyingType ->
            infer false get underlyingType, true
        | Enumerable underlyingType ->
            (infer checkNullability get underlyingType
            |> ListGraphType
            :> IGraphType), false
        | underlyingType ->
            (get underlyingType
            |> Option.toObj), false

    if checkNullability && not isNull
    then NonNullGraphType graphType :> IGraphType
    else graphType


let inferObject ``type`` = infer false Object.get ``type``
let inferInput ``type`` = infer false InputObject.get ``type``

let inferObjectNull ``type`` = infer true Object.get ``type``
let inferInputNull ``type`` = infer true InputObject.get ``type``
