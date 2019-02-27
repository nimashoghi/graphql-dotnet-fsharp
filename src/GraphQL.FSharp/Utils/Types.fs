[<AutoOpen>]
module GraphQL.FSharp.Utils.Types

open System
open System.Collections.Generic
open System.Threading.Tasks

let (|Option|_|) (``type``: Type) =
    if ``type``.IsGenericType &&
        ``type``.GetGenericTypeDefinition () = typedefof<_ option>
    then Some ``type``.GenericTypeArguments.[0]
    else None

let (|Result|_|) (``type``: Type) =
    if ``type``.IsGenericType &&
        ``type``.GetGenericTypeDefinition () = typedefof<Result<_, _>>
    then Some ``type``.GenericTypeArguments.[0]
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
    if ``type``.IsGenericType && ``type``.GetGenericTypeDefinition () = typedefof<Task<_>>
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

let (|String|_|) ``type`` =
    if ``type`` = typeof<string>
    then Some ()
    else None

let (|Enumerable|_|) (``type``: Type) =
    match ``type`` with
    | String -> None
    | ArrayType underlyingType
    | EnumerableType underlyingType -> Some underlyingType
    | _ -> None
