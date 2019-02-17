[<AutoOpen>]
module GraphQL.FSharp.Utils.Types

open System
open System.Collections.Generic
open System.Reflection
open System.Runtime.CompilerServices
open System.Threading.Tasks
open FSharp.Reflection

[<Literal>]
let AnonymousTypeName = "AnonymousType"

let isSimpleField (prop: PropertyInfo) =
    not <| FSharpType.IsFunction prop.PropertyType

let isSimpleAnonymousType ``type`` =
    FSharpType.GetRecordFields ``type``
    |> Array.forall isSimpleField

let (|AnonymousType|_|) (``type``: Type) =
    let hasCompilerGeneratedAttribute =
        ``type``
            .GetCustomAttributes(typeof<CompilerGeneratedAttribute>, false)
        |> Seq.isEmpty
        |> not
    let nameContainsAnonymousType = ``type``.FullName.Contains AnonymousTypeName
    if hasCompilerGeneratedAttribute && nameContainsAnonymousType
    then
        if not <| isSimpleAnonymousType ``type``
        then failwith "Anonymous types have to be simple!"
        else Some ()
    else None

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
