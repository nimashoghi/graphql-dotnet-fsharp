module GraphQL.FSharp.Utils

open System
open System.Collections.Generic
open System.Threading.Tasks
open System.Text.RegularExpressions

let (|Regex|_|) pattern input =
    match Regex.Match (input, pattern) with
    | m when m.Success ->
        m.Groups
        |> Seq.cast<Group>
        |> Seq.toList
        |> List.map (fun group -> group.Value)
        |> List.tail
        |> Some
    | _ -> None

module Type =
    let (|Option|_|) (``type``: Type) =
        if ``type``.IsGenericType &&
            ``type``.GetGenericTypeDefinition () = typedefof<_ option>
        then Some ``type``.GenericTypeArguments.[0]
        else None

    // TODO: Add proper validation using the result type
    let (|Result|_|) (``type``: Type) =
        if ``type``.IsGenericType &&
            ``type``.GetGenericTypeDefinition () = typedefof<Result<_, _>>
        then Some ``type``.GenericTypeArguments.[0]
        else None

    let (|Nullable|_|) (``type``: Type) =
        if ``type``.IsGenericType && ``type``.GetGenericTypeDefinition() = typedefof<Nullable<_>>
        then Some``type``.GenericTypeArguments.[0]
        else None

    // TODO: Add proper inference of IObservable types
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

    let (|String|_|) ``type`` = if ``type`` = typeof<string> then Some () else None

    let (|Enumerable|_|) (``type``: Type) =
        match ``type`` with
        | String -> None
        | ArrayType underlyingType
        | EnumerableType underlyingType -> Some underlyingType
        | _ -> None


module Option =
    let ofBox (x: 't) =
        x
        |> box
        |> Option.ofObj
        |> Option.map unbox<'t>

    let ``or`` ``else`` x =
        match x with
        | Some value -> value
        | None -> ``else``

    let toBox x =
        x
        |> Option.map box
        |> ``or`` (box null)

module Array =
    let some array =
        array
        |> Array.filter Option.isSome
        |> Array.map Option.get

module List =
    let some list =
        list
        |> List.filter Option.isSome
        |> List.map Option.get

module Seq =
    let some seq =
        seq
        |> Seq.filter Option.isSome
        |> Seq.map Option.get
