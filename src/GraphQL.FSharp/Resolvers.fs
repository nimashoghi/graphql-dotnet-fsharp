module GraphQL.FSharp.Resolvers

open System.Collections
open System.Collections.Generic
open System.Threading.Tasks
open FSharp.Reflection
open GraphQL
open GraphQL.Resolvers
open GraphQL.Types
open Newtonsoft.Json

open GraphQL.FSharp.Utils

let private getSource (ctx: ResolveFieldContext<_>) = ctx.Source

[<AutoOpen>]
module Handlers =
    let (|CaseTag|_|) i (case: UnionCaseInfo) = if case.Tag = i then Some () else None

    let unionValue f (x: obj) =
        let ``type`` = x.GetType ()
        if FSharpType.IsUnion ``type`` then
            FSharpValue.GetUnionFields (x, ``type``)
            ||> f
        else invalidArg "x" "x must be a union type"

    let optionValue x =
        unionValue
            (fun case [|value|] ->
                match case with
                | CaseTag 0 -> Some value
                | CaseTag 1 -> None
                | _ -> invalidArg "x" "Could not find proper case"
            )
            x

    let resultValue x =
        unionValue
            (fun case [|value|] ->
                match case with
                | CaseTag 0 -> Ok value
                | CaseTag 1 -> Error value
                | _ -> invalidArg "x" "Could not find proper case"
            )
            x

    let (|Option|_|) (x: obj) =
        let ``type`` = x.GetType ()
        if ``type``.IsGenericType
            && ``type``.GetGenericTypeDefinition () = typedefof<_ option>
        then Some (``type``.GenericTypeArguments.[0], optionValue x)
        else None

    let (|Result|_|) (x: obj) =
        let ``type`` = x.GetType ()
        if ``type``.IsGenericType
            && ``type``.GetGenericTypeDefinition () = typedefof<Result<_, _>>
        then Some (``type``.GenericTypeArguments.[0], ``type``.GenericTypeArguments.[1], resultValue x)
        else None

let private taskMap f (t: _ Task) =
    let source = TaskCompletionSource ()
    t.ContinueWith (fun (t: _ Task) ->
        if t.IsCanceled then source.SetCanceled ()
        elif t.IsFaulted then source.SetException t.Exception
        else source.SetResult (f t.Result)
    ) |> ignore
    source.Task

let inline private resolveHandler handler f =
    {
        new IFieldResolver with
            member __.Resolve ctx =
                ResolveFieldContext<_> ctx
                |> f
                |> handler ctx
    }

let inline private resolveTaskHandler handler (f: _ -> _ Task) =
    {
        new IFieldResolver with
            member __.Resolve ctx =
                ResolveFieldContext<_> ctx
                |> f
                |> taskMap (handler ctx)
                |> box
    }

// FIXME: This is a hacky way to do this right now.
let getDict x =
    try
        JsonConvert.SerializeObject x
        |> JsonConvert.DeserializeObject<Dictionary<string, obj>>
        |> Some
    with _ -> None

// let getDict x = dict [
//     for prop in x.GetType().GetProperties() do
//         yield prop.Name, prop.GetValue x
// ]

// TODO: Add tests for this
let handleObject (ctx: ResolveFieldContext) x =
    match box x with
    | Option (_, opt) -> Option.toObj opt
    | Result (_, _, result) ->
        match result with
        | Ok value -> value
        | Error error ->
            let executionError =
                match getDict error with
                | Some dict -> ExecutionError (error.GetType().Name, dict :> IDictionary)
                | None ->
                    string error
                    |> ExecutionError
            executionError
            |> ctx.Errors.Add
            null
    | x -> x

let withSource f =
    fun (x: ResolveFieldContext<_>) -> f x.Source

let resolve f = resolveHandler handleObject f
let resolveAsync f = resolveTaskHandler handleObject f

let getTask (obj: obj) =
    let source = TaskCompletionSource<obj> ()
    (obj :?> Task)
        .ContinueWith(fun t ->
            if t.IsCanceled then source.SetCanceled ()
            elif t.IsFaulted then source.SetException t.Exception
            else source.SetResult (t.GetType().GetProperty("Result").GetValue t)
        )
        |> ignore
    source.Task

let resolveInfer f =
    {
        new IFieldResolver with
            member __.Resolve ctx =
                let result =
                    ResolveFieldContext<_> ctx
                    |> f
                    |> box

                match result.GetType () with
                | Task _ ->
                    getTask result
                    |> taskMap (handleObject ctx)
                    |> box
                | _ ->
                    result
                    |> handleObject ctx
                    |> box
    }
