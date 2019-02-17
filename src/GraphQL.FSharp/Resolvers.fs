module GraphQL.FSharp.Resolvers

open System.Collections
open System.Collections.Generic
open System.Threading.Tasks
open FSharp.Reflection
open GraphQL
open GraphQL.Resolvers
open GraphQL.Types

open GraphQL.FSharp.Utils

let private getSource (ctx: ResolveFieldContext<_>) = ctx.Source

[<AutoOpen>]
module Handlers =
    let (|CaseTag|_|) i (case: UnionCaseInfo) = if case.Tag = i then Some () else None

    let unionValue f (x: obj) =
        let ``type`` = x.GetType ()
        if FSharpType.IsUnion ``type``
        then f <|| FSharpValue.GetUnionFields (x, ``type``)
        else invalidArg "x" "x must be a union type"

    let optionValue x =
        match box x with
        | null -> None
        | x ->
            unionValue
                (fun case args ->
                    let value = match args with [|value|] -> value | _ -> failwith "Failed to get value out of union case!"
                    match case with
                    | CaseTag 0 -> None
                    | CaseTag 1 -> Some value
                    | _ -> invalidArg "x" "Could not find proper case"
                )
                x

    let resultValue x =
        unionValue
            (fun case args ->
                let value = match args with [|value|] -> value | _ -> failwith "Failed to get value out of union case!"
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

let internal taskMap f (t: _ Task) =
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

let handleObject (ctx: #ResolveFieldContext<_>) x =
    match box x with
    | null -> null
    | Option (_, opt) ->
        Option.toObj opt
    | Result (_, _, result) ->
        match result with
        | Ok value -> value
        | Error error ->
            ctx.Errors.Add <|
                match Dictionary.ofObj error with
                | Some dict ->
                    ExecutionError (
                        message = error.GetType().Name,
                        data = (dict :> IDictionary)
                    )
                | None ->
                    ExecutionError (
                        message = string error
                    )
            null
    | value -> value

let resolve f = resolveHandler handleObject f
let resolveAsync f = resolveTaskHandler handleObject f
