module GraphQL.FSharp.Resolvers

open System
open System.Collections
open System.Reactive.Linq
open System.Threading.Tasks
open FSharp.Utils
open FSharp.Utils.Tasks
open FSharp.Utils.Reflection
open GraphQL

open GraphQL.FSharp.Types

let handleError (error: obj) =
    match Dictionary.ofObj error with
    // TODO: fix this later
    | Some dict -> ExecutionError (error.GetType().Name, dict :?> IDictionary)
    | None -> ExecutionError (string error)

let handleObject (ctx: ResolveContext<'source>) (x: 'field) =
    match box x with
    | null -> null
    | Option opt -> Option.toObj opt
    | ValueOption opt -> ValueOption.toObj opt
    | ValidationResult result ->
        match result with
        | Ok value -> value
        | Error errors ->
            errors
            |> List.map handleError
            |> ctx.Errors.AddRange
            null
    | Result result ->
        match result with
        | Ok value -> value
        | Error errors ->
            errors
            |> handleError
            |> ctx.Errors.Add
            null
    | value -> value

let resolveTaskHandler
    (handler: ResolveContext<'source> -> 'field -> obj)
    (f: ResolveContext<'source> -> 'field Task) =
    AsyncResolver<'source, 'field> (
        fun ctx -> task {
            let! x = f ctx
            return handler ctx x
        }
    )

let resolveSubscriberHandler
    (handler: ResolveContext<'source> -> 'field -> obj)
    (f: ResolveContext<'source> -> 'field IObservable Task) =
    AsyncStreamResolver<'source, 'field> (
        fun ctx -> task {
            let! obs = f ctx
            return obs.Select (handler ctx)
        }
    )

let resolveAsync f = resolveTaskHandler handleObject f
let resolveSubscriberAsync f = resolveSubscriberHandler handleObject f
