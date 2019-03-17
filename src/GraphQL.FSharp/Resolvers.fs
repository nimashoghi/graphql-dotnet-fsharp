module GraphQL.FSharp.Resolvers

open System.Collections
open System.Threading.Tasks
open FSharp.Utils
open FSharp.Utils.Reflection
open GraphQL

open GraphQL.FSharp.Types

let resolveHandler
    (handler: ResolveContext<'source> -> 'field -> obj)
    (f: ResolveContext<'source> -> 'field) =
    Resolver<'source, 'field> (fun ctx -> handler ctx (f ctx))

let resolveTaskHandler
    (handler: ResolveContext<'source> -> 'field -> obj)
    (f: ResolveContext<'source> -> 'field Task) =
    AsyncResolver<'source, 'field> (fun ctx -> Task.map (handler ctx) (f ctx))

let handleError (error: obj) =
    match Dictionary.ofObj error with
    // TODO: fix this later
    | Some dict -> ExecutionError (error.GetType().Name, dict :?> IDictionary)
    | None -> ExecutionError (string error)

let handleObject (ctx: ResolveContext<'source>) (x: 'field) =
    match box x with
    | null -> null
    | Option opt -> Option.toObj opt
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

let resolve f = resolveHandler handleObject f
let resolveAsync f = resolveTaskHandler handleObject f
