module GraphQL.FSharp.Resolvers

open System
open System.Collections
open System.Threading.Tasks
open FSharp.Utils
open FSharp.Utils.Tasks
open GraphQL

open GraphQL.FSharp.Types

[<Struct>]
type ResolvedValue<'value, 'error> =
| ValueValue of Value: 'value
| OptionValue of Option: 'value option
| ValueOptionValue of ValueOption: 'value voption
| ResultValue of Result: Result<'value, 'error list>

let handleError (error: obj) =
    match Dictionary.ofObj error with
    // TODO: fix this later
    | Some dict -> ExecutionError (error.GetType().Name, dict :?> IDictionary)
    | None -> ExecutionError (string error)

module Option =
    let toValueOption x =
        match x with
        | Some x -> ValueSome x
        | None -> ValueNone

let internal handleCtxOption addErrors value =
    match value with
    | ValueValue x -> ValueSome x
    | OptionValue x -> Option.toValueOption x
    | ValueOptionValue x -> x
    | ResultValue x ->
        match x with
        | Ok x -> ValueSome x
        | Error errors ->
            errors
            |> List.map handleError
            |> addErrors

            ValueNone

let internal handleCtx addErrors value =
    handleCtxOption addErrors value
    |> ValueOption.defaultValue Unchecked.defaultof<_>

let resolve (f: ResolveContext<'source> -> ResolvedValue<'field, 'error>) =
    Resolver<'source, 'field> (fun ctx -> f ctx |> handleCtx ctx.Errors.AddRange)

let resolveAsync (f: ResolveContext<'source> -> ResolvedValue<'field, 'error> ValueTask) =
    AsyncResolver<'source, 'field> (
        fun ctx -> vtask {
            let! value = f ctx
            return handleCtx ctx.Errors.AddRange value
        }
    )

let resolveSubscription (f: ResolveContext<'source> -> ResolvedValue<'field IObservable, 'error> ValueTask) =
    AsyncStreamResolver<'source, 'field> (
        fun ctx -> vtask {
            let! value = f ctx
            return handleCtx ctx.Errors.AddRange value
        }
    )
