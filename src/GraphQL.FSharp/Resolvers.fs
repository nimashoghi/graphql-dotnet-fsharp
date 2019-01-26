module GraphQL.FSharp.Resolvers

open System.Threading.Tasks
open FSharp.Control.Tasks.V2
open GraphQL
open GraphQL.Resolvers
open GraphQL.Types

open GraphQL.FSharp.Utils
open GraphQL.FSharp.Utils.Type

let private getSource (ctx: ResolveFieldContext<_>) = ctx.Source

[<AutoOpen>]
module Handlers =
    let internal addError (ctx: ResolveFieldContext<_>) err =
        sprintf "%A" err
        |> ExecutionError
        |> ctx.Errors.Add

    let handleOption _ x = Option.toBox x
    let handleResult ctx x =
        match x with
        | Ok value -> box value
        | Error error ->
            addError ctx error
            null
    let handleObject _ x = box x

let private taskMap f (t: _ Task) =
    task {
        let! result = t
        return f result
    }

let inline private resolve handler f =
    {
        new IFieldResolver with
            member __.Resolve ctx =
                ResolveFieldContext<_> ctx
                |> f
                |> handler ctx
    }

let inline private resolveTask handler (f: _ -> _ Task) =
    {
        new IFieldResolver with
            member __.Resolve ctx =
                ResolveFieldContext<_> ctx
                |> f
                |> taskMap (handler ctx)
                |> box
    }

type Resolver =
    static member ResolveOption f = resolve handleOption f
    static member ResolveResult f = resolve handleResult f
    static member Resolve f = resolve handleObject f

    static member ResolveSourceOption f = resolve handleOption (getSource >> f)
    static member ResolveSourceResult f = resolve handleResult (getSource >> f)
    static member ResolveSource f = resolve handleObject (getSource >> f)

    static member ResolveAsyncOption f = resolveTask handleOption f
    static member ResolveAsyncResult f = resolveTask handleResult f
    static member ResolveAsync f = resolveTask handleObject f

    static member ResolveAsyncSourceOption f = resolveTask handleOption (getSource >> f)
    static member ResolveAsyncSourceResult f = resolveTask handleResult (getSource >> f)
    static member ResolveAsyncSource f = resolveTask handleObject (getSource >> f)
