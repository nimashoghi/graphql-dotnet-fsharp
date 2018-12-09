module GraphQL.FSharp.Util

open System
open FSharp.Control.Reactive
open GraphQL.Builders
open GraphQL.Types
open GraphQL.Validation

open Iris
open Iris.Types

let enterLeaveListener (f: EnterLeaveListener -> unit) =
    EnterLeaveListener(Action<EnterLeaveListener> f) :> INodeVisitor

let resolveAsync
    (this: FieldBuilder<'source, 'retn>)
    (resolve: (ResolveFieldContext<'source> -> 'retn task)) =
    this.ResolveAsync (fun ctx -> resolve ctx |> Task.withError ctx.Errors.Add)

let resolveObservableList this resolve = resolve >> Observable.toTaskList |> resolveAsync this
let resolveObservable this resolve = resolve >> Observable.toTask |> resolveAsync this
