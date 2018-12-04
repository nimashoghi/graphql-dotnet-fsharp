module GraphQL.FSharp.Util

open FSharp.Control.Reactive
open GraphQL.Builders
open GraphQL.Types
open GraphQL.Validation

open Iris
open Iris.Types

open Model

let metadataName field arg = sprintf "$$Field$$%s$$Arg$$%s$$" field arg
let metadataValueName field arg = sprintf "$$Field$$%s$$ArgValue$$%s$$" field arg

let setValidationValue (ctx: ValidationContext) name value =
    match ctx.UserContext with
    | :? UserContext as userCtx -> userCtx.Set name value
    | _ -> ()

let getValidationValue<'t> userCtx name =
    match box userCtx with
    | :? UserContext as userCtx -> userCtx.Get<'t> name
    | _ -> None

let metadata<'t> (metadataOperation: IObjectGraphType option) name =
    match metadataOperation with
    | Some op -> op.GetMetadata<'t option> (name, None)
    | None -> None

let resolveAsync
    (this: FieldBuilder<'source, 'retn>)
    (resolve: (ResolveFieldContext<'source> -> 'retn task)) =
    this.ResolveAsync (fun ctx -> resolve ctx |> Task.withError ctx.Errors.Add)

let resolveObservableList this resolve = resolve >> Observable.toTaskList |> resolveAsync this
let resolveObservable this resolve = resolve >> Observable.toTask |> resolveAsync this
