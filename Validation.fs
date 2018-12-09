module GraphQL.FSharp.Validation

open System.Collections.Concurrent
open FSharp.Validation
open GraphQL.Types
open GraphQL.Validation
open GraphQL.Language.AST
open Iris.Option

open Util

[<AutoOpen>]
module Model =
    type UserContext() =
        let properties = ConcurrentDictionary<string, obj>()

        member __.Get<'t> name =
            match properties.TryGetValue name with
            | TryGet.Some value when (value :? 't) -> Some (value :?> 't)
            | _ -> None

        member __.Set name value = properties.[name] <- box value

[<AutoOpen>]
module Util =
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

let reportError (ctx: ValidationContext) (arg: Argument) (error: IErrorCode) =
    ctx.ReportError(ValidationError(ctx.OriginalQuery, string error, string error, arg.NamedNode)) // TODO

let handleErrors ctx (errors: (ErrorCode * Argument) list) =
    for (err, arg) in errors do reportError ctx arg err

type GenericValidationType() =
    interface IValidationRule with
        member __.Validate ctx =
            enterLeaveListener (fun listener ->
                let mutable metadataOperation: IObjectGraphType option = None
                listener.Match<Operation>(enter = fun operation ->
                    match operation.OperationType with
                    | OperationType.Query -> metadataOperation <- Some ctx.Schema.Query
                    | OperationType.Mutation -> metadataOperation <- Some ctx.Schema.Mutation
                    | _ -> ())
                let mutable currentField: Field option = None
                listener.Match<Field>(enter = fun field -> currentField <- Some field)
                listener.Match<Argument>(enter = fun arg ->
                    ignore <| maybe {
                        let! field = currentField
                        let! validator =
                            metadataName field.Name arg.Name
                            |> metadata<Validator<obj>> metadataOperation
                        match validator arg.Value.Value with
                        | Ok value -> (ctx.UserContext :?> UserContext).Set (metadataValueName field.Name arg.Name) value
                        | Error errors -> List.iter (reportError ctx arg) errors
                    }))
