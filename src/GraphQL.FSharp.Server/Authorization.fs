[<AutoOpen>]
module GraphQL.FSharp.Server.Authorization

open System
open System.Threading.Tasks
open FSharp.Reflection
open FSharp.Utils
open FSharp.Utils.Tasks
open GraphQL.FSharp
open GraphQL.FSharp.Builder.Operations
open GraphQL.Server
open GraphQL.Server.Authorization.AspNetCore
open Microsoft.AspNetCore.Authorization
open Microsoft.Extensions.DependencyInjection

let mkName name = sprintf "GraphQL.FSharp.Server.Auth.%s" name

type IPolicy =
    abstract member Authorize: AuthorizationPolicyBuilder -> AuthorizationPolicyBuilder

type Requirement (invoke: IServiceProvider -> AuthorizationHandlerContext -> bool Task) =
    interface IAuthorizationRequirement

    member __.Invoke services ctx = invoke services ctx

type RequirementHandler (services) =
    inherit AuthorizationHandler<Requirement> ()

    override __.HandleRequirementAsync (ctx, requirement) =
        upcast task {
            let! result = requirement.Invoke services ctx
            if result then ctx.Succeed requirement
        }

type AuthorizationPolicyBuilder with
    member this.Require (f: 'dependencies -> AuthorizationHandlerContext -> bool Task) =
        this.AddRequirements (Requirement (fun services ctx -> f (DependencyInjection.resolve services) ctx))

type AuthorizationSettings<'t when 't: equality and 't :> IPolicy> =
    {
        InvokeHandlersAfterFailure: bool
        DefaultPolicy: 't option
    }

    static member Default: AuthorizationSettings<'t> =
        {
            InvokeHandlersAfterFailure = true
            DefaultPolicy = None
        }

    static member FromOptions (options: AuthorizationOptions) =
        {
            AuthorizationSettings.Default with
                InvokeHandlersAfterFailure = options.InvokeHandlersAfterFailure
        }

    member this.InvokeOnOptions (options: AuthorizationOptions) =
        options.InvokeHandlersAfterFailure <- this.InvokeHandlersAfterFailure

type Field<'field, 'arguments, 'source> with
    member this.Authorize<'t when 't: equality and 't :> IPolicy> (policy: 't) =
        let case, _ =
            FSharpValue.GetUnionFields (
                value = policy,
                unionType = typeof<'t>
            )
        this.AuthorizeWith (mkName case.Name)

let authorize policy = Operation.Configure <| fun (field: Field<'field, 'arguments, 'source>) ->
    field.Authorize policy
    field

type IGraphQLBuilder with
    member this.AddAuthorization<'t when 't: equality and 't :> IPolicy> (optionBuilder: AuthorizationSettings<'t> -> AuthorizationSettings<'t>) =
        this.Services.AddSingleton<IAuthorizationHandler, RequirementHandler> ()
        |> ignore
        this.AddGraphQLAuthorization (
            fun options ->
                let settings =
                    AuthorizationSettings<'t>.FromOptions options
                    |> optionBuilder
                settings.InvokeOnOptions options

                FSharpType.GetUnionCases typeof<'t>
                |> Array.map (
                    fun case ->
                        let name, case = mkName case.Name, FSharpValue.MakeUnion (case , [||]) |> unbox<'t>
                        options.AddPolicy (
                            name = name,
                            configurePolicy = Action<_> (case.Authorize >> ignore)
                        )
                        options.GetPolicy name, case
                )
                |> Array.tryPick (
                    fun (policy, case) ->
                        match settings.DefaultPolicy with
                        | Some case' when case' = case -> Some policy
                        | _ -> None
                )
                |> Option.iter options.set_DefaultPolicy
        )
