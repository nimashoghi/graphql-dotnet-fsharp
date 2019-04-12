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
open GraphQL.Server.Transports.Subscriptions.Abstractions
open Microsoft.AspNetCore.Authorization
open Microsoft.AspNetCore.Authentication
open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.DependencyInjection
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Primitives

[<Literal>]
let internal SubscriptionName = "graphql-ws"

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

type InitialMessagePayload = {
    Authorization: string voption
}
type SubscriptionAuthorizationListener (contextAccessor: IHttpContextAccessor, schemes) =
    let authenticationMiddleware = AuthenticationMiddleware (RequestDelegate (fun _ -> Task.CompletedTask), schemes)

    let (|InitialMessage|_|) (message: OperationMessage) =
        match message.Type with
        | MessageType.GQL_CONNECTION_INIT ->
            if isNull message.Payload then Some {Authorization = ValueNone} else
            Dictionary.ofObj message.Payload
            |> Option.map (
                fun dictionary ->
                    match dictionary.TryGetValue "Authorization" with
                    | true, (:? string as value) -> {Authorization = ValueSome value}
                    | _ -> {Authorization = ValueNone}
            )
        | _ -> None

    interface IOperationMessageListener with
        member __.AfterHandleAsync _ = Task.CompletedTask
        member __.BeforeHandleAsync messageContext =
            match messageContext.Message with
            | InitialMessage {Authorization = ValueSome authorization} ->
                contextAccessor.HttpContext.Request.Headers.["Authorization"] <- StringValues authorization
            | _ -> ()
            // contextAccessor.HttpContext.AuthenticateAsync () :> Task
            authenticationMiddleware.Invoke contextAccessor.HttpContext
        member __.HandleAsync _ = Task.CompletedTask

type SubscriptionAuthenticationMiddleware (next, schemes) =
    let authenticationMiddleware = AuthenticationMiddleware (next, schemes)

    member __.Invoke (context: HttpContext) =
        if List.ofSeq context.WebSockets.WebSocketRequestedProtocols <> [SubscriptionName]
        then authenticationMiddleware.Invoke context
        else next.Invoke context

type IApplicationBuilder with
    member this.UseGraphQLAuthentication () =
        this.UseMiddleware<SubscriptionAuthenticationMiddleware> Array.empty

type IGraphQLBuilder with
    member this.AddSubscriptionAuthorizationHandler () =
        this.Services
            .AddHttpContextAccessor()
            .AddScoped<IOperationMessageListener, SubscriptionAuthorizationListener>()
        |> ignore
        this

    member this.AddAuthorization<'t when 't: equality and 't :> IPolicy> (optionBuilder: AuthorizationSettings<'t> -> AuthorizationSettings<'t>) =
        this.AddSubscriptionAuthorizationHandler () |> ignore
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
