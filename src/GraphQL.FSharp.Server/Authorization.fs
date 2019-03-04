[<AutoOpen>]
module GraphQL.FSharp.Server.Authorization

open System
open FSharp.Reflection
open GraphQL.FSharp
open GraphQL.FSharp.BuilderBase
open GraphQL.FSharp.BuilderTypes
open GraphQL.Server
open GraphQL.Server.Authorization.AspNetCore
open Microsoft.AspNetCore.Authorization
open Microsoft.Extensions.DependencyInjection

let mkName name = sprintf "GraphQL.FSharp.Server.Auth.%s" name

type AuthorizationSettings<'t when 't: equality and 't: (member Authorize: IServiceProvider * AuthorizationPolicyBuilder -> AuthorizationPolicyBuilder)> =
    {
        InvokeHandlersAfterFailure: bool
        DefaultPolicy: 't option
    }

    static member inline Default: AuthorizationSettings<'t> =
        {
            InvokeHandlersAfterFailure = true
            DefaultPolicy = None
        }

    static member inline FromOptions (options: AuthorizationOptions) =
        {
            AuthorizationSettings.Default with
                InvokeHandlersAfterFailure = options.InvokeHandlersAfterFailure
        }

    member inline this.InvokeOnOptions (options: AuthorizationOptions) =
        options.InvokeHandlersAfterFailure <- this.InvokeHandlersAfterFailure

type Field<'arguments, 'field, 'source> with
    member inline this.Authorize< ^t when ^t: equality and ^t: (member Authorize: IServiceProvider * AuthorizationPolicyBuilder -> AuthorizationPolicyBuilder)> (policy: ^t) =
        let case, _ =
            FSharpValue.GetUnionFields (
                value = policy,
                unionType = typeof< ^t>
            )
        this.AuthorizeWith (mkName case.Name)

type FieldBuilder<'arguments, 'field, 'source> with
    [<CustomOperation "authorize">]
    member inline __.Authorize (state: State<Field<'arguments, 'field, 'source>>, policy) =
        (fun (field: Field<'arguments, 'field, 'source>) -> field.Authorize policy; field) @@ state

type IGraphQLBuilder with
    member inline this.AddAuthorization< ^t when ^t: equality and ^t: (member Authorize: IServiceProvider * AuthorizationPolicyBuilder -> AuthorizationPolicyBuilder)> (optionBuilder: AuthorizationSettings< ^t> -> AuthorizationSettings< ^t>) =
        this.AddGraphQLAuthorization (
            fun options ->
                let settings =
                    AuthorizationSettings< ^t>.FromOptions options
                    |> optionBuilder
                settings.InvokeOnOptions options

                FSharpType.GetUnionCases typeof< ^t>
                |> Array.map (
                    fun case ->
                        let name, case = mkName case.Name, FSharpValue.MakeUnion (case , [||]) |> unbox< ^t>
                        options.AddPolicy (
                            name = name,
                            configurePolicy = fun policyBuilder ->
                                (^t: (member Authorize: IServiceProvider * AuthorizationPolicyBuilder -> AuthorizationPolicyBuilder) (case, this.Services.BuildServiceProvider (), policyBuilder))
                                |> ignore
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
