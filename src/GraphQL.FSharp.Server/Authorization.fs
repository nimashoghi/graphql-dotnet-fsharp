[<AutoOpen>]
module GraphQL.FSharp.Server.Authorization

open FSharp.Reflection
open GraphQL.FSharp
open GraphQL.FSharp.Types
open GraphQL.Server
open GraphQL.Server.Authorization.AspNetCore
open Microsoft.AspNetCore.Authorization

let mkName name = sprintf "GraphQL.FSharp.Server.Auth.%s" name

type Field<'field, 'source> with
    member inline this.Authorize< ^t when ^t: (static member Authorize: AuthorizationPolicyBuilder -> (^t -> AuthorizationPolicyBuilder))> (policy: ^t) =
        let case, _ =
            FSharpValue.GetUnionFields (
                value = policy,
                unionType = typeof< ^t>
            )
        this.AuthorizeWith (mkName case.Name)

type FieldBuilder<'field, 'source> with
    [<CustomOperation "authorize">]
    member inline __.Authorize (field: Field<'field, 'source>, policy) =
        field.Authorize policy
        field

type IGraphQLBuilder with
    member inline this.AddAuthorization< ^t when ^t: (static member Authorize: AuthorizationPolicyBuilder -> (^t -> AuthorizationPolicyBuilder))> (optionBuilder: AuthorizationOptions -> unit) =
        this.AddGraphQLAuthorization (
            fun options ->
                optionBuilder options

                FSharpType.GetUnionCases typeof< ^t>
                |> Array.map (fun case -> mkName case.Name, FSharpValue.MakeUnion (case , [||]) :?> ^t)
                |> Array.iter (fun (name, case) ->
                    options.AddPolicy (
                        name = name,
                        configurePolicy = fun policyBuilder ->
                            (^t: (static member Authorize: AuthorizationPolicyBuilder -> (^t -> AuthorizationPolicyBuilder)) policyBuilder) case
                            |> ignore
                    )
                )
        )
