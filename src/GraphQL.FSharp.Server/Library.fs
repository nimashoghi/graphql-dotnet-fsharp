module GraphQL.FSharp.Server.GraphQLServer

open System
open GraphQL.Server
open Microsoft.AspNetCore.Authorization
open Microsoft.Extensions.DependencyInjection

let addGraphQL optionBuilder (services: IServiceCollection) =
    services
        .AddGraphQL(
            fun options ->
                options.ExposeExceptions <- false
                options.EnableMetrics <- true
                optionBuilder options
        )
        .AddDefaultFieldNameConverter()

let addWebsockets (builder: IGraphQLBuilder) = builder.AddWebSockets()

let addNameConverter (builder: IGraphQLBuilder) = builder.AddDefaultFieldNameConverter()

let inline addAuthorization< ^t when ^t: equality and ^t: (member Authorize: IServiceProvider * AuthorizationPolicyBuilder -> AuthorizationPolicyBuilder)> optionBuilder (builder: IGraphQLBuilder) =
    builder.AddAuthorization< ^t> optionBuilder
