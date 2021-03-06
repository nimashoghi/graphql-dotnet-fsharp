module GraphQL.FSharp.Server.GraphQLServer

open GraphQL.Server
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

let addFSharp (builder: IGraphQLBuilder) = builder.AddFSharp ()

let addWebsockets (builder: IGraphQLBuilder) = builder.AddWebSockets ()

let addNameConverter (builder: IGraphQLBuilder) = builder.AddDefaultFieldNameConverter ()

let addDocumentExecutor (builder: IGraphQLBuilder) = builder.AddDocumentExecutor ()

let addSubscriptionAuthorizationHandler (builder: IGraphQLBuilder) = builder.AddSubscriptionAuthorizationHandler ()

let addAuthorization<'t when 't: equality and 't :> IPolicy> optionBuilder (builder: IGraphQLBuilder) = builder.AddAuthorization<'t> optionBuilder
