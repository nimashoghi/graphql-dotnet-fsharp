[<AutoOpen>]
module GraphQL.FSharp.Main

open System
open FSharp.Control.Reactive
open GraphQL
open GraphQL.Language.AST
open GraphQL.Server
open GraphQL.Server.Ui.GraphiQL
open GraphQL.Server.Ui.Playground
open GraphQL.Server.Ui.Voyager
open GraphQL.Types
open GraphQL.Validation
open Iris
open Iris.Option.Builders
open Microsoft.Extensions.DependencyInjection
open Microsoft.AspNetCore.Builder
open Rx
open Rx.Builders

type IServiceCollection with
    // TODO: Better naming
    // TODO: More configurability
    member this.AddGraphQLFS<'injection, 'schema when 'schema : not struct and 'schema :> Schema> (f: 'injection -> 'schema) =
        this
            .AddTransient<IValidationRule>(Func.from validator)
            .AddSingleton<Schema>(Func.from (fun (provider: IServiceProvider) -> f (provider.GetService typeof<'injection> :?> 'injection) :> Schema))
            .AddGraphQL(fun options ->
                options.EnableMetrics <- true
                options.ExposeExceptions <- true)
            .AddWebSockets()
            .AddDataLoader()
            .AddUserContextBuilder(fun _ -> UserContext())

type IApplicationBuilder with
    // TODO: Better naming
    // TODO: More configurability
    member this.UseGraphQLFS<'injection, 'schema when 'schema :> Schema> (_: 'injection -> 'schema) =
        this
            .UseWebSockets()
            .UseGraphQL<Schema>()
            .UseGraphQLWebSockets<Schema>()
            .UseGraphiQLServer(GraphiQLOptions())
            .UseGraphQLPlayground(GraphQLPlaygroundOptions())
            .UseGraphQLVoyager(GraphQLVoyagerOptions())
