namespace GraphQL.FSharp.TestServer

open System
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.DependencyInjection

open GraphQL
open GraphQL.Types
open GraphQL.Server
open GraphQL.Server.Ui.Playground
open GraphQL.FSharp.Builder

module GQL =
    let myQuery = query {
        fields [
            field {
                name "myQuery"
                // ``type`` typeof<IntGraphType>
                resolve (fun ctx -> 1)
            }
        ]
    }

    let mySchema = schema {
        query myQuery
    }

type Startup() =
    member this.ConfigureServices(services: IServiceCollection) =
        services
            .AddSingleton<Schema>(GQL.mySchema)
            |> ignore
        services
            .AddGraphQL(fun options ->
                options.ExposeExceptions <- true)
            |> ignore
        ()

    member this.Configure(app: IApplicationBuilder, env: IHostingEnvironment) =
        if env.IsDevelopment() then
            app.UseDeveloperExceptionPage() |> ignore

        app.UseGraphQL<Schema> "/graphql" |> ignore
        GraphQLPlaygroundOptions () |> app.UseGraphQLPlayground |> ignore

        app.Run(fun context -> context.Response.WriteAsync("Hello World!")) |> ignore
