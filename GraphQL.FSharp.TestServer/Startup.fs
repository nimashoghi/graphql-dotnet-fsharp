namespace GraphQL.FSharp.TestServer

open System
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.DependencyInjection

open GraphQL
open GraphQL.Server
open GraphQL.Server.Ui.Playground
open GraphQL.FSharp.Builder

module GQL =
    let myQuery () = query {
        fields [
            field {
                name "myQuery"
                resolve (fun ctx -> 1)
            }
        ]
    }

    let mySchema () = schema {
        query (myQuery ())
    }

type Startup() =

    // This method gets called by the runtime. Use this method to add services to the container.
    // For more information on how to configure your application, visit https://go.microsoft.com/fwlink/?LinkID=398940
    member this.ConfigureServices(services: IServiceCollection) =
        services.AddSingleton<Types.Schema> (GQL.mySchema ()) |> ignore
        services
            .AddGraphQL(fun options ->
                options.ExposeExceptions <- true)
            |> ignore
        ()

    // This method gets called by the runtime. Use this method to configure the HTTP request pipeline.
    member this.Configure(app: IApplicationBuilder, env: IHostingEnvironment) =
        if env.IsDevelopment() then
            app.UseDeveloperExceptionPage() |> ignore

        app.UseGraphQL<Types.Schema> "/graphql" |> ignore
        app.UseGraphQLPlayground (GraphQLPlaygroundOptions()) |> ignore

        app.Run(fun context -> context.Response.WriteAsync("Hello World!")) |> ignore
