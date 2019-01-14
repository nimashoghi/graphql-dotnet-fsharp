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
open GraphQL.FSharp
open GraphQL.FSharp.Builder
open System.Threading.Tasks

module GQL =
    type MyType = {
        name: string
    }

    let myType = object {
        fields [
            field {
                get (fun x -> x.name)
            }
        ]
    }

    let myQuery = query {
        fields [
            field {
                name "getMyType"
                resolve (fun _ -> {name = "sup"})
            }
            field {
                name "myQuery"
                resolve (fun ctx -> argument<int>.Get "myArg" ctx :: [1; 2; 3; 4; 5])
                arguments [
                    argument<int>.New ("myArg", 1)
                ]
            }
        ]
    }

    let mySchema = schema {
        query myQuery
        types [
            myType
        ]
    }

type Startup() =
    member this.ConfigureServices(services: IServiceCollection) =
        services
            .AddGraphQL(fun options ->
                options.ExposeExceptions <- true)
            .AddFSharp GQL.mySchema
            |> ignore
        ()

    member this.Configure(app: IApplicationBuilder, env: IHostingEnvironment) =
        if env.IsDevelopment() then
            app.UseDeveloperExceptionPage() |> ignore

        app.UseGraphQL<Schema> "/graphql" |> ignore
        GraphQLPlaygroundOptions () |> app.UseGraphQLPlayground |> ignore

        app.Run(fun context -> context.Response.WriteAsync("Hello World!")) |> ignore
