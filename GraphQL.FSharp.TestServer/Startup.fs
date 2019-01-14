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
    [<CLIMutable>]
    type MyType = {
        name: string
    }

    let myType = object {
        name "myTypeManual"
        fields [
            field {
                get (fun x -> x.name)
            }
        ]
    }

    let myInferredType = Auto.Object<MyType>
    myInferredType.Name <- "inferredMyType"
    let myInferredInputType = Auto.InputObject<MyType>
    myInferredInputType.Name <- "myInferredInputType"

    let myEnum = enum {
        name "myEnum"
        cases [
            "MyFirstCase" => "MyFirstCase"
        ]
    }

    type MySecondEnum =
    | First
    | Second

    let mySecondEnum = Auto.Enum<MySecondEnum>

    type IMyInterface =
        abstract member Name: string

    // type MyUnion =
    // | FirstUnion of Name: string * Age: int

    // let myAutoUnion = Auto.Union<MyUnion>

    let myQuery = query {
        fields [
            field {
                name "getMyType"
                resolve (fun _ -> {name = "sup"})
            }
            field {
                name "myQuery"
                resolve (fun ctx -> arg<int>.Get "myArg" ctx :: [1; 2; 3; 4; 5])
                args [
                    arg<int>.New ("myArg", 1)
                ]
            }
            fieldOf myEnum {
                name "myEnum"
                resolve (fun _ -> "MyFirstCase")
                ofType myEnum
            }
            fieldOf mySecondEnum {
                name "mySecondEnum"
                resolve (fun _ -> First)
            }
            field {
                name "withInput"
                args [
                    arg<MyType>.New "myArg"
                ]
                resolve (fun ctx -> ctx.GetArgument<MyType> "myArg")
            }
            // field {
            //     name "myAutoUnion"
            //     resolve (fun _ -> FirstUnion ("sup", 12))
            // }
        ]
    }

    let mySchema = schema {
        query myQuery
    }

type Startup() =
    member this.ConfigureServices(services: IServiceCollection) =
        services
            .AddSingleton(GQL.mySchema)
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
