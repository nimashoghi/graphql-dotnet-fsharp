namespace GraphQL.FSharp.TestServer

open System.Threading.Tasks
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.DependencyInjection
open GraphQL.Server
open GraphQL.Types
open GraphQL.Server.Ui.Playground
open GraphQL.FSharp.Builder
open GraphQL.FSharp.Server

module Model =
    type MyType() =
        member __.GetSomethingSync () = "hello world"
        member __.GetSomethingAsync () = Task.FromResult "hello world async"
        member __.MethodWithParam i j k l = sprintf "%i + %A + %A + %A" i j k l

    type MyUnion =
    | First of Name: string
    | Second of Age: int

    type MyEnum =
    | First = 0
    | Second = 1
    | Thrid = 2

module Schema =
    open Model

    let MyTypeGraph =
        object<MyType> {
            fields [
                field {
                    method (
                        fun
                            this
                            (args:
                            {|
                                IntegerParam: int
                                ListParam: int []
                                OptionalParam: int option
                                ResultParam: Result<int, string>
                            |})
                            -> this.MethodWithParam args.IntegerParam args.ListParam args.OptionalParam args.ResultParam
                    )
                }
                field {
                    method (fun this _ -> this.GetSomethingSync ())
                }
                field {
                    methodAsync (fun this _ -> this.GetSomethingAsync ())
                }
            ]
        }

    let MyUnionGraph = union.auto<MyUnion> ()
    let MyEnumGraph = enum.auto<MyEnum> ()

    let Query =
        query [
            endpoint "GetMyEnum" {
                resolveAsync (fun _ -> Task.FromResult <| MyEnum.Thrid)
            }
            endpoint "GetMyUnion" {
                resolveAsync (fun _ -> Task.FromResult <| First "hello")
            }
            endpoint "GetMyType" {
                resolveAsync (fun _ -> Task.FromResult <| MyType ())
            }
        ]

    let Schema =
        schema {
            query Query
            types [
                MyTypeGraph
                MyUnionGraph
                MyEnumGraph
            ]
        }

type Startup() =
    member __.ConfigureServices(services: IServiceCollection) =
        services
            .AddSingleton(Schema.Schema)
            .AddGraphQL(fun options ->
                options.ExposeExceptions <- false
                options.EnableMetrics <- true
            )
            .AddWebSockets()
            .AddDefaultFieldNameConverter()
            |> ignore
        ()

    member __.Configure(app: IApplicationBuilder, env: IHostingEnvironment) =
        if env.IsDevelopment() then
            app.UseDeveloperExceptionPage() |> ignore

        app.UseWebSockets () |> ignore

        app.UseGraphQL<Schema> "/graphql" |> ignore
        app.UseGraphQLWebSockets<Schema> "/graphql" |> ignore
        GraphQLPlaygroundOptions () |> app.UseGraphQLPlayground |> ignore
