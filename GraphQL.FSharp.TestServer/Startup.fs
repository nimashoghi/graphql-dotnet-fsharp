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
    type IMyInterface =
        abstract member SomeName: string

    type MyImplementation() =
        member val SomeOtherName = ""

        interface IMyInterface with
            member val SomeName = ""

    type MyImplementationNum2() =
        member val SomeThirdName = ""

        interface IMyInterface with
            member val SomeName = ""

    let myInterface = Auto.Interface<IMyInterface>
    let myImplementation = Auto.Object<MyImplementation>
    let myImplementationNum2 = Auto.Object<MyImplementationNum2>

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

    type MyUnion =
    | FirstUnion of Name: string * Age: int
    | SecondUnion of Something: float * Id: System.Guid

    let myAutoUnion = Auto.Union<MyUnion>

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
            field {
                name "myAutoUnion"
                resolve (fun _ -> FirstUnion ("sup", 12))
            }
            field {
                name "myAutoUnionList"
                resolve (fun _ -> [
                    FirstUnion ("sup", 12)
                    FirstUnion ("dfsjiosh", 122)
                    SecondUnion (1.2, Guid.NewGuid())
                    SecondUnion (1.5, Guid.NewGuid())
                    SecondUnion (1.3, Guid.NewGuid())
                ])
            }
            field {
                name "myImpl"
                resolve (fun _ -> MyImplementation())
            }
            field {
                name "myImplList"
                resolve (fun _ -> [
                    MyImplementation() :> IMyInterface
                    MyImplementationNum2() :> IMyInterface
                    MyImplementation() :> IMyInterface
                ])
            }
            field {
                name "myInterface"
                resolve (fun _ -> MyImplementation() :> IMyInterface)
            }
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
                options.ExposeExceptions <- true
                options.EnableMetrics <- true)
            .AddDataLoader()
            |> ignore
        ()

    member this.Configure(app: IApplicationBuilder, env: IHostingEnvironment) =
        if env.IsDevelopment() then
            app.UseDeveloperExceptionPage() |> ignore

        app.UseGraphQL<Schema> "/graphql" |> ignore
        app.UseGraphQLWebSockets<Schema> "/graphql" |> ignore
        GraphQLPlaygroundOptions () |> app.UseGraphQLPlayground |> ignore

        app.Run(fun context -> context.Response.WriteAsync("Hello World!")) |> ignore
