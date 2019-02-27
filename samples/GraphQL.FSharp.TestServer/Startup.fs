namespace GraphQL.FSharp.TestServer

open System.Threading.Tasks
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.DependencyInjection
open GraphQL.Server
open GraphQL.Server.Ui.Playground
open GraphQL.FSharp.Builder
open GraphQL.FSharp.Server
open GraphQL.FSharp.Types
open Validation.Builder

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

[<AutoOpen>]
module Validation =
    let validateAge (age: int) =
        if age = 21
        then Ok 21
        else Error ["Invalid Age"]

    let validateHeight (height: float) =
        if height = 180.
        then Ok 180.
        else Error ["Invalid Height"]

    let validateName (name: string) =
        if name = "Alice"
        then Ok "Alice"
        else Error ["Invalid Name"]

module Schema =
    open Model

    let MyTypeGraph =
        object<MyType> {
            fields [
                field __ {
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
                field __ {
                    method (fun this _ -> this.GetSomethingSync ())
                }
                field __ {
                    methodAsync (fun this _ -> this.GetSomethingAsync ())
                }
            ]
        }

    let MyUnionGraph = union.auto<MyUnion> ()
    let MyEnumGraph = enum.auto<MyEnum> ()

    let Query =
        query [
            endpoint __ "GetMyEnum" {
                resolveAsync (fun _ _ -> Task.FromResult <| MyEnum.Thrid)
            }
            endpoint __ "GetMyUnion" {
                resolveAsync (fun _ _ -> Task.FromResult <| First "hello")
            }
            endpoint __ "GetMyType" {
                resolveAsync (fun _ _ -> Task.FromResult <| MyType ())
            }
            endpoint __ "Validate" {
                validate (
                    fun (args: {|Age: int; Height: float; Name: string|}) -> validation {
                        validate age in validateAge args.Age
                        validate height in validateHeight args.Height
                        validate name in validateName args.Name

                        return
                            {|
                                args with
                                    Age = age
                                    Height = height
                                    Name = name
                            |}
                    }
                )
                resolve (fun _ args -> sprintf "%s_%i_%.0f" args.Name args.Age args.Height)
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
