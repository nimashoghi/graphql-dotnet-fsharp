namespace GraphQL.FSharp.TestServer

open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.DependencyInjection
open GraphQL.Types
open GraphQL.Server
open GraphQL.Server.Ui.GraphiQL
open GraphQL.Server.Ui.Playground
open GraphQL.FSharp
open GraphQL.FSharp.Builder

module Model =
    type IUser =
        abstract member GetName: System.Guid System.Collections.Generic.List -> string

    [<CLIMutable>]
    type User =
        {
            Name: string
        }
        interface IUser with
            member this.GetName id = sprintf "%s-%s" this.Name (System.String.Join(' ', id |> Seq.map (fun id -> id.ToString ())))

    [<CLIMutable>]
    type Website = {
        Users: User list
    }

    type MyError = {
        description: string
        code: int
    }

    type ResultClass () =
        member __.Ok (x: int) : Result<int, MyError> = Ok x
        member __.Error (x: int) : Result<int, MyError> = Error {description = "messed up"; code = 12}

    type UserUnion =
    | DescriptionUser of User: User * Description: string
    | Metadata of User: User * Metadata: int

module Schema =
    open Model

    let IUserGraph = Auto.Interface<IUser>
    let UserGraph = Auto.Object<User>
    let WebsiteGraph = Auto.Object<Website>
    let UserUnionGraph = Auto.Union<UserUnion>
    let ResultClassGraph = Auto.Object<ResultClass>

    let user = {Name = "sup"}
    let website = {Users = [user]}

    let Query =
        query [
            field {
                name "getUser"
                resolve (fun _ -> user :> IUser)
            }
            field {
                name "getUserUnion"
                resolve (fun _ -> DescriptionUser (user, "Sup"))
            }
            field {
                name "getWebsite"
                resolve (fun _ -> website)
            }
            field {
                name "getResult"
                resolve (fun _ -> ResultClass ())
            }
        ]

    let Schema =
        schema {
            types [
                IUserGraph
                ResultClassGraph
                UserGraph
                UserUnionGraph
                WebsiteGraph
            ]
            query Query
        }

type Startup() =
    member this.ConfigureServices(services: IServiceCollection) =
        services
            .AddSingleton(Schema.Schema)
            .AddGraphQL(fun options ->
                options.ExposeExceptions <- false
                options.EnableMetrics <- true)
            .AddWebSockets()
            |> ignore
        ()

    member this.Configure(app: IApplicationBuilder, env: IHostingEnvironment) =
        if env.IsDevelopment() then
            app.UseDeveloperExceptionPage() |> ignore

        app.UseWebSockets() |> ignore

        app.UseGraphQL<Schema> "/graphql" |> ignore
        app.UseGraphQLWebSockets<Schema> "/graphql" |> ignore
        GraphQLPlaygroundOptions () |> app.UseGraphQLPlayground |> ignore
        GraphiQLOptions () |> app.UseGraphiQLServer |> ignore

        app.Run(fun context -> context.Response.WriteAsync("Hello World!")) |> ignore
