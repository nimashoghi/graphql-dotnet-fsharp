open System
open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.Logging
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.DependencyInjection
open GraphQL.FSharp
open GraphQL.Types
open GraphQL.FSharp.Builder
open Apollo

let q = query {
    name "Query"
    fields [
        field {
            name "getSomething"
            resolve (fun _ -> Observable.unit 1)
        }
        field {
            name "getSomethingElse"
            resolve (fun _ -> Observable.unit "sup")
        }
    ]
}

let m = mutation {
    name "Mutation"
}

let schema = schema {
    query q
    mutation m
}

let configureApp (app : IApplicationBuilder) =
    app.UseGraphQLFS schema |> ignore

let configureServices (services : IServiceCollection) =
    services.AddGraphQLFS schema |> ignore

[<EntryPoint>]
let main argv =
    WebHostBuilder()
        .UseKestrel()
        .Configure(Action<IApplicationBuilder> configureApp)
        .ConfigureServices(configureServices)
        // .UseStartup<Startup>()
        .Build()
        .Run()
    0
