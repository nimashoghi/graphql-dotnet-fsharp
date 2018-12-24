open System
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.DependencyInjection
open GraphQL.FSharp
open GraphQL.FSharp.Builder
open Apollo

type Record = {
    s: string
}
let record = object {
    name "Record"
    description "Some Record"
    fields [
        field {
            name "s"
            get (fun record -> record.s)
        }
    ]
}

let q = query {
    fields [
        field {
            name "getSomething"
            resolve (fun _ -> Observable.unit [1;2;3;4;5])
        }
        field {
            name "getSomethingElse"
            resolve (fun _ -> Observable.unit {s = "sdfs"})
        }
    ]
}

let mutable k = 0

let m = mutation {
    fields [
        field {
            name "mutateSomething"
            arguments [
                argument {
                    name "k"
                    defaultValue 1
                }
                argument {
                    name "l"
                    defaultValue 1
                }
            ]
            resolve (fun ctx ->
                Arg<int>.get "k" ctx
                |> Observable.map (fun i -> k <- k + i; k))
        }
    ]
}

let schema = schema {
    query q
    mutation m
    types [
        record
    ]
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
