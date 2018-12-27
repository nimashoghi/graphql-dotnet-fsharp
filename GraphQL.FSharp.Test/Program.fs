open System
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.DependencyInjection
open GraphQL.FSharp
open GraphQL.FSharp.Builder
open Apollo

[<CLIMutable>]
type MyTesterino = {
    name: string
    friends: int
    hello: float
}
let myTesterino = record<MyTesterino> {
    description "my testerino"
}

[<CLIMutable>]
type Record = {
    s: string
}
let myRec = object {
    name "Record"
    description "Some Record"
    fields [
        field {
            name "s"
            get (fun myRec -> myRec.s)
        }
    ]
}

[<CLIMutable>]
type Input = {
    w: string
    j: int
}
let someInput = input {
    name "Input"
    description "Some Input"
    fields [
        field {
            name "w"
            get (fun input -> input.w)
        }
        field {
            name "j"
            get (fun input -> input.j)
        }
    ]
}

let q = query {
    fields [
        field {
            name "getSomething"
            resolve (fun _ -> Observable.unit [1; 2; 3; 4; 5])
        }
        field {
            name "getSomethingElse"
            resolve (fun _ -> Observable.unit {s = "sdfs"})
        }
        field {
            name "getTesterino"
            resolve (fun _ -> Observable.unit {name = "sup"; friends = 2; hello = 2.4})
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
                arg<int>.["k"] ctx
                |> Observable.map (fun i -> k <- k + i; k))
        }

        field {
            name "mutateCustom"
            arguments [
                argument<Input> {
                    name "input"
                }
            ]
            resolve (fun ctx -> arg<Input>.["input"] ctx |> Observable.map (fun i -> i.j))
        }
    ]
}

let schema = schema {
    query q
    mutation m
    types [
        myRec
        someInput
        myTesterino
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
