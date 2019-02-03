module GraphQL.FSharp.TestServer.Program

open Microsoft.AspNetCore
open Microsoft.AspNetCore.Hosting

[<EntryPoint>]
let main args =
    WebHost
        .CreateDefaultBuilder(args)
        .UseStartup<Startup>()
        .Build()
        .Run()

    0
