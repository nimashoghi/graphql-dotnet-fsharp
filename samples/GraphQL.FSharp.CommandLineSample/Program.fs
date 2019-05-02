module GraphQL.FSharp.CommandLineSample.Program

open GraphQL
open GraphQL.Types

open GraphQL.FSharp.CommandLineSample.Schema

type Schema with
    member this.ExecuteQuery query =
        this.Execute (fun options ->
            options.Schema <- this
            options.Query <- query
            options.Root <- null
        )


let ``basic mutation result`` () =
    printfn "%s" <| Schema.ExecuteQuery """
        query {
            getMyType {
                getSomethingSync
                getSomethingAsync
            }
        }
    """

[<EntryPoint>]
let main argv =
    ``basic mutation result`` ()
    0
