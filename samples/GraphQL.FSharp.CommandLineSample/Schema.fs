module GraphQL.FSharp.CommandLineSample.Schema

open System.Threading.Tasks
open GraphQL.FSharp
open GraphQL.FSharp.Builder

open GraphQL.FSharp.CommandLineSample.Model

let MyTypeGraph = Auto.Object<MyType>

let Query =
    query [
        field {
            name "getMyType"
            resolveAsync (fun _ -> Task.FromResult <| MyType ())
        }
    ]

let Schema =
    schema {
        query Query
        types [
            MyTypeGraph
        ]
    }
