module GraphQL.FSharp.CommandLineSample.Schema

open System.Threading.Tasks
open GraphQL.FSharp.Builder

open GraphQL.FSharp.CommandLineSample.Model

let MyTypeGraph =
    object<MyType> {
        fields [
            field {
                method (fun this _ -> this.GetSomethingSync ())
            }
            field {
                methodAsync (fun this _ -> this.GetSomethingAsync ())
            }
        ]
    }

let Query =
    query [
        endpoint "getMyType" {
            resolveAsync (fun _ _ -> Task.FromResult <| MyType ())
        }
    ]

let Schema =
    schema {
        query Query
        types [
            MyTypeGraph
        ]
    }
