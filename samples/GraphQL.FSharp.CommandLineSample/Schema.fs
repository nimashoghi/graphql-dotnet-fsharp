module GraphQL.FSharp.CommandLineSample.Schema

open System.Threading.Tasks
open GraphQL.FSharp.Builder
open GraphQL.FSharp.Types

open GraphQL.FSharp.CommandLineSample.Model

let MyTypeGraph =
    object<MyType> {
        fields [
            field __ {
                method (fun this _ -> Task.FromResult(this.GetSomethingSync()))
            }
            field __ {
                method (fun this _ -> this.GetSomethingAsync())
            }
        ]
    }

let Query =
    query [
        endpoint __ "getMyType" {
            resolve (fun _ _ -> Task.FromResult(MyType()))
        }
    ]

let Schema =
    schema {
        query Query
        types [
            MyTypeGraph
        ]
    }
