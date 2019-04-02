module GraphQL.FSharp.CommandLineSample.Schema

open FSharp.Utils.Tasks
open GraphQL.FSharp.Builder
open GraphQL.FSharp.Types

open GraphQL.FSharp.CommandLineSample.Model

let MyTypeGraph =
    object<MyType> [
        fields [
            field __ [
                resolve.method (fun this _ -> vtask { return this.GetSomethingSync () })
            ]
            field __ [
                resolve.method (fun this _ -> vtask { return! this.GetSomethingAsync () })
            ]
        ]
    ]

let Query =
    query [
        field __  [
            name "getMyType"
            resolve.method (fun _ _ -> vtask { return MyType () })
        ]
    ]

let Schema =
    schema [
        Query
        types [
            MyTypeGraph
        ]
    ]
