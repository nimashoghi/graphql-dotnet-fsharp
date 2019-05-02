module GraphQL.FSharp.IntegrationTests.Tasks

open System.Threading.Tasks
open NUnit.Framework
open FSharp.Utils.Tasks
open GraphQL.FSharp.Builder
open GraphQL.FSharp.Types

open GraphQL.FSharp.TestUtils.Assert

[<AutoOpen>]
module GraphTypes =
    type MyType() =
        member __.GetSomethingSync () = "hello world"
        member __.GetSomethingAsync () = Task.FromResult "hello world async"


[<Literal>]
let QueryString = """
    query {
        GetMyType {
            GetSomethingSync
            GetSomethingAsync
        }
        GetMyTypeAsync {
            GetSomethingSync
            GetSomethingAsync
        }
    }
"""

[<Literal>]
let ExpectedResult = """
    {
        "data": {
            "GetMyType": {
                "GetSomethingSync": "hello world",
                "GetSomethingAsync": "hello world async"
            },
            "GetMyTypeAsync": {
                "GetSomethingSync": "hello world",
                "GetSomethingAsync": "hello world async"
            }
        }
    }
"""


[<Test>]
let ``Schema using synchronous resolver with methods returning task works properly`` () =
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
            field __ [
                name "GetMyType"
                resolve.method (fun _ _ -> vtask { return MyType () })
            ]
            field __ [
                name "GetMyTypeAsync"
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

    queryEqual QueryString ExpectedResult Schema
