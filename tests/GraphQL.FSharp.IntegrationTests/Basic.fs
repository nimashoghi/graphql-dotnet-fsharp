module GraphQL.FSharp.IntegrationTests.Tasks

open System.Threading.Tasks
open NUnit.Framework
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
    let Query = [
            endpoint __ "GetMyType" {
                resolve (fun _ _ -> Task.FromResult(MyType()))
            }
            endpoint __ "GetMyTypeAsync" {
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

    queryEqual QueryString ExpectedResult Schema
