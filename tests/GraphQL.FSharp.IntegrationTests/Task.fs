module GraphQL.FSharp.IntegrationTests.Tasks

open System.Threading.Tasks
open NUnit.Framework
open Swensen.Unquote
open GraphQL.FSharp
open GraphQL.FSharp.Builder

open GraphQL.FSharp.TestUtils.Assert

[<AutoOpen>]
module GraphTypes =
    type MyType() =
        member __.GetSomethingSync () = "hello world"
        member __.GetSomethingAsync () = Task.FromResult "hello world async"


[<Literal>]
let Query = """
    query {
        getMyType {
            getSomethingSync
            getSomethingAsync
        }
    }
"""

[<Literal>]
let ExpectedResult = """
    {
        "data": {
            "getMyType": {
                "getSomethingSync": "hello world",
                "getSomethingAsync": "hello world async"
            }
        }
    }
"""


[<Test>]
let ``Schema using synchronous resolver with methods returning task works properly`` () =
    let _myTypeGraph = Auto.Object<MyType>
    let myQuery =
        query {
            fields [
                field {
                    name "getMyType"
                    resolve (fun _ -> MyType ())
                }
            ]
        }
    let mySchema =
        schema {
            query myQuery
        }

    queryEqual Query ExpectedResult mySchema

[<Test>]
let ``Schema using asynchronous resolver with methods returning task works properly`` () =
    let _myTypeGraph = Auto.Object<MyType>
    let myQuery =
        query {
            fields [
                field {
                    name "getMyType"
                    resolveAsync (fun _ -> Task.FromResult <| MyType ())
                }
            ]
        }
    let mySchema =
        schema {
            query myQuery
        }

    queryEqual Query ExpectedResult mySchema
