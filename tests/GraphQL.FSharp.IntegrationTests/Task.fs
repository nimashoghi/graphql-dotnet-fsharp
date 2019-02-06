module GraphQL.FSharp.IntegrationTests.Tasks

open System.Threading.Tasks
open NUnit.Framework
open GraphQL.FSharp
open GraphQL.FSharp.Builder

open GraphQL.FSharp.TestUtils.Assert

[<AutoOpen>]
module GraphTypes =
    [<Auto>]
    type MyType() =
        member __.GetSomethingSync () = "hello world"
        member __.GetSomethingAsync () = Task.FromResult "hello world async"


[<Literal>]
let QueryString = """
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
    let MyTypeGraph = Auto.Object<MyType>
    let Query =
        query [
            field {
                name "getMyType"
                resolve (fun _ -> MyType ())
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

[<Test>]
let ``Schema using asynchronous resolver with methods returning task works properly`` () =
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

    queryEqual QueryString ExpectedResult Schema

[<Test>]
let ``getAsync test`` () =
    let MyTypeGraph = Auto.Object<MyType>
    let MyObjectGraph =
        object {
            name "MyObject"
            fields [
                field {
                    getAsync (fun (x: MyType) -> x.GetSomethingAsync ())
                }
                field {
                    get (fun (x: MyType) -> x.GetSomethingSync ())
                }
            ]
        }

    let Query =
        query [
            fieldOf MyObjectGraph {
                name "getMyType"
                resolve (fun _ -> MyType ())
            }
        ]
    let Schema =
        schema {
            query Query
            types [
                MyTypeGraph
                MyObjectGraph
            ]
        }

    queryEqual QueryString ExpectedResult Schema
