module GraphQL.FSharp.IntegrationTests.Validation

open NUnit.Framework
open FSharp.Utils.Tasks
open GraphQL.FSharp.Builder
open GraphQL.FSharp.Types
open Validation.Builder

open GraphQL.FSharp.TestUtils.Assert

[<AutoOpen>]
module Validation =
    let validateAge (age: int) =
        if age = 21
        then Ok 21
        else Error ["Invalid Age"]

    let validateHeight (height: float) =
        if height = 180.
        then Ok 180.
        else Error ["Invalid Height"]

    let validateName (name: string) =
        if name = "Alice"
        then Ok "Alice"
        else Error ["Invalid Name"]

    let validateAsyncName (name: string) =
        task {
            if name = "AsyncAlice"
            then return Ok "AsyncAlice"
            else return Error ["Invalid AsyncName"]
        }

[<Literal>]
let QueryString = """
    query {
        ValidateSuccess: Validate(Name: "Alice", AsyncName: "AsyncAlice", Age: 21, Height: 180.0)
        ValidateFailure: Validate(Name: "Hello", AsyncName: "AsyncHello", Age: 20, Height: 182.0)
    }
"""

[<Literal>]
let ExpectedResult = """
    {
        "data": {
            "ValidateSuccess": "Alice_AsyncAlice_21_180",
            "ValidateFailure": null
        },
        "errors": [
            {
                "message": "Invalid Age"
            },
            {
                "message": "Invalid Height"
            },
            {
                "message": "Invalid Name"
            },
            {
                "message": "Invalid AsyncName"
            }
        ]
    }
"""

[<Test>]
let ``Schema using asynchronous resolver with methods returning task works properly`` () =
    let Query =
        query [
            field __ [
                name "Validate"
                validate (
                    fun (args: {|Age: int; Height: float; Name: string; AsyncName: string|}) -> validation {
                        validate age in validateAge args.Age
                        validate height in validateHeight args.Height
                        validate name in validateName args.Name
                        validate asyncName in validateAsyncName args.AsyncName
                        return
                            {|
                                args with
                                    Age = age
                                    Height = height
                                    Name = name
                                    AsyncName = asyncName
                            |}
                    }
                )
                resolve.method (fun _ args -> vtask { return sprintf "%s_%s_%i_%.0f" args.Name args.AsyncName args.Age args.Height })
            ]
        ]
    let Schema =
        schema [
            Query
        ]

    queryEqual QueryString ExpectedResult Schema
