module GraphQL.FSharp.IntegrationTests.Validation

open NUnit.Framework
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


[<Literal>]
let QueryString = """
    query {
        ValidateSuccess: Validate(Name: "Alice", Age: 21, Height: 180.0)
        ValidateFailure: Validate(Name: "Hello", Age: 20, Height: 182.0)
    }
"""

[<Literal>]
let ExpectedResult = """
    {
        "data": {
            "ValidateSuccess": "Alice_21_180",
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
            }
        ]
    }
"""

[<Test>]
let ``Schema using synchronous resolver with methods returning task works properly`` () =
    let Query =
        query [
            endpoint __ "Validate" {
                validate (
                    fun (args: {|Age: int; Height: float; Name: string|}) -> validation {
                        validate age in validateAge args.Age
                        validate height in validateHeight args.Height
                        validate name in validateName args.Name

                        return
                            {|
                                args with
                                    Age = age
                                    Height = height
                                    Name = name
                            |}
                    }
                )
                resolve (fun _ args -> sprintf "%s_%i_%.0f" args.Name args.Age args.Height)
            }
        ]
    let Schema =
        schema {
            query Query
        }

    queryEqual QueryString ExpectedResult Schema
