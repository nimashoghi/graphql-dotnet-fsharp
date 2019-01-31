module GraphQL.FSharp.IntegrationTests.Mutation

open System
open NUnit.Framework
open GraphQL.FSharp
open GraphQL.FSharp.Builder

open GraphQL.FSharp.TestUtils.Assert

[<CLIMutable>]
type ReturnType = {
    Id: Guid
    Name: string
}

[<Literal>]
let MutationString = """
    mutation {
        login(name: "Username", password: "89d5bd13-c110-41c3-82ed-b265a441e7f9") {
            id
            name
        }
    }
"""

[<Literal>]
let Expected = """
    {
        "data": {
            "login": {
                "id": "89d5bd13-c110-41c3-82ed-b265a441e7f9",
                "name": "Username"
            }
        }
    }
"""

[<Test>]
let ``Mutation basic`` () =
    let ReturnTypeGraph = Auto.Object<ReturnType>

    let Query =
        query []
    let Mutation =
        mutation [
            field {
                name "login"
                arguments [
                    Define.Argument<string> "name"
                    Define.Argument<Guid> "password"
                ]
                resolve (fun ctx ->
                    let name = ctx.GetArgument<string> "name"
                    let password = ctx.GetArgument<Guid> "password"

                    {Id = password; Name = name}
                )
            }
        ]
    let Schema =
        schema {
            query Query
            mutation Mutation
            types [
                ReturnTypeGraph
            ]
        }

    queryEqual MutationString Expected Schema
