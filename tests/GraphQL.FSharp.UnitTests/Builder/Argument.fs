module GraphQL.FSharp.UnitTests.Builder.Enum

open NUnit.Framework
open Swensen.Unquote
open GraphQL.FSharp.Builder
open GraphQL.Types

open GraphQL.FSharp.UnitTests.Assert

[<Test>]
let ``Builder Argument basic test`` () =
    arg<int> {
        name "myArg"
    }
    |> argumentEqual "myArg" (nonNull IntGraphType) None
