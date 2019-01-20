module GraphQL.FSharp.Tests.Builder.Enum

open NUnit.Framework
open Swensen.Unquote
open GraphQL.FSharp.Builder
open GraphQL.Types

open GraphQL.FSharp.Tests.Assert

[<Test>]
let ``Builder Argument basic test`` () =
    arg<int> {
        name "myArg"
    }
    |> argumentEqual "myArg" IntGraphType None
