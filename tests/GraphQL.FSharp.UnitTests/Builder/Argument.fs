module GraphQL.FSharp.UnitTests.Builder.Argument

open NUnit.Framework
open Swensen.Unquote
open GraphQL.FSharp
open GraphQL.FSharp.Builder
open GraphQL.Types

open GraphQL.FSharp.TestUtils.Assert


// TODO: Add tests for option types to check nullable fields/args

[<Test>]
let ``configure test`` () =
    argument<int> {
        name "myArg"
        configure (fun arg -> arg.ResolvedType <- FloatGraphType ())
        configure (fun arg -> arg.Name <- "changedName")
    }
    |> argumentEqual "changedName" (nullable FloatGraphType) None

[<Test>]
let ``basic test`` () =
    argument<int> {
        name "myArg"
    }
    |> argumentEqual "myArg" (nonNull IntGraphType) None
