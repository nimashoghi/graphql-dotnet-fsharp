module GraphQL.FSharp.UnitTests.Builder.Argument

open NUnit.Framework
open GraphQL.FSharp.Builder
open GraphQL.FSharp.Types
open GraphQL.Types

open GraphQL.FSharp.TestUtils.Assert


// TODO: Add tests for option types to check nullable fields/args

[<Test>]
let ``configure test`` () =
    argument<int> __ {
        name "myArg"
        configure (fun arg -> arg.ResolvedType <- FloatGraph)
        configure (fun arg -> arg.Name <- "changedName")
    }
    |> argumentEqual "changedName" (nullable FloatGraphType) None

[<Test>]
let ``basic test`` () =
    argument<int> __ {
        name "myArg"
    }
    |> argumentEqual "myArg" (nonNull IntGraphType) None
