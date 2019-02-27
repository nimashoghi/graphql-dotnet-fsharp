module GraphQL.FSharp.UnitTests.Builder.Argument

open NUnit.Framework
open GraphQL.FSharp.Builder
open GraphQL.FSharp.Types
open GraphQL.Types

open GraphQL.FSharp.TestUtils.Assert

let getArg name (field: #FieldType) =
    field.Arguments
    |> Seq.find (fun arg -> arg.Name = name)

[<Test>]
let ``automatically inferred arguments from anonymous record`` () =
    let f =
        field __ {
            name "test"
            resolve (fun _ (_: {|Name: string; Age: int|}) -> null)
        }
    getArg "Name" f
    |> argumentEqual "Name" (nonNull StringGraphType) None

    getArg "Age" f
    |> argumentEqual "Age" (nonNull IntGraphType) None

type Input = {Name: string; Age: int}

[<Test>]
let ``automatically inferred arguments from record`` () =
    let f =
        field __ {
            name "test"
            resolve (fun _ (_: Input) -> null)
        }
    getArg "Name" f
    |> argumentEqual "Name" (nonNull StringGraphType) None

    getArg "Age" f
    |> argumentEqual "Age" (nonNull IntGraphType) None


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
