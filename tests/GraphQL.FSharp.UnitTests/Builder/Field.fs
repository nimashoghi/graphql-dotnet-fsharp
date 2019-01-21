module GraphQL.FSharp.UnitTests.Builder.Field

open System
open NUnit.Framework
open Swensen.Unquote
open GraphQL.FSharp.Builder
open GraphQL.Types

open GraphQL.FSharp.TestUtils.Assert

// TODO: Add tests for option types to check nullable fields/args

[<CLIMutable>]
type MyType = {
    Name: string
}

[<Test>]
let ``Builder Field getter invalid argument`` () =
    raises<ArgumentException>
        <@
            field {
                get (fun x -> x.Name.ToString())
            }
        @>

[<Test>]
let ``Builder Field valid getter`` () =
    field {
        get (fun x -> x.Name)
    }
    |> fieldEqual "Name" (nonNull StringGraphType)

[<CLIMutable>]
type SomeType = {
    Testing: bool
}

[<Test>]
let ``Builder Field inferred field type without default value should be non nullable`` () =
    field {
        get (fun x -> x.Testing)
    }
    |> fieldEqual "Testing" (nonNull BooleanGraphType)

[<Test>]
let ``Builder Field inferred field type with default value should be nullable`` () =
    field {
        get (fun x -> x.Testing)
        defaultValue false
    }
    |> fieldEqual "Testing" (nullable BooleanGraphType)
