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
    name: string
}

[<Test>]
let ``Builder Field getter invalid argument`` () =
    raises<ArgumentException>
        <@
            field {
                get (fun x -> x.name.ToString())
            }
        @>

[<Test>]
let ``Builder Field valid getter`` () =
    field {
        get (fun x -> x.name)
    }
    |> fieldEqual "name" (nonNull StringGraphType)
