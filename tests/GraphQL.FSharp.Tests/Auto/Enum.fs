module GraphQL.FSharp.Tests.Auto.Enum

open System
open NUnit.Framework
open Swensen.Unquote
open GraphQL.FSharp

open GraphQL.FSharp.Tests.Assert

type ValidEnum =
| First
| Second

[<Test>]
let ``Auto Enum valid enum`` () =
    Auto.Enum<ValidEnum>
    |> enumEqual "ValidEnum" [
        "First", box First
        "Second", box Second
    ]

type ValidClassicalEnum =
| First = 0
| Second = 1

[<Test>]
let ``Auto Enum valid classical enum`` () =
    Auto.Enum<ValidClassicalEnum>
    |> enumEqual "ValidClassicalEnum" [
        "First", box ValidClassicalEnum.First
        "Second", box ValidClassicalEnum.Second
    ]

type NonEnum = {
    Name: string
}

[<Test>]
let ``Auto Enum non enum type should fail`` () =
    raises<ArgumentException> <@ Auto.Enum<NonEnum> @>
