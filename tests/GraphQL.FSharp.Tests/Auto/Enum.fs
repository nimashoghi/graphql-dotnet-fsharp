module GraphQL.FSharp.Tests.Auto.Enum

open System
open NUnit.Framework
open Swensen.Unquote
open GraphQL.FSharp

open GraphQL.FSharp.Tests.Assert

[<Name "NameChangedEnum"; Description "My enum description">]
type MyEnum =
| NotChanged
| [<Name "FirstEnum"; Description "First case">] First
| [<Name "SecondEnum"; Description "Second case">] Second

[<Test>]
let ``Auto Enum valid enum with attributes`` () =
    Auto.Enum<MyEnum>
    |> Assert.EnumGraphEqual (
        name = "NameChangedEnum",
        description = "My enum description",
        values = [
            "NotChanged", "", box NotChanged
            "FirstEnum", "First case", box First
            "SecondEnum", "Second case", box Second
        ]
    )

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

type InvalidEnum =
| First
| Second of int

[<Test>]
let ``Auto Enum invalid discriminated union`` () =
    raises<ArgumentException> <@ Auto.Enum<InvalidEnum> @>

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
