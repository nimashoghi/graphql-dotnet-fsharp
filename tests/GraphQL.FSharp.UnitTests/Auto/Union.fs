module GraphQL.FSharp.UnitTests.Auto.Union

open System
open NUnit.Framework
open Swensen.Unquote
open GraphQL.Types
open GraphQL.FSharp

open GraphQL.FSharp.TestUtils.Assert

// TODO: Add tests using custom attributes

[<Name "MyAttributeUnionCustom"; Description "My attribute union description"; DeprecationReason "Not deprecated">]
type MyAttributeUnion =
| [<Name "FirstCase">] First of int
| [<Name "SecondCase">] Second of fst: int * snd: float

[<Test>]
let ``Auto Union union with attributes`` () =
    Auto.Union<MyAttributeUnion>
    |> Assert.UnionGraphEqual (
        name = "MyAttributeUnionCustom",
        description = "My attribute union description",
        deprecationReason = "Not deprecated",
        cases = [
            "FirstCase", [
                "Item", nonNull IntGraphType
            ]
            "SecondCase", [
                "fst", nonNull IntGraphType
                "snd", nonNull FloatGraphType
            ]
        ]
    )

[<CLIMutable>]
type User = {
    Name: string
}

type MyUnion =
| NamedUser of User: User
| UnnamedUser of User
| UnnamedInteger of int
| NamedInteger of Name: int

[<Test>]
let ``Auto Union single member case`` () =
    let user = Auto.Object<User>

    Auto.Union<MyUnion>
    |> unionEqual "MyUnion" [
        "NamedUser", [
            "User", liftNonNull user
        ]
        "UnnamedUser", [
            "Item", liftNonNull user
        ]
        "UnnamedInteger", [
            "Item", nonNull IntGraphType
        ]
        "NamedInteger", [
            "Name", nonNull IntGraphType
        ]
    ]

type ValidUnion =
| First of Name: int
| Second of Fst: string * Snd: float

[<Test>]
let ``Auto Union valid test graph type name`` () =
    Auto.Union<ValidUnion>
    |> unionEqual "ValidUnion" [
        "First", [
            "Name", nonNull IntGraphType
        ]
        "Second", [
            "Fst", nonNull StringGraphType
            "Snd", nonNull FloatGraphType
        ]
    ]

type NonUnionType = {
    Name: string
}

[<Test>]
let ``Auto Union non union type should fail`` () =
    raises<ArgumentException> <@ Auto.Union<NonUnionType> @>
