module GraphQL.FSharp.Tests.Auto.Union

open System
open FSharp.Reflection
open NUnit.Framework
open Swensen.Unquote
open GraphQL.Types
open GraphQL.FSharp

open GraphQL.FSharp.Tests.Assert

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
                "Item", graph IntGraphType
            ]
            "SecondCase", [
                "fst", graph IntGraphType
                "snd", graph FloatGraphType
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
            "User", liftGraph user
        ]
        "UnnamedUser", [
            "Item", liftGraph user
        ]
        "UnnamedInteger", [
            "Item", graph IntGraphType
        ]
        "NamedInteger", [
            "Name", graph IntGraphType
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
            "Name", graph IntGraphType
        ]
        "Second", [
            "Fst", graph StringGraphType
            "Snd", graph FloatGraphType
        ]
    ]

type NonUnionType = {
    Name: string
}

[<Test>]
let ``Auto Union non union type should fail`` () =
    raises<ArgumentException> <@ Auto.Union<NonUnionType> @>
