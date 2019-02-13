module GraphQL.FSharp.UnitTests.Auto.Union

open System
open NUnit.Framework
open Swensen.Unquote
open GraphQL.Types
open GraphQL.FSharp

open GraphQL.FSharp.TestUtils.Assert

module ``addTag`` =
    open GraphQL.FSharp.AutoUnion

    [<Test>]
    let ``basic test`` () =
        ObjectGraphType<obj> (
            Name = "myObj"
        )
        |> addTag 0
        |> objectEqual "myObj" ["Tag", nonNull IntGraphType]

[<Name "MyAttributeUnionCustom"; Description "My attribute union description"; DeprecationReason "Not deprecated">]
type MyAttributeUnion =
| [<Name "FirstCase">] First of int
| [<Name "SecondCase">] Second of fst: int * snd: float

[<Test>]
let ``Auto Union union with attributes`` () =
    Auto.Union<MyAttributeUnion>
    :> UnionGraphType
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
        "MyUnionNamedUser", [
            "User", liftNonNull user
        ]
        "MyUnionUnnamedUser", [
            "Item", liftNonNull user
        ]
        "MyUnionUnnamedInteger", [
            "Item", nonNull IntGraphType
        ]
        "MyUnionNamedInteger", [
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
        "ValidUnionFirst", [
            "Name", nonNull IntGraphType
        ]
        "ValidUnionSecond", [
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
