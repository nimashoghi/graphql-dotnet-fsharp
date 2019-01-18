module GraphQL.FSharp.Tests.Auto.Union

open System
open NUnit.Framework
open Swensen.Unquote
open GraphQL.Types

open GraphQL.FSharp

// TODO: Add tests using custom attributes

type ValidUnion =
| First of Name: int
| Second of Fst: string * Snd: float

let validSetup () =
    let union = Auto.Union<ValidUnion>
    let possibleTypes = Seq.toArray union.PossibleTypes

    union, possibleTypes

[<Test>]
let ``Auto Union valid test graph type name`` () =
    let union = validSetup () |> fst
    union.Name =! "ValidUnion"

[<Test>]
let ``Auto Union valid test cases`` () =
    let possibleTypes = validSetup () |> snd

    Array.length possibleTypes =! 2

    possibleTypes.[0].Name =! "First"
    possibleTypes.[1].Name =! "Second"

[<Test>]
let ``Auto Union valid test first case fields`` () =
    let possibleTypes = validSetup () |> snd

    let firstCaseFields = Seq.toArray possibleTypes.[0].Fields
    Array.length firstCaseFields =! 1
    firstCaseFields.[0].Name =! "Name"
    firstCaseFields.[0].ResolvedType =! upcast IntGraphType()

[<Test>]
let ``Auto Union valid test second case fields`` () =
    let possibleTypes = validSetup () |> snd

    let secondCaseFields = Seq.toArray possibleTypes.[1].Fields
    Array.length secondCaseFields =! 2
    secondCaseFields.[0].Name =! "Fst"
    secondCaseFields.[0].ResolvedType =! upcast StringGraphType()

    secondCaseFields.[1].Name =! "Snd"
    secondCaseFields.[1].ResolvedType =! upcast FloatGraphType()

type NonUnionType = {
    Name: string
}

[<Test>]
let ``Auto Union non union type should fail`` () =
    raises<ArgumentException> <@ Auto.Union<NonUnionType> @>
