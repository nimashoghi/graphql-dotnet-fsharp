module GraphQL.FSharp.Tests.Auto.Enum

open System
open NUnit.Framework
open Swensen.Unquote

open GraphQL.FSharp

type ValidEnum =
| First
| Second

[<Test>]
let ``Auto Enum valid enum`` () =
    let enum = Auto.Enum<ValidEnum>
    let values = Seq.toArray enum.Values

    Array.length values =! 2

    values.[0].Name =! "First"
    values.[0].Value =! box First
    values.[1].Name =! "Second"
    values.[1].Value =! box Second

type ValidClassicalEnum =
| First = 0
| Second = 1

[<Test>]
let ``Auto Enum valid classical enum`` () =
    let enum = Auto.Enum<ValidClassicalEnum>
    let values = Seq.toArray enum.Values

    Array.length values =! 2

    values.[0].Name =! "First"
    values.[0].Value =! box ValidClassicalEnum.First
    values.[1].Name =! "Second"
    values.[1].Value =! box ValidClassicalEnum.Second

type NonEnum = {
    Name: string
}

[<Test>]
let ``Auto Enum non enum type should fail`` () =
    raises<ArgumentException> <@ Auto.Enum<NonEnum> @>
