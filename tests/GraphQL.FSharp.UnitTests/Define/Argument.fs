module GraphQL.FSharp.UnitTests.Define

open System
open NUnit.Framework
open Swensen.Unquote

open GraphQL.FSharp

[<Test>]
let ``Define Argument valid information`` () =
    let argument =
        Define.Argument (
            name = "MyArg",
            defaultValue = 1
        )
    argument.Name =! "MyArg"
    argument.DefaultValue =! box 1

[<Test>]
let ``Define Argument null name`` () =
    raises<ArgumentException> <@ Define.Argument (name = null) @>

[<Test>]
let ``Define Argument empty name`` () =
    raises<ArgumentException> <@ Define.Argument (name = "") @>
