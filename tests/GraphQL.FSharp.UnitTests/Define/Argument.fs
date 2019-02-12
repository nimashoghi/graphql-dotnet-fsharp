module GraphQL.FSharp.UnitTests.Define

open System
open NUnit.Framework
open Swensen.Unquote
open GraphQL.FSharp
open GraphQL.Types

[<Test>]
let ``nonNull types`` () =
    let arg = Define.Argument<int> "MyArg"
    arg.ResolvedType =! upcast NonNullGraphType (IntGraphType ())

[<Test>]
let ``null option types`` () =
    let arg = Define.Argument<int option> "MyArg"
    arg.ResolvedType =! upcast IntGraphType ()

[<Test>]
let ``valid information`` () =
    let argument =
        Define.Argument (
            name = "MyArg",
            defaultValue = 1
        )
    argument.Name =! "MyArg"
    argument.DefaultValue =! box 1

[<Test>]
let ``null name`` () =
    raises<ArgumentException> <@ Define.Argument (name = null) @>

[<Test>]
let ``empty name`` () =
    raises<ArgumentException> <@ Define.Argument (name = "") @>
