module GraphQL.FSharp.Define.EnumCase

open NUnit.Framework
open Swensen.Unquote
open GraphQL.FSharp
open GraphQL.Types

[<AutoOpen>]
module Utils =
    let enumCaseEqual name value =
        fun (enumCase: EnumValueDefinition) ->
            enumCase.Name =! name
            enumCase.Value =! box value

[<Test>]
let ``basic test`` () =
    Define.EnumCase ("hello", 1)
    |> enumCaseEqual "hello" 1
