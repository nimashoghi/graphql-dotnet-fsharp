module GraphQL.FSharp.UnitTests.Util

open NUnit.Framework
open Swensen.Unquote

open GraphQL.FSharp.Utils

[<Test>]
let ``Regex active patten phone test`` () =
    let value = (|Regex|_|) @"\(([0-9]{3})\)[-. ]?([0-9]{3})[-. ]?([0-9]{4})" "(555) 444-2222"
    value =! Some ["555"; "444"; "2222"]

module Option =
    [<Test>]
    let ``ofBox valid string`` () =
        Option.ofBox "hello world" =! Some "hello world"

    [<Test>]
    let ``ofBox null string`` () =
        Option.ofBox (null: string) =! None

    [<Test>]
    let ``or test with None input`` () =
        Option.``or`` "test" None =! "test"

    [<Test>]
    let ``or test with Some input`` () =
        Option.``or`` "test" (Some "input") =! "input"

module Seq =
    [<Test>]
    let ``some`` () =
        seq { yield None; yield Some 1 }
        |> Seq.some
        |> Seq.toList
        =! [1]

module List =
    [<Test>]
    let ``some`` () =
        [None; Some 1]
        |> List.some
        =! [1]

module Array =
    [<Test>]
    let ``some`` () =
        [|None; Some 1|]
        |> Array.some
        =! [|1|]
