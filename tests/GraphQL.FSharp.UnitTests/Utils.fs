module GraphQL.FSharp.UnitTests.Util

open System
open NUnit.Framework
open Swensen.Unquote

open GraphQL.FSharp.Utils
open GraphQL.FSharp.Utils.Attributes

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

module Attributes =
    module ``tryGetAttribute`` =
        type TestAttribute () = inherit Attribute ()
        type OtherAttribute () = inherit Attribute ()

        [<Test>]
        let ``success`` () =
            tryGetAttribute<TestAttribute> <| seq {
                yield TestAttribute ()
                yield OtherAttribute ()
            } =! Some (TestAttribute ())

        [<Test>]
        let ``fail`` () =
            tryGetAttribute<TestAttribute> <| seq {
                yield OtherAttribute ()
            } =! None
