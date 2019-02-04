module GraphQL.FSharp.UnitTests.Auto.Base

open System
open FSharp.Reflection
open NUnit.Framework
open Swensen.Unquote

module ``Attribute`` =
    open GraphQL.FSharp.AutoBase.Attribute

    module ``getMemberAttribute`` =
        type SomeAttribute () = inherit Attribute ()

        type X () =
            [<Some>]
            member __.HasAttribute i = i + 2

            member __.NoAttribute i = i + 2

        [<Test>]
        let ``has attribute test`` () =
            typeof<X>.GetMethod "HasAttribute"
            |> getMemberAttribute<SomeAttribute>
            =! Some (SomeAttribute ())

        [<Test>]
        let ``no attribute test`` () =
            typeof<X>.GetMethod "NoAttribute"
            |> getMemberAttribute<SomeAttribute>
            =! None

module ``Update`` =
    open GraphQL.FSharp.AutoBase.Update

    module ``shouldIgnore`` =
        open GraphQL.FSharp.Attributes

        type SomeOtherAttribute () = inherit Attribute ()

        [<Test>]
        let ``has ignore case`` () =
            shouldIgnore <| seq {
                yield SomeOtherAttribute ()
                yield IgnoreAttribute ()
            } =! true

        [<Test>]
        let ``no ignore case`` () =
            shouldIgnore <| seq {
                yield SomeOtherAttribute ()
                yield SomeOtherAttribute ()
            } =! false

    module ``tryGetAttribute`` =
        type TestAttribute () = inherit Attribute ()
        type OtherAttribute () = inherit Attribute ()

        [<Test>]
        let ``success`` () =
            tryGetAttribute<TestAttribute> <| seq {
                yield TestAttribute ()
                yield OtherAttribute ()
            } =! (Some (TestAttribute ()))

        [<Test>]
        let ``fail`` () =
            tryGetAttribute<TestAttribute> <| seq {
                yield OtherAttribute ()
            } =! None

module ``Field`` =
    open GraphQL.FSharp.AutoBase.Field

    module ``properties`` =
        type MyType () =
            member __.MyProp = 2

        [<Test>]
        let ``validProp success`` () =
            typeof<MyType>.GetProperty "MyProp"
            |> validProp
            =! true

        [<Test>]
        let ``properties success`` () =
            properties<MyType>
            =! [|typeof<MyType>.GetProperty "MyProp"|]

        type MyRecord = {
            name: string
            age: int
        }

        [<Test>]
        let ``properties record`` () =
            properties<MyRecord>
            =! FSharpType.GetRecordFields typeof<MyRecord>


    // module ``setInfo`` =
    //     type MyType = {
    //         Age: int
    //         mutable Name: string
    //     }

    //     [<Test>]
    //     let ``success`` () =
    //         let myTypeInstance = {Age = 12; Name = "test"}
    //         {Age = 0; Name = "original"}
    //         |> setInfo myTypeInstance
    //         =! {Age = 0; Name = "test"}
