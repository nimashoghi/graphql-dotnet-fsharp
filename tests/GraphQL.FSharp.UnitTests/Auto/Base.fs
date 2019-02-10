module GraphQL.FSharp.UnitTests.Auto.Base

open System
open FSharp.Reflection
open NUnit.Framework
open Swensen.Unquote

open GraphQL.FSharp
open GraphQL.FSharp.AutoBase

module ``Attribute`` =
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
    module ``shouldIgnore`` =
        type SomeOtherAttribute () = inherit Attribute ()

        type OptInType () = class end
        [<Auto>]
        type OptOutType () = class end

        [<Test>]
        let ``OptIn has ignore case`` () =
            shouldIgnore typeof<OptInType> <| seq {
                yield SomeOtherAttribute ()
            } =! true

        [<Test>]
        let ``OptIn no ignore case`` () =
            shouldIgnore typeof<OptInType> <| seq {
                yield SomeOtherAttribute ()
                yield FieldAttribute ()
            } =! false

        [<Test>]
        let ``OptOut default has ignore case`` () =
            shouldIgnore typeof<OptOutType> <| seq {
                yield SomeOtherAttribute ()
                yield IgnoreAttribute ()
            } =! true

        [<Test>]
        let ``OptOut default no ignore case`` () =
            shouldIgnore typeof<OptOutType> <| seq {
                yield SomeOtherAttribute ()
                yield SomeOtherAttribute ()
            } =! false

module ``Field`` =
    open GraphQL.FSharp.AutoBase.Field

    module ``properties`` =
        [<Auto>]
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

        [<Auto>]
        type MyRecord = {
            name: string
            age: int
        }

        [<Test>]
        let ``properties record`` () =
            properties<MyRecord>
            =! FSharpType.GetRecordFields typeof<MyRecord>
