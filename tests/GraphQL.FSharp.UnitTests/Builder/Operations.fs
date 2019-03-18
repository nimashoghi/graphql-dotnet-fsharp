module GraphQL.FSharp.UnitTests.Builder.Operations

open System.Collections.Generic
open NUnit.Framework
open Swensen.Unquote
open FSharp.Utils
open GraphQL.FSharp.Builder.Operations
open GraphQL.FSharp.Types
open GraphQL.Types

module ``active patterns`` =
    let f (i: int) = i
    let operation =
        {
            new IOperation<int> with
                member __.Invoke i = f i
                member __.Priority = 0
        }

    [<Test>]
    let ``operation active pattern`` () =
        let (Operation operation) = operation
        for i in 0 .. 100 do
            operation i =! f i

    [<Test>]
    let ``priority active pattern`` () =
        let (Priority priority) = operation
        priority =! 0

type MyType = {
    mutable Name: string
}

[<Test>]
let ``name`` () =
    let (Operation f) = name "new name"
    f {Name = "hello"} =! {Name = "new name"}

type MyTypeEx = {
    mutable DeprecationReason: string
}

[<Test>]
let ``deprecationReason`` () =
    let (Operation f) = deprecate "new deprecation reason"
    f {DeprecationReason = "hello"} =! {DeprecationReason = "new deprecation reason"}

module ``defaultValue`` =
    [<Test>]
    let ``field`` () =
        let (Operation f) = defaultValue "defaultValue"
        let field =
            Field<_, _, _> (
                GraphType = StringGraph
            )
            |> f
        field.DefaultValue =! box "defaultValue"
        field.ResolvedType =! upcast StringGraph
    [<Test>]
    let ``argument`` () =
        let (Operation f) = defaultValue "defaultValue"
        let argument =
            Argument<_> (
                GraphType = StringGraph
            )
            |> f
        argument.DefaultValue =! box "defaultValue"
        argument.ResolvedType =! upcast StringGraph

type GraphTypeTester () =
    member val GraphType: IGraphType = null with get, set

[<Test>]
let ``graphtype`` () =
    let graph =
        GraphTypeTester ()
        |> (|Operation|) (graphType StringGraph)
    graph.GraphType =! upcast StringGraph

type MetadataTester () =
    member val Metadata: IDictionary<string, obj> = null with get, set

[<Test>]
let ``metadata without existing`` () =
    let graph =
        MetadataTester ()
        |> (|Operation|) (metadata ["hello", box "hello"])
    graph.Metadata.Count =! 1
    graph.Metadata.["hello"] =! box "hello"

[<Test>]
let ``metadata with existing`` () =
    let graph =
        MetadataTester (
            Metadata = Dictionary.ofList ["first", box "first"]
        )
        |> (|Operation|) (metadata ["hello", box "hello"])
    graph.Metadata.Count =! 2
    graph.Metadata.["first"] =! box "first"
    graph.Metadata.["hello"] =! box "hello"

module ``Documentation`` =
    type DescriptionType () =
        member val Description: string = null with get, set

    [<Test>]
    let ``description`` () =
        let target =
            DescriptionType ()
            |> (|Operation|) (description "my desc")
        target.Description =! "my desc"

    [<Test>]
    let ``argumentDocumentation`` () =
        let field = Field<_, _, _> ()
        let argument =
            Argument (
                Name = "myArg"
            )
        field.Arguments <- QueryArguments argument

        field
        |> (|Operation|) (argumentDocumentation ["myArg", "my desc"])
        |> ignore

        argument.Description =! "my desc"
