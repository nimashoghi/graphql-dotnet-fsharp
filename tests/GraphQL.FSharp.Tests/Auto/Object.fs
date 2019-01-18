module GraphQL.FSharp.Tests.Auto.Object

open System
open NUnit.Framework
open Swensen.Unquote
open GraphQL.Types

open GraphQL.FSharp

[<CLIMutable>]
type User = {
    Name: string
    Count: int
}

let setupObject () =
    let object = Auto.Object<User>
    let fields = Seq.toArray object.Fields

    object, fields

[<Test>]
let ``Auto Object valid object`` () =
    let object = setupObject () |> fst

    object.Name =! "User"

[<Test>]
let ``Auto Object valid object fields`` () =
    let fields = setupObject () |> snd

    Array.length fields =! 2

    fields.[0].Name =! "Name"
    fields.[0].ResolvedType =! upcast StringGraphType ()

    fields.[1].Name =! "Count"
    fields.[1].ResolvedType =! upcast IntGraphType ()

// TODO: add functionality for detecting (and tests for) for non-object types
// type NonObjectType =
// | First
// | Second
