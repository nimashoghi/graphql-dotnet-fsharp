module GraphQL.FSharp.UnitTests.Builder.Schema

open NUnit.Framework
open Swensen.Unquote
open GraphQL.FSharp.Builder
open GraphQL.Types

[<Test>]
let ``Schema Builder basic test`` () =
    let myQuery = ObjectGraphType<obj> ()
    let myMutation = ObjectGraphType<obj> ()
    let mySubscription = ObjectGraphType<obj> ()

    let mySchema = schema {
        query myQuery
        mutation myMutation
        subscription mySubscription
    }

    mySchema.Query =! (myQuery :> IObjectGraphType)
    mySchema.Mutation =! (myMutation :> IObjectGraphType)
    mySchema.Subscription =! (mySubscription :> IObjectGraphType)
