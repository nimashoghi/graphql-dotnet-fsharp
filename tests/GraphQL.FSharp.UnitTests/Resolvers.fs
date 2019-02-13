module GraphQL.FSharp.UnitTests.Resolvers

open System.Threading.Tasks
open NUnit.Framework
open Swensen.Unquote
open GraphQL.FSharp.Resolvers
open GraphQL
open GraphQL.Types

module ``Handlers`` =
    module ``optionValue`` =
        [<Test>]
        let ``some`` () =
            Some "hello"
            |> box
            |> optionValue
            =! Some (box "hello")

        [<Test>]
        let ``none`` () =
            None
            |> box
            |> optionValue
            =! None

    module ``resultValue`` =
        [<Test>]
        let ``ok`` () =
            Ok "hello"
            |> box
            |> resultValue
            =! Ok (box "hello")

        [<Test>]
        let ``error`` () =
            Error "some error"
            |> box
            |> resultValue
            =! Error (box "some error")

module ``handleObject`` =
    [<Test>]
    let ``normal object`` () =
        let ctx =
            ResolveFieldContext (
                Errors = ExecutionErrors ()
            )
        let input = "hello"
        handleObject ctx input =! box input
        ctx.Errors.Count =! 0

    [<Test>]
    let ``option object some`` () =
        let ctx =
            ResolveFieldContext (
                Errors = ExecutionErrors ()
            )
        let input = "hello"
        handleObject ctx (Some input) =! box input
        ctx.Errors.Count =! 0

    [<Test>]
    let ``option object none`` () =
        let ctx =
            ResolveFieldContext (
                Errors = ExecutionErrors ()
            )
        handleObject ctx None =! null
        ctx.Errors.Count =! 0

    [<Test>]
    let ``result object success`` () =
        let ctx =
            ResolveFieldContext (
                Errors = ExecutionErrors ()
            )
        let input = "hello"
        handleObject ctx (Ok input) =! box input
        ctx.Errors.Count =! 0

    [<Test>]
    let ``result object failure`` () =
        let ctx =
            ResolveFieldContext (
                Errors = ExecutionErrors ()
            )
        handleObject ctx (Error "something went wrong") =! null
        ctx.Errors.Count =! 1
        ctx.Errors.[0].Message =! "something went wrong"

module ``taskMap`` =
    [<Test>]
    let ``basic test`` () =
        (
            Task.FromResult 1
            |> taskMap (fun x -> x + 2)
        ).Result =! 3

    [<Test>]
    let ``different type test`` () =
        (
            Task.FromResult 1
            |> taskMap (fun x -> sprintf "%i" <| x + 2)
        ).Result =! "3"
