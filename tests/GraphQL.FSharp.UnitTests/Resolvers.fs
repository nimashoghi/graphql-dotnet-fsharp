module GraphQL.FSharp.UnitTests.Resolvers

open NUnit.Framework
open Swensen.Unquote
open GraphQL.FSharp.Resolvers
open GraphQL
open GraphQL.FSharp.Types

module ``handleObject`` =
    [<Test>]
    let ``normal object`` () =
        let ctx =
            ResolveContext<obj>(
                Errors = ExecutionErrors ()
            )
        let input = "hello"
        handleObject ctx input =! box input
        ctx.Errors.Count =! 0

    [<Test>]
    let ``option object some`` () =
        let ctx =
            ResolveContext<obj>(
                Errors = ExecutionErrors ()
            )
        let input = "hello"
        handleObject ctx (Some input) =! box input
        ctx.Errors.Count =! 0

    [<Test>]
    let ``option object none`` () =
        let ctx =
            ResolveContext<obj>(
                Errors = ExecutionErrors ()
            )
        handleObject ctx None =! null
        ctx.Errors.Count =! 0

    [<Test>]
    let ``result object success`` () =
        let ctx =
            ResolveContext<obj>(
                Errors = ExecutionErrors ()
            )
        let input = "hello"
        handleObject ctx (Ok input) =! box input
        ctx.Errors.Count =! 0

    [<Test>]
    let ``result object failure`` () =
        let ctx =
            ResolveContext<obj>(
                Errors = ExecutionErrors ()
            )
        handleObject ctx (Error "something went wrong") =! null
        ctx.Errors.Count =! 1
        ctx.Errors.[0].Message =! "something went wrong"
