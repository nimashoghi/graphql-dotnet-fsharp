module GraphQL.FSharp.UnitTests.Resolvers

open System.Collections.Generic
open System.Threading.Tasks
open NUnit.Framework
open Swensen.Unquote
open FsCheck.NUnit
open FSharp.Utils
open GraphQL.FSharp.Resolvers
open GraphQL
open GraphQL.FSharp.Types
open Newtonsoft.Json

let (=!!) (lhs: _ Task) rhs = lhs.Result =! rhs

module ``resolve`` =
    [<Property>]
    let ``basic test`` () =
        resolveAsync(fun _ -> Task.FromResult "Hello World")
            .Resolver (ResolveContext ())
        =!! box "Hello World"

    [<Property>]
    let ``basic property`` (value: obj) =
        resolveAsync(fun _ -> Task.FromResult value)
            .Resolver (ResolveContext ())
        =!! box value

    [<Test>]
    let ``option positive test`` () =
        resolveAsync(fun _ -> Task.FromResult (Some "Hello World"))
            .Resolver (ResolveContext ())
        =!! box "Hello World"

    [<Property>]
    let ``option positive property`` (value: obj) =
        resolveAsync(fun _ -> Task.FromResult (Some value))
            .Resolver (ResolveContext ())
        =!! box value

    [<Test>]
    let ``option negative test`` () =
        resolveAsync(fun _ -> Task.FromResult (None: string option))
            .Resolver (ResolveContext ())
        =!! box null

    [<Test>]
    let ``validation result positive test`` () =
        resolveAsync(fun _ -> Task.FromResult (Ok "Hello World": Result<string, string list>))
            .Resolver (ResolveContext ())
        =!! box "Hello World"

    [<Property>]
    let ``validation result positive property`` (value: obj) =
        resolveAsync(fun _ -> Task.FromResult (Ok value: Result<obj, string list>))
            .Resolver (ResolveContext ())
        =!! box value

    [<Test>]
    let ``validation result negative test`` () =
        let ctx = ResolveContext ()
        resolveAsync(fun _ -> Task.FromResult (Error ["error"]: Result<string, string list>))
            .Resolver ctx
        =!! box null
        let errors =
            ctx.Errors
            |> Seq.toList
        List.length errors =! 1
        List.head(errors).Message =! "error"

    [<Property>]
    let ``validation result negative property`` (errors: string list) =
        let ctx = ResolveContext ()
        resolveAsync(fun _ -> Task.FromResult (Error errors: Result<string, string list>))
            .Resolver ctx
        =!! box null
        let ctxErr =
            ctx.Errors
            |> Seq.toList
        List.length ctxErr =! List.length errors
        (ctxErr, errors)
        ||> List.zip
        |> List.iter (fun (ctxErr, errorMsg) -> ctxErr.Message =! string errorMsg)

    [<Test>]
    let ``result positive test`` () =
        resolveAsync(fun _ -> Task.FromResult (Ok "Hello World": Result<string, string>))
            .Resolver (ResolveContext ())
        =!! box "Hello World"

    [<Test>]
    let ``result negative test`` () =
        let ctx = ResolveContext ()
        resolveAsync(fun _ -> Task.FromResult (Error "error": Result<string, string>))
            .Resolver ctx
        =!! box null
        let errors =
            ctx.Errors
            |> Seq.toList
        List.length errors =! 1
        List.head(errors).Message =! "error"

// module ``resolveHandler`` =
//     [<Test>]
//     let ``basic test`` () =
//         let resolver = resolveHandler (fun _ -> sprintf "%s - added" >> box) (fun _ -> "Hello world")
//         resolver.Resolver (ResolveContext ()) =! box "Hello world - added"

module ``resolveTaskHandler`` =
    [<Test>]
    let ``basic test`` () =
        let resolver = resolveTaskHandler (fun _ -> sprintf "%s - added" >> box) (fun _ -> Task.FromResult "Hello world")
        resolver.Resolver(ResolveContext ()).Result
        =! box "Hello world - added"

let dictEquals (lhs: IDictionary<'key, 'value>) (rhs: IDictionary<'key, 'value>) =
    JsonConvert.SerializeObject lhs =! JsonConvert.SerializeObject rhs

module ``handleError`` =
    type Error = {
        Code: int
    }
    [<Test>]
    let ``dict test`` () =
        let error = {Code = 1}
        let actual = handleError error

        actual.Message =! "Error"

        actual.DataAsDictionary
        |> dictEquals (Dictionary.ofList ["Code", box 1])

    [<Test>]
    let ``string test`` () =
        let error = "Something went wrong"
        let actual = handleError error
        actual.Message =! error

module ``handleObject`` =
    [<Test>]
    let ``normal object`` () =
        let ctx =
            ResolveContext<obj> (
                Errors = ExecutionErrors ()
            )
        let input = "hello"
        handleObject ctx input =! box input
        ctx.Errors.Count =! 0

    [<Test>]
    let ``option object some`` () =
        let ctx =
            ResolveContext<obj> (
                Errors = ExecutionErrors ()
            )
        let input = "hello"
        handleObject ctx (Some input) =! box input
        ctx.Errors.Count =! 0

    [<Test>]
    let ``option object none`` () =
        let ctx =
            ResolveContext<obj> (
                Errors = ExecutionErrors ()
            )
        handleObject ctx None =! null
        ctx.Errors.Count =! 0

    [<Test>]
    let ``result object success`` () =
        let ctx =
            ResolveContext<obj> (
                Errors = ExecutionErrors ()
            )
        let input = "hello"
        handleObject ctx (Ok input) =! box input
        ctx.Errors.Count =! 0

    [<Test>]
    let ``result object failure`` () =
        let ctx =
            ResolveContext<obj> (
                Errors = ExecutionErrors ()
            )
        handleObject ctx (Error "something went wrong") =! null
        ctx.Errors.Count =! 1
        ctx.Errors.[0].Message =! "something went wrong"
