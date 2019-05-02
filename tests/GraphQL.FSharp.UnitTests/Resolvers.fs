module GraphQL.FSharp.UnitTests.Resolvers

open System.Collections.Generic
open System.Threading.Tasks
open NUnit.Framework
open Swensen.Unquote
open FsCheck.NUnit
open FSharp.Utils
open FSharp.Utils.Tasks
open GraphQL.FSharp.Resolvers
open GraphQL.FSharp.Types
open Newtonsoft.Json

let (=!!) (lhs: _ Task) rhs = lhs.Result =! rhs

module ``resolve`` =
    [<Property>]
    let ``basic test`` () =
        resolveAsync(fun _ -> vtask { return "Hello World" })
            .Resolver (ResolveContext ())
        =!! box "Hello World"

    [<Property>]
    let ``basic property`` (value: obj) =
        resolveAsync(fun _ -> vtask { return value })
            .Resolver (ResolveContext ())
        =!! box value

    [<Test>]
    let ``option positive test`` () =
        resolveAsync(fun _ -> vtask { return (Some "Hello World") })
            .Resolver (ResolveContext ())
        =!! box "Hello World"

    [<Property>]
    let ``option positive property`` (value: obj) =
        resolveAsync(fun _ -> vtask { return (Some value) })
            .Resolver (ResolveContext ())
        =!! box value

    [<Test>]
    let ``option negative test`` () =
        resolveAsync(fun _ -> vtask { return (None: string option) })
            .Resolver (ResolveContext ())
        =!! null

    [<Test>]
    let ``validation result positive test`` () =
        resolveAsync(fun _ -> vtask { return (Ok "Hello World": Result<string, string list>) })
            .Resolver (ResolveContext ())
        =!! box "Hello World"

    [<Property>]
    let ``validation result positive property`` (value: obj) =
        resolveAsync(fun _ -> vtask { return (Ok value: Result<obj, string list>) })
            .Resolver (ResolveContext ())
        =!! box value

    [<Test>]
    let ``validation result negative test`` () =
        let ctx = ResolveContext ()
        resolveAsync(fun _ -> vtask { return (Error ["error"]: Result<string, string list>) })
            .Resolver ctx
        =!! null
        let errors =
            ctx.Errors
            |> Seq.toList
        List.length errors =! 1
        List.head(errors).Message =! "error"

    [<Property>]
    let ``validation result negative property`` (errors: string list) =
        let ctx = ResolveContext ()
        resolveAsync(fun _ -> vtask { return (Error errors: Result<string, string list>) })
            .Resolver ctx
        =!! null
        let ctxErr =
            ctx.Errors
            |> Seq.toList
        List.length ctxErr =! List.length errors
        (ctxErr, errors)
        ||> List.zip
        |> List.iter (fun (ctxErr, errorMsg) -> ctxErr.Message =! string errorMsg)

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
