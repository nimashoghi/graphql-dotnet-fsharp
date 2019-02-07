module GraphQL.FSharp.CommandLineSample.Model

open System.Threading.Tasks
open GraphQL.FSharp

[<Auto>]
type MyType() =
    member __.GetSomethingSync () = "hello world"
    member __.GetSomethingAsync () = Task.FromResult "hello world async"
