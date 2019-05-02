module GraphQL.FSharp.CommandLineSample.Model

open System.Threading.Tasks

type MyType() =
    member __.GetSomethingSync () = "hello world"
    member __.GetSomethingAsync () = Task.FromResult "hello world async"

type MyUnion =
| First of name: string
| Second of age: int
