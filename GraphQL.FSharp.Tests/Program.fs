module GraphQL.FSharp.Tests.Program

open System
open GraphQL.FSharp
open GraphQL.FSharp.Builder
open GraphQL.Types

[<CLIMutable>]
type ItemInput = {
    Id: Guid
    Name: string
    Count: int
}

let item _ = object {
    name "My Object"
    description "My Object"
    field (f {
        get (fun i -> i.Id)
    })
}

let g () = inject item

let add = schema [
    inject item
]

type MySchema() =
    inherit Schema()

let s = new MySchema()
let provider =
    {
        new IServiceProvider with
            member __.GetService x = box s
    }

let ``test FindType`` () =
    assert (s.FindType "My Object" <> null)

[<EntryPoint>]
let main argv =
    add provider
    ``test FindType`` ()
    0
