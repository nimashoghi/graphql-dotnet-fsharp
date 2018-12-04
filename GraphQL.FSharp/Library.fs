module GraphQL.FSharp

let schema arg =
    let rec run lst =
        match lst with
        | head :: tail -> ()
        | [] -> ()
    run arg

module Test =
    open System
    open GraphQL.Types
    open GraphQL.FSharp.Builder

    let x = {
        new System.Object() with
            member __.ToString () = ""
    }

    [<CLIMutable>]
    type ItemInput = {
        Id: Guid
        Name: string
        Count: int
    }

    let inline (~%) obj = ()

    let x = % object {
        name "myObject"
        description "My Description"
        field (f {
            get (fun input -> input.Id)
        })
        field (f {
            get (fun input -> input.Name)
        })
        field (f {
            get (fun input -> input.Count)
        })
    }
