module GraphQL.FSharp.Tests.Auto.Object

open NUnit.Framework
open GraphQL.Types
open GraphQL.FSharp

open GraphQL.FSharp.Tests.Assert

type EmptyObject() = class end

[<Test>]
let ``Auto Object empty class`` () =
    Auto.Object<EmptyObject>
    |> objectEqual "EmptyObject" []

[<CLIMutable>]
type UserWithProps = {
    Name: string
    Count: int
}

[<Test>]
let ``Auto Object record with properties`` () =
    Auto.Object<UserWithProps>
    |> objectEqual "UserWithProps" [
        "Name", upcast StringGraphType ()
        "Count", upcast IntGraphType ()
    ]

[<CLIMutable>]
type UserWithPropsAndMethods =
    {
        Name: string
        Count: int
    }

    member this.MyMethod () = this.Name

[<Test>]
let ``Auto Object record with properties and methods`` () =
    Auto.Object<UserWithPropsAndMethods>
    |> objectEqual "UserWithPropsAndMethods" [
        "Name", upcast StringGraphType ()
        "Count", upcast IntGraphType ()
        "MyMethod", upcast StringGraphType ()
    ]

type IUserInterface =
    abstract member GetAbstract: unit -> int

[<CLIMutable>]
type UserWithInterface =
    {
        Name: string
        Count: int
    }

    interface IUserInterface with
        member this.GetAbstract () = this.Count

[<Test>]
let ``Auto Object record implementing an interface`` () =
    Auto.Object<UserWithInterface>
    |> objectEqual "UserWithInterface" [
        "Name", upcast StringGraphType ()
        "Count", upcast IntGraphType ()
        "GetAbstract", upcast IntGraphType ()
    ]

type RegularClassWithMethodsAndProps() =
    member __.Name = "Hello"
    member __.Count = 1

    member this.GetName () = this.Name
    member this.GetCount () = this.Count

[<Test>]
let ``Auto Object regular class with methods and props`` () =
    Auto.Object<RegularClassWithMethodsAndProps>
    |> objectEqual "RegularClassWithMethodsAndProps" [
        "Name", upcast StringGraphType ()
        "Count", upcast IntGraphType ()
        "GetName", upcast StringGraphType ()
        "GetCount", upcast IntGraphType ()
    ]

// TODO: add functionality for detecting (and tests for) for non-object types
// type NonObjectType =
// | First
// | Second
