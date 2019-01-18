module GraphQL.FSharp.Tests.Auto.InputObject

open NUnit.Framework
open GraphQL.Types
open GraphQL.FSharp

open GraphQL.FSharp.Tests.Assert

type EmptyObject() = class end

[<Test>]
let ``Auto InputObject empty class`` () =
    Auto.InputObject<EmptyObject>
    |> objectEqual "EmptyObject" []

[<CLIMutable>]
type UserWithProps = {
    Name: string
    Count: int
}

[<Test>]
let ``Auto InputObject record with properties`` () =
    Auto.InputObject<UserWithProps>
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
let ``Auto InputObject record with properties and methods`` () =
    Auto.InputObject<UserWithPropsAndMethods>
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
let ``Auto InputObject record implementing an interface`` () =
    Auto.InputObject<UserWithInterface>
    |> objectEqual "UserWithInterface" [
        "Name", upcast StringGraphType ()
        "Count", upcast IntGraphType ()
        "GetAbstract", upcast IntGraphType ()
    ]
