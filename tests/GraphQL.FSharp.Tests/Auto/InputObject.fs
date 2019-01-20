module GraphQL.FSharp.Tests.Auto.InputObject

open NUnit.Framework
open GraphQL.Types
open GraphQL.FSharp

open GraphQL.FSharp.Tests.Assert

[<Name "MyCustomName"; Description "My custom description">]
type AttributeClass() =
    [<Name "MyCustomNameField"; Description "My custom description field">]
    member val Name = "" with get, set

[<Test>]
let ``Auto InputObject class with attributes`` () =
    Auto.InputObject<AttributeClass>
    :> IComplexGraphType
    |> Assert.ObjectGraphEqual (
        name = "MyCustomName",
        description = "My custom description",
        fields = [
            "MyCustomNameField", "My custom description field", graph StringGraphType
        ]
    )

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
        "Name", graph StringGraphType
        "Count", graph IntGraphType
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
        "Name", graph StringGraphType
        "Count", graph IntGraphType
        "MyMethod", graph StringGraphType
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
        "Name", graph StringGraphType
        "Count", graph IntGraphType
        "GetAbstract", graph IntGraphType
    ]

type RegularClassWithMethodsAndProps() =
    member __.Name = "Hello"
    member __.Count = 1

    member this.GetName () = this.Name
    member this.GetCount () = this.Count

[<Test>]
let ``Auto Object regular class with methods and props`` () =
    Auto.InputObject<RegularClassWithMethodsAndProps>
    |> objectEqual "RegularClassWithMethodsAndProps" [
        "Name", graph StringGraphType
        "Count", graph IntGraphType
        "GetName", graph StringGraphType
        "GetCount", graph IntGraphType
    ]
