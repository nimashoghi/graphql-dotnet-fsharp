module GraphQL.FSharp.UnitTests.Auto.Object

open NUnit.Framework
open GraphQL.Types
open GraphQL.FSharp

open GraphQL.FSharp.UnitTests.Assert

[<Name "MyCustomName"; Description "My custom description">]
type AttributeClass() =
    [<Name "MyCustomNameField"; Description "My custom description field">]
    member val Name = "" with get, set

[<Test>]
let ``Auto Object class with attributes`` () =
    Auto.Object<AttributeClass>
    :> IComplexGraphType
    |> Assert.ObjectGraphEqual (
        name = "MyCustomName",
        description = "My custom description",
        fields = [
            "MyCustomNameField", "My custom description field", nonNull StringGraphType
        ]
    )

[<Name "MyCustomName"; Description "My custom description">]
type IAttributeInterface =
    [<Name "MyCustomNameField"; Description "My custom description field">]
    abstract member Name: string with get, set

[<Name "MyCustomNameImpl"; Description "My custom impl description">]
type AttributeClassExtended() =
    interface IAttributeInterface with
        member val Name = "" with get, set

[<Test>]
let ``Auto Object class implementing an interface with attributes`` () =
    Auto.Object<AttributeClassExtended>
    :> IComplexGraphType
    |> Assert.ObjectGraphEqual (
        name = "MyCustomNameImpl",
        description = "My custom impl description",
        fields = [
            "MyCustomNameField", "My custom description field", nonNull StringGraphType
        ]
    )

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
        "Name", nonNull StringGraphType
        "Count", nonNull IntGraphType
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
        "Name", nonNull StringGraphType
        "Count", nonNull IntGraphType
        "MyMethod", nonNull StringGraphType
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
        "Name", nonNull StringGraphType
        "Count", nonNull IntGraphType
        "GetAbstract", nonNull IntGraphType
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
        "Name", nonNull StringGraphType
        "Count", nonNull IntGraphType
        "GetName", nonNull StringGraphType
        "GetCount", nonNull IntGraphType
    ]

// TODO: add functionality for detecting (and tests for) for non-object types
// type NonObjectType =
// | First
// | Second
