module GraphQL.FSharp.UnitTests.Auto.InputObject

open NUnit.Framework
open GraphQL.Types
open GraphQL.FSharp

open GraphQL.FSharp.TestUtils.Assert

[<CLIMutable>]
type NullableFieldObject = {
    Name: string option
    Age: int option
    Height: float
}

[<Test>]
let ``Auto InputObject nullable fields`` () =
    Auto.InputObject<NullableFieldObject>
    |> objectEqual "NullableFieldObject" [
        "Name", nullable StringGraphType
        "Age", nullable IntGraphType
        "Height", nonNull FloatGraphType
    ]

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
            "MyCustomNameField", "My custom description field", nonNull StringGraphType
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
let ``Auto InputObject record with properties and methods`` () =
    Auto.InputObject<UserWithPropsAndMethods>
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
let ``Auto InputObject record implementing an interface`` () =
    Auto.InputObject<UserWithInterface>
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
    Auto.InputObject<RegularClassWithMethodsAndProps>
    |> objectEqual "RegularClassWithMethodsAndProps" [
        "Name", nonNull StringGraphType
        "Count", nonNull IntGraphType
        "GetName", nonNull StringGraphType
        "GetCount", nonNull IntGraphType
    ]
