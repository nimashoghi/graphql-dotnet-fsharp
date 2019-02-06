module GraphQL.FSharp.UnitTests.Auto.Object

open System.Threading.Tasks
open NUnit.Framework
open GraphQL.FSharp
open GraphQL.Types

open GraphQL.FSharp.TestUtils.Assert

[<Auto>]
type IObjectInterface =
    abstract member Name: string
    abstract member PhoneNumber: int
    abstract member OtherPhoneNumber: unit -> float
    abstract member GetThirdPhoneNumber: AreaCode: int -> string

[<Test>]
let ``Auto Object with interface underlying type`` () =
    Auto.Object<IObjectInterface>
    |> objectEqual "IObjectInterface" [
        "Name", nonNull StringGraphType
        "PhoneNumber", nonNull IntGraphType
        "OtherPhoneNumber", nonNull FloatGraphType
        "GetThirdPhoneNumber", nonNull StringGraphType
    ]

[<Auto>]
type TaskMethodType() =
    member __.TaskMethod () = Task.FromResult "test"

[<Test>]
let ``Auto Object field of method returning task`` () =
    Auto.Object<TaskMethodType>
    |> objectEqual "TaskMethodType" [
        "TaskMethod", nonNull StringGraphType
    ]

[<Auto>]
type ContextParameterTest () =
    member __.TestContext (_ctx: ResolveFieldContext, name: int) = ""

[<Test>]
let ``Auto Object context parameter`` () =
    let object = Auto.Object<ContextParameterTest>
    object
    |> objectEqual "ContextParameterTest" [
        "TestContext", nonNull StringGraphType
    ]

    let testContext = object.Fields |> Seq.head
    Swensen.Unquote.Assertions.test
        <@
            testContext.Arguments
            |> Seq.exists (fun arg -> arg.Name = "name" && arg.ResolvedType = upcast NonNullGraphType (IntGraphType ()))
        @>

[<Auto; CLIMutable>]
type IgnoredFieldObject =
    {
        Name: string
        [<Ignore>]
        Ignored: int
    }
    member this.GetName () = this.Name
    [<Ignore>]
    member this.GetIgnored () = this.Ignored

[<Test>]
let ``Auto Object ignored fields`` () =
    Auto.Object<IgnoredFieldObject>
    |> objectEqual "IgnoredFieldObject" [
        "Name", nonNull StringGraphType
        "GetName", nonNull StringGraphType
    ]

[<Auto; CLIMutable>]
type NullableFieldObject = {
    Name: string option
    Age: int option
    Height: float
}

[<Test>]
let ``Auto Object nullable fields`` () =
    Auto.Object<NullableFieldObject>
    |> objectEqual "NullableFieldObject" [
        "Name", nullable StringGraphType
        "Age", nullable IntGraphType
        "Height", nonNull FloatGraphType
    ]

[<Auto; Name "MyCustomName"; Description "My custom description">]
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

[<Auto; Name "MyCustomName"; Description "My custom description">]
type IAttributeInterface =
    [<Name "MyCustomNameField"; Description "My custom description field">]
    abstract member Name: string with get, set

[<Auto; Name "MyCustomNameImpl"; Description "My custom impl description">]
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

[<Auto>]
type EmptyObject() = class end

[<Test>]
let ``Auto Object empty class`` () =
    Auto.Object<EmptyObject>
    |> objectEqual "EmptyObject" []

[<Auto; CLIMutable>]
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

[<Auto; CLIMutable>]
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

[<Auto; CLIMutable>]
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

[<Auto>]
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
