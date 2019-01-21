module GraphQL.FSharp.UnitTests.Auto.Interface

open NUnit.Framework
open Swensen.Unquote
open GraphQL
open GraphQL.FSharp
open GraphQL.FSharp.Builder
open GraphQL.Types

open GraphQL.FSharp.TestUtils.Assert

type IInterfaceWithNullableMembers =
    abstract member Name: string option with get, set
    abstract member GetInteger: unit -> int option

[<Test>]
let ``Auto Interface with nullable members`` () =
    Auto.Interface<IInterfaceWithNullableMembers>
    |> objectEqual "IInterfaceWithNullableMembers" [
        "Name", nullable StringGraphType
        "GetInteger", nullable IntGraphType
    ]

[<Name "MyCustomName"; Description "My custom description">]
type IAttributeInterface =
    [<Name "MyCustomNameField"; Description "My custom description field">]
    abstract member Name: string with get, set

[<Test>]
let ``Auto Interface interface with attributes`` () =
    Auto.Interface<IAttributeInterface>
    :> IComplexGraphType
    |> Assert.ObjectGraphEqual (
        name = "MyCustomName",
        description = "My custom description",
        fields = [
            "MyCustomNameField", "My custom description field", nonNull StringGraphType
        ]
    )

type IEmptyInterface = interface end

[<Test>]
let ``Auto Interface empty class`` () =
    Auto.Interface<IEmptyInterface>
    |> objectEqual "IEmptyInterface" []

type IMethodPropInterface =
    abstract member Name: string
    abstract member GetCount: unit -> int

[<Test>]
let ``Auto Interface interface with methods and properties`` () =
    Auto.Interface<IMethodPropInterface>
    |> objectEqual "IMethodPropInterface" [
        "Name", nonNull StringGraphType
        "GetCount", nonNull IntGraphType
    ]

type INestedInterfaceWithMethodPropInterface =
    inherit IMethodPropInterface

    abstract member GetSecondName: unit -> string
    abstract member SecondCount: int

[<Test>]
let ``Auto Interface nested interface with methods and properties`` () =
    Auto.Interface<INestedInterfaceWithMethodPropInterface>
    |> objectEqual "INestedInterfaceWithMethodPropInterface" [
        "Name", nonNull StringGraphType
        "GetCount", nonNull IntGraphType
        "GetSecondName", nonNull StringGraphType
        "SecondCount", nonNull IntGraphType
    ]

type MethodPropInterface() =
    interface IMethodPropInterface with
        member __.Name = "sup"
        member __.GetCount () = 232

[<Test>]
let ``Auto Interface interface implementation object`` () =
    let ``interface`` = Auto.Interface<IMethodPropInterface>
    let object = Auto.Object<MethodPropInterface>

    object
    |> objectEqual "MethodPropInterface" [
        "Name", nonNull StringGraphType
        "GetCount", nonNull IntGraphType
    ]

    test
        <@
            object.ResolvedInterfaces
            |> Seq.exists ((=) (upcast ``interface`` ))
        @>

[<Test>]
let ``Auto Interface implementation results`` () =
    Auto.Interface<IMethodPropInterface> |> ignore
    Auto.Object<MethodPropInterface> |> ignore


    let Query = """
        query {
            getMethod {
                name,
                getCount
            }
        }
    """

    let Expected = """
        {
            "data": {
                "getMethod": {
                    "name": "sup",
                    "getCount": 232
                }
            }
        }
    """

    let myQuery =
        query {
            fields [
                field {
                    name "getMethod"
                    resolve (fun _ -> MethodPropInterface () :> IMethodPropInterface)
                }
            ]
        }
    let mySchema =
        schema {
            query myQuery
        }

    mySchema
    |> queryEqual Query Expected
