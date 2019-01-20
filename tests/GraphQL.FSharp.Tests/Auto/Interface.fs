module GraphQL.FSharp.Tests.Auto.Interface

open NUnit.Framework
open Swensen.Unquote
open GraphQL.Types
open GraphQL.FSharp

open GraphQL.FSharp.Tests.Assert

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
        "Name", graph StringGraphType
        "GetCount", graph IntGraphType
    ]

type INestedInterfaceWithMethodPropInterface =
    inherit IMethodPropInterface

    abstract member GetSecondName: unit -> string
    abstract member SecondCount: int

[<Test>]
let ``Auto Interface nested interface with methods and properties`` () =
    Auto.Interface<INestedInterfaceWithMethodPropInterface>
    |> objectEqual "INestedInterfaceWithMethodPropInterface" [
        "Name", graph StringGraphType
        "GetCount", graph IntGraphType
        "GetSecondName", graph StringGraphType
        "SecondCount", graph IntGraphType
    ]

type MethodPropInterface() =
    interface IMethodPropInterface with
        member __.Name = "sup"
        member __.GetCount () = 0

[<Test>]
let ``Auto Interface interface implementation object`` () =
    let ``interface`` = Auto.Interface<IMethodPropInterface> :> IInterfaceGraphType

    let object = Auto.Object<MethodPropInterface>

    object
    |> objectEqual "MethodPropInterface" [
        "Name", graph StringGraphType
        "GetCount", graph IntGraphType
    ]

    test
        <@
            object.ResolvedInterfaces
            |> Seq.exists ((=) ``interface``)
        @>
