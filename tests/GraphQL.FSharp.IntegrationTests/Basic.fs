module GraphQL.FSharp.IntegrationTests.Basic

open System
open NUnit.Framework
open GraphQL.FSharp
open GraphQL.FSharp.Builder

open GraphQL.FSharp.IntegrationTests.Utils

[<AutoOpen>]
module GraphTypes =
    type IMyInterface =
        abstract member SomeName: string
        abstract member GetSomeOtherName: name: string -> string

    type MyImplementation() =
        member val SomeOtherName = ""
        member __.GetSomeThirdName name = sprintf "sup third %s" name

        interface IMyInterface with
            member val SomeName = ""
            member __.GetSomeOtherName name = sprintf "sup %s" name

    type MyImplementationNum2() =
        member val SomeThirdName = ""

        interface IMyInterface with
            member val SomeName = ""
            member __.GetSomeOtherName name = sprintf "hello %s" name

    let myInterface = Auto.Interface<IMyInterface>
    let myImplementation = Auto.Object<MyImplementation>
    let myImplementationNum2 = Auto.Object<MyImplementationNum2>

    [<CLIMutable>]
    type MyType = {
        name: string
    }

    let myInferredType = Auto.Object<MyType>
    myInferredType.Name <- "inferredMyType"

    let myType = object {
        name "myTypeManual"
        fields [
            field {
                get (fun x -> x.name)
            }
        ]
    }

    let myInferredInputType = Auto.InputObject<MyType>
    myInferredInputType.Name <- "myInferredInputType"

    let myEnum = enum {
        name "myEnum"
        cases [
            "MyFirstCase", "MyFirstCase"
        ]
    }

    type MySecondEnum =
    | First
    | Second

    let mySecondEnum = Auto.Enum<MySecondEnum>

    type MyUnion =
    | FirstUnion of Name: string * Age: int
    | SecondUnion of Something: float * Id: System.Guid

    let myAutoUnion = Auto.Union<MyUnion>

    let firstGuid = Guid.Parse "5c8809cb-8816-4f2c-b187-08ddf664783a"
    let secondGuid = Guid.Parse "01ff3840-abad-4fd1-a68e-ad021d4c6868"
    let thirdGuid = Guid.Parse "eca96a5e-f2ec-413f-bfe8-9e05c248e148"

    let myQuery = query {
        fields [
            field {
                name "getMyType"
                resolve (fun _ -> {name = "sup"})
            }
            field {
                name "myQuery"
                resolve (fun ctx -> ctx.GetArgument<int> "myArg" :: [1; 2; 3; 4; 5])
                args [
                    Define.Argument<int> ("myArg", 1)
                ]
            }
            fieldOf myEnum {
                name "myEnum"
                resolve (fun _ -> "MyFirstCase")
                ofType myEnum
            }
            fieldOf mySecondEnum {
                name "mySecondEnum"
                resolve (fun _ -> First)
            }
            field {
                name "withInput"
                args [
                    Define.Argument<MyType> "myArg"
                ]
                resolve (fun ctx -> ctx.GetArgument<MyType> "myArg")
            }
            field {
                name "myAutoUnion"
                resolve (fun _ -> FirstUnion ("sup", 12))
            }
            field {
                name "myAutoUnionList"
                resolve (fun _ -> [
                    FirstUnion ("sup", 12)
                    FirstUnion ("dfsjiosh", 122)
                    SecondUnion (1.2, firstGuid)
                    SecondUnion (1.5, secondGuid)
                    SecondUnion (1.3, thirdGuid)
                ])
            }
            field {
                name "myImpl"
                resolve (fun _ -> MyImplementation())
            }
            field {
                name "myImplList"
                resolve (fun _ -> [
                    MyImplementation() :> IMyInterface
                    MyImplementationNum2() :> IMyInterface
                    MyImplementation() :> IMyInterface
                ])
            }
            field {
                name "myInterface"
                resolve (fun _ -> MyImplementation() :> IMyInterface)
            }
        ]
    }

    let mySchema = schema {
        query myQuery
    }

[<Literal>]
let Query = """
query {
  getMyType {
    name
    __typename
  }
  myEnum
  mySecondEnum
  withInput(myArg: { name: "sup bitch" }) {
    name
    __typename
  }
  myImpl {
    __typename
    someName
    someOtherName
    getSomeOtherName(name: "shhoisd")
    getSomeThirdName(name: "yolo")
  }
  myInterface {
    __typename
    someName
    getSomeOtherName(name: "itme")
    ... on MyImplementation {
      someOtherName
      getSomeThirdName(name: "yolo")
    }
  }
  myImplList {
    __typename
    someName
    getSomeOtherName(name: "itme")
    ... on MyImplementation {
      someOtherName
      getSomeThirdName(name: "yolo")
    }
    ... on MyImplementationNum2 {
      someThirdName
    }
  }
  myAutoUnion {
    __typename
    ... on FirstUnion {
      name
      age
    }
  }
  myAutoUnionList {
    __typename
    ... on FirstUnion {
      name
      age
    }
    ... on SecondUnion {
      something
      id
    }
  }
}
"""

[<Literal>]
let ExpectedResult = """
{
  "data": {
    "getMyType": {
      "name": "sup",
      "__typename": "inferredMyType"
    },
    "myEnum": "MyFirstCase",
    "mySecondEnum": "First",
    "withInput": {
      "name": "sup bitch",
      "__typename": "inferredMyType"
    },
    "myImpl": {
      "__typename": "MyImplementation",
      "someName": "",
      "someOtherName": "",
      "getSomeOtherName": "sup shhoisd",
      "getSomeThirdName": "sup third yolo"
    },
    "myInterface": {
      "__typename": "MyImplementation",
      "someName": "",
      "getSomeOtherName": "sup itme",
      "someOtherName": "",
      "getSomeThirdName": "sup third yolo"
    },
    "myImplList": [
      {
        "__typename": "MyImplementation",
        "someName": "",
        "getSomeOtherName": "sup itme",
        "someOtherName": "",
        "getSomeThirdName": "sup third yolo"
      },
      {
        "__typename": "MyImplementationNum2",
        "someName": "",
        "getSomeOtherName": "hello itme",
        "someThirdName": ""
      },
      {
        "__typename": "MyImplementation",
        "someName": "",
        "getSomeOtherName": "sup itme",
        "someOtherName": "",
        "getSomeThirdName": "sup third yolo"
      }
    ],
    "myAutoUnion": {
      "__typename": "FirstUnion",
      "name": "sup",
      "age": 12
    },
    "myAutoUnionList": [
      {
        "__typename": "FirstUnion",
        "name": "sup",
        "age": 12
      },
      {
        "__typename": "FirstUnion",
        "name": "dfsjiosh",
        "age": 122
      },
      {
        "__typename": "SecondUnion",
        "something": 1.2,
        "id": "5c8809cb-8816-4f2c-b187-08ddf664783a"
      },
      {
        "__typename": "SecondUnion",
        "something": 1.5,
        "id": "01ff3840-abad-4fd1-a68e-ad021d4c6868"
      },
      {
        "__typename": "SecondUnion",
        "something": 1.3,
        "id": "eca96a5e-f2ec-413f-bfe8-9e05c248e148"
      }
    ]
  }
}
"""

[<Test>]
let ``Complex query result`` () =
    queryEqual Query ExpectedResult mySchema
