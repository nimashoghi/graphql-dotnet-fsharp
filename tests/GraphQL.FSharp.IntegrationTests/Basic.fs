module GraphQL.FSharp.IntegrationTests.Basic

open System
open NUnit.Framework
open GraphQL.FSharp
open GraphQL.FSharp.Builder
open GraphQL.Types

open GraphQL.FSharp.TestUtils.Assert

[<AutoOpen>]
module GraphTypes =
    [<Auto>]
    type IMyInterface =
        abstract member SomeName: string
        abstract member GetSomeOtherName: name: string -> string

    [<Auto>]
    type MyImplementation() =
        member val SomeOtherName = ""
        member __.GetSomeThirdName name = sprintf "sup third %s" name

        interface IMyInterface with
            member val SomeName = ""
            member __.GetSomeOtherName name = sprintf "sup %s" name

    [<Auto>]
    type MyImplementationNum2() =
        member val SomeThirdName = ""

        interface IMyInterface with
            member val SomeName = ""
            member __.GetSomeOtherName name = sprintf "hello %s" name

    let IMyInterfaceGraph = Auto.Interface<IMyInterface>
    let MyImplementationGraph = Auto.Object<MyImplementation>
    let MyImplementationNum2Graph = Auto.Object<MyImplementationNum2>

    [<Auto; CLIMutable>]
    type MyTypeInput = {
        name: string
    }

    [<Auto; CLIMutable>]
    type MyType = {
        name: string
    }

    let MyTypeGraph = Auto.Object<MyType>

    let MyTypeManualGraph = object {
        name "myTypeManual"
        fields [
            field {
                get (fun x -> x.name)
            }
        ]
    }

    let MyTypeInputGraph = Auto.InputObject<MyTypeInput>

    let MyEnumGraph = enum {
        name "myEnum"
        cases [
            Define.EnumCase ("MyFirstCase", "MyFirstCase")
        ]
    }

    type MySecondEnum =
    | First
    | Second

    let MySecondEnumGraph = Auto.Enum<MySecondEnum>

    type MyUnion =
    | FirstUnion of Name: string * Age: int
    | SecondUnion of Something: float * Id: System.Guid

    let MyUnionGraph = Auto.Union<MyUnion>

    let firstGuid = Guid.Parse "5c8809cb-8816-4f2c-b187-08ddf664783a"
    let secondGuid = Guid.Parse "01ff3840-abad-4fd1-a68e-ad021d4c6868"
    let thirdGuid = Guid.Parse "eca96a5e-f2ec-413f-bfe8-9e05c248e148"

    let myQuery =
        query [
            field {
                name "getMyType"
                resolve (fun _ -> {name = "sup"})
            }
            field {
                name "myQuery"
                resolve (fun ctx -> ctx.GetArgument<int> "myArg" :: [1; 2; 3; 4; 5])
                arguments [
                    Define.Argument<int> ("myArg", 1)
                ]
            }
            fieldOf (NonNullGraphType MyEnumGraph) {
                name "myEnumNonNull"
                resolve (fun _ -> "MyFirstCase")
            }
            fieldOf MyEnumGraph {
                name "myEnum"
                resolve (fun _ -> "MyFirstCase")
            }
            field {
                name "mySecondEnumNonNull"
                resolve (fun _ -> First)
            }
            fieldOf MySecondEnumGraph {
                name "mySecondEnum"
                resolve (fun _ -> First)
            }
            field {
                name "withInput"
                arguments [
                    Define.Argument<MyTypeInput> "myArg"
                ]
                resolve (fun ctx -> {name = (ctx.GetArgument<MyTypeInput> "myArg").name} : MyType)
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
                    MyImplementation () :> IMyInterface
                    MyImplementationNum2 () :> IMyInterface
                    MyImplementation () :> IMyInterface
                ])
            }
            field {
                name "myInterface"
                resolve (fun _ -> MyImplementation () :> IMyInterface)
            }
        ]

    let mySchema =
        schema {
            query myQuery
            types [
                MyUnionGraph
                MySecondEnumGraph
                MyEnumGraph
                MyTypeInputGraph
                MyTypeManualGraph
                MyTypeGraph
                MyImplementationNum2Graph
                MyImplementationGraph
                IMyInterfaceGraph
            ]
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
        withInput(myArg: { name: "sup" }) {
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
            ... on MyUnionFirstUnion {
                name
                age
            }
        }
        myAutoUnionList {
            __typename
            ... on MyUnionFirstUnion {
                name
                age
            }
            ... on MyUnionSecondUnion {
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
        "__typename": "MyType"
      },
      "myEnum": "MyFirstCase",
      "mySecondEnum": "First",
      "withInput": {
        "name": "sup",
        "__typename": "MyType"
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
        "__typename": "MyUnionFirstUnion",
        "name": "sup",
        "age": 12
      },
      "myAutoUnionList": [
        {
          "__typename": "MyUnionFirstUnion",
          "name": "sup",
          "age": 12
        },
        {
          "__typename": "MyUnionFirstUnion",
          "name": "dfsjiosh",
          "age": 122
        },
        {
          "__typename": "MyUnionSecondUnion",
          "something": 1.2,
          "id": "5c8809cb-8816-4f2c-b187-08ddf664783a"
        },
        {
          "__typename": "MyUnionSecondUnion",
          "something": 1.5,
          "id": "01ff3840-abad-4fd1-a68e-ad021d4c6868"
        },
        {
          "__typename": "MyUnionSecondUnion",
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

[<Literal>]
let IntrosepctionQuery = """
    query IntrospectionQuery {
        __schema {
            queryType {
                name
            }
            mutationType {
                name
            }
            subscriptionType {
                name
            }
            types {
                ...FullType
            }
            directives {
                name
                description
                locations
                args {
                    ...InputValue
                }
            }
        }
    }

    fragment FullType on __Type {
        kind
        name
        description
        fields(includeDeprecated: true) {
            name
            description
            args {
                ...InputValue
            }
            type {
                ...TypeRef
            }
            isDeprecated
            deprecationReason
        }
        inputFields {
            ...InputValue
        }
        interfaces {
            ...TypeRef
        }
        enumValues(includeDeprecated: true) {
            name
            description
            isDeprecated
            deprecationReason
        }
        possibleTypes {
            ...TypeRef
        }
    }

    fragment InputValue on __InputValue {
        name
        description
        type {
            ...TypeRef
        }
        defaultValue
    }

    fragment TypeRef on __Type {
        kind
        name
        ofType {
            kind
            name
            ofType {
                kind
                name
                ofType {
                    kind
                    name
                    ofType {
                        kind
                        name
                        ofType {
                            kind
                            name
                            ofType {
                                kind
                                name
                                ofType {
                                    kind
                                    name
                                }
                            }
                        }
                    }
                }
            }
        }
    }
"""

[<Literal>]
let ExpectedIntrospectionResult = """
  {
    "data": {
      "__schema": {
        "queryType": {
          "name": "Query"
        },
        "mutationType": null,
        "subscriptionType": null,
        "types": [
          {
            "kind": "SCALAR",
            "name": "String",
            "description": null,
            "fields": null,
            "inputFields": null,
            "interfaces": null,
            "enumValues": null,
            "possibleTypes": null
          },
          {
            "kind": "SCALAR",
            "name": "Boolean",
            "description": null,
            "fields": null,
            "inputFields": null,
            "interfaces": null,
            "enumValues": null,
            "possibleTypes": null
          },
          {
            "kind": "SCALAR",
            "name": "Float",
            "description": null,
            "fields": null,
            "inputFields": null,
            "interfaces": null,
            "enumValues": null,
            "possibleTypes": null
          },
          {
            "kind": "SCALAR",
            "name": "Int",
            "description": null,
            "fields": null,
            "inputFields": null,
            "interfaces": null,
            "enumValues": null,
            "possibleTypes": null
          },
          {
            "kind": "SCALAR",
            "name": "ID",
            "description": null,
            "fields": null,
            "inputFields": null,
            "interfaces": null,
            "enumValues": null,
            "possibleTypes": null
          },
          {
            "kind": "SCALAR",
            "name": "Date",
            "description": "The `Date` scalar type represents a year, month and day in accordance with the [ISO-8601](https://en.wikipedia.org/wiki/ISO_8601) standard.",
            "fields": null,
            "inputFields": null,
            "interfaces": null,
            "enumValues": null,
            "possibleTypes": null
          },
          {
            "kind": "SCALAR",
            "name": "DateTime",
            "description": "The `DateTime` scalar type represents a date and time. `DateTime` expects timestamps to be formatted in accordance with the [ISO-8601](https://en.wikipedia.org/wiki/ISO_8601) standard.",
            "fields": null,
            "inputFields": null,
            "interfaces": null,
            "enumValues": null,
            "possibleTypes": null
          },
          {
            "kind": "SCALAR",
            "name": "DateTimeOffset",
            "description": "The `DateTimeOffset` scalar type represents a date, time and offset from UTC. `DateTimeOffset` expects timestamps to be formatted in accordance with the [ISO-8601](https://en.wikipedia.org/wiki/ISO_8601) standard.",
            "fields": null,
            "inputFields": null,
            "interfaces": null,
            "enumValues": null,
            "possibleTypes": null
          },
          {
            "kind": "SCALAR",
            "name": "Seconds",
            "description": "The `Seconds` scalar type represents a period of time represented as the total number of seconds.",
            "fields": null,
            "inputFields": null,
            "interfaces": null,
            "enumValues": null,
            "possibleTypes": null
          },
          {
            "kind": "SCALAR",
            "name": "Milliseconds",
            "description": "The `Milliseconds` scalar type represents a period of time represented as the total number of milliseconds.",
            "fields": null,
            "inputFields": null,
            "interfaces": null,
            "enumValues": null,
            "possibleTypes": null
          },
          {
            "kind": "SCALAR",
            "name": "Decimal",
            "description": null,
            "fields": null,
            "inputFields": null,
            "interfaces": null,
            "enumValues": null,
            "possibleTypes": null
          },
          {
            "kind": "OBJECT",
            "name": "__Schema",
            "description": "A GraphQL Schema defines the capabilities of a GraphQL server. It exposes all available types and directives on the server, as well as the entry points for query, mutation, and subscription operations.",
            "fields": [
              {
                "name": "directives",
                "description": "A list of all directives supported by this server.",
                "args": [],
                "type": {
                  "kind": "NON_NULL",
                  "name": null,
                  "ofType": {
                    "kind": "LIST",
                    "name": null,
                    "ofType": {
                      "kind": "NON_NULL",
                      "name": null,
                      "ofType": {
                        "kind": "OBJECT",
                        "name": "__Directive",
                        "ofType": null
                      }
                    }
                  }
                },
                "isDeprecated": false,
                "deprecationReason": null
              },
              {
                "name": "mutationType",
                "description": "If this server supports mutation, the type that mutation operations will be rooted at.",
                "args": [],
                "type": {
                  "kind": "OBJECT",
                  "name": "__Type",
                  "ofType": null
                },
                "isDeprecated": false,
                "deprecationReason": null
              },
              {
                "name": "queryType",
                "description": "The type that query operations will be rooted at.",
                "args": [],
                "type": {
                  "kind": "NON_NULL",
                  "name": null,
                  "ofType": {
                    "kind": "OBJECT",
                    "name": "__Type",
                    "ofType": null
                  }
                },
                "isDeprecated": false,
                "deprecationReason": null
              },
              {
                "name": "subscriptionType",
                "description": "If this server supports subscription, the type that subscription operations will be rooted at.",
                "args": [],
                "type": {
                  "kind": "OBJECT",
                  "name": "__Type",
                  "ofType": null
                },
                "isDeprecated": false,
                "deprecationReason": null
              },
              {
                "name": "types",
                "description": "A list of all types supported by this server.",
                "args": [],
                "type": {
                  "kind": "NON_NULL",
                  "name": null,
                  "ofType": {
                    "kind": "LIST",
                    "name": null,
                    "ofType": {
                      "kind": "NON_NULL",
                      "name": null,
                      "ofType": {
                        "kind": "OBJECT",
                        "name": "__Type",
                        "ofType": null
                      }
                    }
                  }
                },
                "isDeprecated": false,
                "deprecationReason": null
              }
            ],
            "inputFields": null,
            "interfaces": [],
            "enumValues": null,
            "possibleTypes": null
          },
          {
            "kind": "OBJECT",
            "name": "__Type",
            "description": "The fundamental unit of any GraphQL Schema is the type. There are many kinds of types in GraphQL as represented by the `__TypeKind` enum.\n\nDepending on the kind of a type, certain fields describe information about that type. Scalar types provide no information beyond a name and description, while Enum types provide their values. Object and Interface types provide the fields they describe. Abstract types, Union and Interface, provide the Object types possible at runtime. List and NonNull types compose other types.",
            "fields": [
              {
                "name": "description",
                "description": null,
                "args": [],
                "type": {
                  "kind": "SCALAR",
                  "name": "String",
                  "ofType": null
                },
                "isDeprecated": false,
                "deprecationReason": null
              },
              {
                "name": "enumValues",
                "description": null,
                "args": [
                  {
                    "name": "includeDeprecated",
                    "description": null,
                    "type": {
                      "kind": "SCALAR",
                      "name": "Boolean",
                      "ofType": null
                    },
                    "defaultValue": "false"
                  }
                ],
                "type": {
                  "kind": "LIST",
                  "name": null,
                  "ofType": {
                    "kind": "NON_NULL",
                    "name": null,
                    "ofType": {
                      "kind": "OBJECT",
                      "name": "__EnumValue",
                      "ofType": null
                    }
                  }
                },
                "isDeprecated": false,
                "deprecationReason": null
              },
              {
                "name": "fields",
                "description": null,
                "args": [
                  {
                    "name": "includeDeprecated",
                    "description": null,
                    "type": {
                      "kind": "SCALAR",
                      "name": "Boolean",
                      "ofType": null
                    },
                    "defaultValue": "false"
                  }
                ],
                "type": {
                  "kind": "LIST",
                  "name": null,
                  "ofType": {
                    "kind": "NON_NULL",
                    "name": null,
                    "ofType": {
                      "kind": "OBJECT",
                      "name": "__Field",
                      "ofType": null
                    }
                  }
                },
                "isDeprecated": false,
                "deprecationReason": null
              },
              {
                "name": "inputFields",
                "description": null,
                "args": [],
                "type": {
                  "kind": "LIST",
                  "name": null,
                  "ofType": {
                    "kind": "NON_NULL",
                    "name": null,
                    "ofType": {
                      "kind": "OBJECT",
                      "name": "__InputValue",
                      "ofType": null
                    }
                  }
                },
                "isDeprecated": false,
                "deprecationReason": null
              },
              {
                "name": "interfaces",
                "description": null,
                "args": [],
                "type": {
                  "kind": "LIST",
                  "name": null,
                  "ofType": {
                    "kind": "NON_NULL",
                    "name": null,
                    "ofType": {
                      "kind": "OBJECT",
                      "name": "__Type",
                      "ofType": null
                    }
                  }
                },
                "isDeprecated": false,
                "deprecationReason": null
              },
              {
                "name": "kind",
                "description": null,
                "args": [],
                "type": {
                  "kind": "NON_NULL",
                  "name": null,
                  "ofType": {
                    "kind": "ENUM",
                    "name": "__TypeKind",
                    "ofType": null
                  }
                },
                "isDeprecated": false,
                "deprecationReason": null
              },
              {
                "name": "name",
                "description": null,
                "args": [],
                "type": {
                  "kind": "SCALAR",
                  "name": "String",
                  "ofType": null
                },
                "isDeprecated": false,
                "deprecationReason": null
              },
              {
                "name": "ofType",
                "description": null,
                "args": [],
                "type": {
                  "kind": "OBJECT",
                  "name": "__Type",
                  "ofType": null
                },
                "isDeprecated": false,
                "deprecationReason": null
              },
              {
                "name": "possibleTypes",
                "description": null,
                "args": [],
                "type": {
                  "kind": "LIST",
                  "name": null,
                  "ofType": {
                    "kind": "NON_NULL",
                    "name": null,
                    "ofType": {
                      "kind": "OBJECT",
                      "name": "__Type",
                      "ofType": null
                    }
                  }
                },
                "isDeprecated": false,
                "deprecationReason": null
              }
            ],
            "inputFields": null,
            "interfaces": [],
            "enumValues": null,
            "possibleTypes": null
          },
          {
            "kind": "ENUM",
            "name": "__TypeKind",
            "description": "An enum describing what kind of type a given __Type is.",
            "fields": null,
            "inputFields": null,
            "interfaces": null,
            "enumValues": [
              {
                "name": "SCALAR",
                "description": "Indicates this type is a scalar.",
                "isDeprecated": false,
                "deprecationReason": null
              },
              {
                "name": "OBJECT",
                "description": "Indicates this type is an object.  `fields` and `possibleTypes` are valid fields.",
                "isDeprecated": false,
                "deprecationReason": null
              },
              {
                "name": "INTERFACE",
                "description": "Indicates this type is an interface.  `fields` and `possibleTypes` are valid fields.",
                "isDeprecated": false,
                "deprecationReason": null
              },
              {
                "name": "UNION",
                "description": "Indicates this type is a union.  `possibleTypes` is a valid field.",
                "isDeprecated": false,
                "deprecationReason": null
              },
              {
                "name": "ENUM",
                "description": "Indicates this type is an num.  `enumValues` is a valid field.",
                "isDeprecated": false,
                "deprecationReason": null
              },
              {
                "name": "INPUT_OBJECT",
                "description": "Indicates this type is an input object.  `inputFields` is a valid field.",
                "isDeprecated": false,
                "deprecationReason": null
              },
              {
                "name": "LIST",
                "description": "Indicates this type is a list.  `ofType` is a valid field.",
                "isDeprecated": false,
                "deprecationReason": null
              },
              {
                "name": "NON_NULL",
                "description": "Indicates this type is a non-null.  `ofType` is a valid field.",
                "isDeprecated": false,
                "deprecationReason": null
              }
            ],
            "possibleTypes": null
          },
          {
            "kind": "OBJECT",
            "name": "__Field",
            "description": "Object and Interface types are described by a list of Fields, each of which has a name, potentially a list of arguments, and a return type.",
            "fields": [
              {
                "name": "args",
                "description": null,
                "args": [],
                "type": {
                  "kind": "NON_NULL",
                  "name": null,
                  "ofType": {
                    "kind": "LIST",
                    "name": null,
                    "ofType": {
                      "kind": "NON_NULL",
                      "name": null,
                      "ofType": {
                        "kind": "OBJECT",
                        "name": "__InputValue",
                        "ofType": null
                      }
                    }
                  }
                },
                "isDeprecated": false,
                "deprecationReason": null
              },
              {
                "name": "deprecationReason",
                "description": null,
                "args": [],
                "type": {
                  "kind": "SCALAR",
                  "name": "String",
                  "ofType": null
                },
                "isDeprecated": false,
                "deprecationReason": null
              },
              {
                "name": "description",
                "description": null,
                "args": [],
                "type": {
                  "kind": "SCALAR",
                  "name": "String",
                  "ofType": null
                },
                "isDeprecated": false,
                "deprecationReason": null
              },
              {
                "name": "isDeprecated",
                "description": null,
                "args": [],
                "type": {
                  "kind": "NON_NULL",
                  "name": null,
                  "ofType": {
                    "kind": "SCALAR",
                    "name": "Boolean",
                    "ofType": null
                  }
                },
                "isDeprecated": false,
                "deprecationReason": null
              },
              {
                "name": "name",
                "description": null,
                "args": [],
                "type": {
                  "kind": "NON_NULL",
                  "name": null,
                  "ofType": {
                    "kind": "SCALAR",
                    "name": "String",
                    "ofType": null
                  }
                },
                "isDeprecated": false,
                "deprecationReason": null
              },
              {
                "name": "type",
                "description": null,
                "args": [],
                "type": {
                  "kind": "NON_NULL",
                  "name": null,
                  "ofType": {
                    "kind": "OBJECT",
                    "name": "__Type",
                    "ofType": null
                  }
                },
                "isDeprecated": false,
                "deprecationReason": null
              }
            ],
            "inputFields": null,
            "interfaces": [],
            "enumValues": null,
            "possibleTypes": null
          },
          {
            "kind": "OBJECT",
            "name": "__InputValue",
            "description": "Arguments provided to Fields or Directives and the input fields of an InputObject are represented as Input Values which describe their type and optionally a default value.",
            "fields": [
              {
                "name": "defaultValue",
                "description": "A GraphQL-formatted string representing the default value for this input value.",
                "args": [],
                "type": {
                  "kind": "SCALAR",
                  "name": "String",
                  "ofType": null
                },
                "isDeprecated": false,
                "deprecationReason": null
              },
              {
                "name": "description",
                "description": null,
                "args": [],
                "type": {
                  "kind": "SCALAR",
                  "name": "String",
                  "ofType": null
                },
                "isDeprecated": false,
                "deprecationReason": null
              },
              {
                "name": "name",
                "description": null,
                "args": [],
                "type": {
                  "kind": "NON_NULL",
                  "name": null,
                  "ofType": {
                    "kind": "SCALAR",
                    "name": "String",
                    "ofType": null
                  }
                },
                "isDeprecated": false,
                "deprecationReason": null
              },
              {
                "name": "type",
                "description": null,
                "args": [],
                "type": {
                  "kind": "NON_NULL",
                  "name": null,
                  "ofType": {
                    "kind": "OBJECT",
                    "name": "__Type",
                    "ofType": null
                  }
                },
                "isDeprecated": false,
                "deprecationReason": null
              }
            ],
            "inputFields": null,
            "interfaces": [],
            "enumValues": null,
            "possibleTypes": null
          },
          {
            "kind": "OBJECT",
            "name": "__EnumValue",
            "description": "One possible value for a given Enum. Enum values are unique values, not a placeholder for a string or numeric value. However an Enum value is returned in a JSON response as a string.",
            "fields": [
              {
                "name": "deprecationReason",
                "description": null,
                "args": [],
                "type": {
                  "kind": "SCALAR",
                  "name": "String",
                  "ofType": null
                },
                "isDeprecated": false,
                "deprecationReason": null
              },
              {
                "name": "description",
                "description": null,
                "args": [],
                "type": {
                  "kind": "SCALAR",
                  "name": "String",
                  "ofType": null
                },
                "isDeprecated": false,
                "deprecationReason": null
              },
              {
                "name": "isDeprecated",
                "description": null,
                "args": [],
                "type": {
                  "kind": "NON_NULL",
                  "name": null,
                  "ofType": {
                    "kind": "SCALAR",
                    "name": "String",
                    "ofType": null
                  }
                },
                "isDeprecated": false,
                "deprecationReason": null
              },
              {
                "name": "name",
                "description": null,
                "args": [],
                "type": {
                  "kind": "NON_NULL",
                  "name": null,
                  "ofType": {
                    "kind": "SCALAR",
                    "name": "String",
                    "ofType": null
                  }
                },
                "isDeprecated": false,
                "deprecationReason": null
              }
            ],
            "inputFields": null,
            "interfaces": [],
            "enumValues": null,
            "possibleTypes": null
          },
          {
            "kind": "OBJECT",
            "name": "__Directive",
            "description": "A Directive provides a way to describe alternate runtime execution and type validation behavior in a GraphQL document.\n\nIn some cases, you need to provide options to alter GraphQL's execution behavior in ways field arguments will not suffice, such as conditionally including or skipping a field. Directives provide this by describing additional information to the executor.",
            "fields": [
              {
                "name": "args",
                "description": null,
                "args": [],
                "type": {
                  "kind": "NON_NULL",
                  "name": null,
                  "ofType": {
                    "kind": "LIST",
                    "name": null,
                    "ofType": {
                      "kind": "NON_NULL",
                      "name": null,
                      "ofType": {
                        "kind": "OBJECT",
                        "name": "__InputValue",
                        "ofType": null
                      }
                    }
                  }
                },
                "isDeprecated": false,
                "deprecationReason": null
              },
              {
                "name": "description",
                "description": null,
                "args": [],
                "type": {
                  "kind": "SCALAR",
                  "name": "String",
                  "ofType": null
                },
                "isDeprecated": false,
                "deprecationReason": null
              },
              {
                "name": "locations",
                "description": null,
                "args": [],
                "type": {
                  "kind": "NON_NULL",
                  "name": null,
                  "ofType": {
                    "kind": "LIST",
                    "name": null,
                    "ofType": {
                      "kind": "NON_NULL",
                      "name": null,
                      "ofType": {
                        "kind": "ENUM",
                        "name": "__DirectiveLocation",
                        "ofType": null
                      }
                    }
                  }
                },
                "isDeprecated": false,
                "deprecationReason": null
              },
              {
                "name": "name",
                "description": null,
                "args": [],
                "type": {
                  "kind": "NON_NULL",
                  "name": null,
                  "ofType": {
                    "kind": "SCALAR",
                    "name": "String",
                    "ofType": null
                  }
                },
                "isDeprecated": false,
                "deprecationReason": null
              },
              {
                "name": "onField",
                "description": null,
                "args": [],
                "type": {
                  "kind": "NON_NULL",
                  "name": null,
                  "ofType": {
                    "kind": "SCALAR",
                    "name": "Boolean",
                    "ofType": null
                  }
                },
                "isDeprecated": true,
                "deprecationReason": "Use 'locations'."
              },
              {
                "name": "onFragment",
                "description": null,
                "args": [],
                "type": {
                  "kind": "NON_NULL",
                  "name": null,
                  "ofType": {
                    "kind": "SCALAR",
                    "name": "Boolean",
                    "ofType": null
                  }
                },
                "isDeprecated": true,
                "deprecationReason": "Use 'locations'."
              },
              {
                "name": "onOperation",
                "description": null,
                "args": [],
                "type": {
                  "kind": "NON_NULL",
                  "name": null,
                  "ofType": {
                    "kind": "SCALAR",
                    "name": "Boolean",
                    "ofType": null
                  }
                },
                "isDeprecated": true,
                "deprecationReason": "Use 'locations'."
              }
            ],
            "inputFields": null,
            "interfaces": [],
            "enumValues": null,
            "possibleTypes": null
          },
          {
            "kind": "ENUM",
            "name": "__DirectiveLocation",
            "description": "A Directive can be adjacent to many parts of the GraphQL language, a __DirectiveLocation describes one such possible adjacencies.",
            "fields": null,
            "inputFields": null,
            "interfaces": null,
            "enumValues": [
              {
                "name": "QUERY",
                "description": "Location adjacent to a query operation.",
                "isDeprecated": false,
                "deprecationReason": null
              },
              {
                "name": "MUTATION",
                "description": "Location adjacent to a mutation operation.",
                "isDeprecated": false,
                "deprecationReason": null
              },
              {
                "name": "SUBSCRIPTION",
                "description": "Location adjacent to a subscription operation.",
                "isDeprecated": false,
                "deprecationReason": null
              },
              {
                "name": "FIELD",
                "description": "Location adjacent to a field.",
                "isDeprecated": false,
                "deprecationReason": null
              },
              {
                "name": "FRAGMENT_DEFINITION",
                "description": "Location adjacent to a fragment definition.",
                "isDeprecated": false,
                "deprecationReason": null
              },
              {
                "name": "FRAGMENT_SPREAD",
                "description": "Location adjacent to a fragment spread.",
                "isDeprecated": false,
                "deprecationReason": null
              },
              {
                "name": "INLINE_FRAGMENT",
                "description": "Location adjacent to an inline fragment.",
                "isDeprecated": false,
                "deprecationReason": null
              },
              {
                "name": "SCHEMA",
                "description": "Location adjacent to a schema definition.",
                "isDeprecated": false,
                "deprecationReason": null
              },
              {
                "name": "SCALAR",
                "description": "Location adjacent to a scalar definition.",
                "isDeprecated": false,
                "deprecationReason": null
              },
              {
                "name": "OBJECT",
                "description": "Location adjacent to an object type definition.",
                "isDeprecated": false,
                "deprecationReason": null
              },
              {
                "name": "FIELD_DEFINITION",
                "description": "Location adjacent to a field definition.",
                "isDeprecated": false,
                "deprecationReason": null
              },
              {
                "name": "ARGUMENT_DEFINITION",
                "description": "Location adjacent to an argument definition.",
                "isDeprecated": false,
                "deprecationReason": null
              },
              {
                "name": "INTERFACE",
                "description": "Location adjacent to an interface definition.",
                "isDeprecated": false,
                "deprecationReason": null
              },
              {
                "name": "UNION",
                "description": "Location adjacent to a union definition.",
                "isDeprecated": false,
                "deprecationReason": null
              },
              {
                "name": "ENUM",
                "description": "Location adjacent to an enum definition",
                "isDeprecated": false,
                "deprecationReason": null
              },
              {
                "name": "ENUM_VALUE",
                "description": "Location adjacent to an enum value definition",
                "isDeprecated": false,
                "deprecationReason": null
              },
              {
                "name": "INPUT_OBJECT",
                "description": "Location adjacent to an input object type definition.",
                "isDeprecated": false,
                "deprecationReason": null
              },
              {
                "name": "INPUT_FIELD_DEFINITION",
                "description": "Location adjacent to an input object field definition.",
                "isDeprecated": false,
                "deprecationReason": null
              }
            ],
            "possibleTypes": null
          },
          {
            "kind": "UNION",
            "name": "MyUnion",
            "description": null,
            "fields": null,
            "inputFields": null,
            "interfaces": null,
            "enumValues": null,
            "possibleTypes": [
              {
                "kind": "OBJECT",
                "name": "MyUnionFirstUnion",
                "ofType": null
              },
              {
                "kind": "OBJECT",
                "name": "MyUnionSecondUnion",
                "ofType": null
              }
            ]
          },
          {
            "kind": "OBJECT",
            "name": "MyUnionFirstUnion",
            "description": null,
            "fields": [
              {
                "name": "age",
                "description": null,
                "args": [],
                "type": {
                  "kind": "NON_NULL",
                  "name": null,
                  "ofType": {
                    "kind": "SCALAR",
                    "name": "Int",
                    "ofType": null
                  }
                },
                "isDeprecated": false,
                "deprecationReason": null
              },
              {
                "name": "name",
                "description": null,
                "args": [],
                "type": {
                  "kind": "NON_NULL",
                  "name": null,
                  "ofType": {
                    "kind": "SCALAR",
                    "name": "String",
                    "ofType": null
                  }
                },
                "isDeprecated": false,
                "deprecationReason": null
              },
              {
                "name": "tag",
                "description": null,
                "args": [],
                "type": {
                  "kind": "NON_NULL",
                  "name": null,
                  "ofType": {
                    "kind": "SCALAR",
                    "name": "Int",
                    "ofType": null
                  }
                },
                "isDeprecated": false,
                "deprecationReason": null
              }
            ],
            "inputFields": null,
            "interfaces": [],
            "enumValues": null,
            "possibleTypes": null
          },
          {
            "kind": "OBJECT",
            "name": "MyUnionSecondUnion",
            "description": null,
            "fields": [
              {
                "name": "id",
                "description": null,
                "args": [],
                "type": {
                  "kind": "NON_NULL",
                  "name": null,
                  "ofType": {
                    "kind": "SCALAR",
                    "name": "ID",
                    "ofType": null
                  }
                },
                "isDeprecated": false,
                "deprecationReason": null
              },
              {
                "name": "something",
                "description": null,
                "args": [],
                "type": {
                  "kind": "NON_NULL",
                  "name": null,
                  "ofType": {
                    "kind": "SCALAR",
                    "name": "Float",
                    "ofType": null
                  }
                },
                "isDeprecated": false,
                "deprecationReason": null
              },
              {
                "name": "tag",
                "description": null,
                "args": [],
                "type": {
                  "kind": "NON_NULL",
                  "name": null,
                  "ofType": {
                    "kind": "SCALAR",
                    "name": "Int",
                    "ofType": null
                  }
                },
                "isDeprecated": false,
                "deprecationReason": null
              }
            ],
            "inputFields": null,
            "interfaces": [],
            "enumValues": null,
            "possibleTypes": null
          },
          {
            "kind": "ENUM",
            "name": "MySecondEnum",
            "description": null,
            "fields": null,
            "inputFields": null,
            "interfaces": null,
            "enumValues": [
              {
                "name": "First",
                "description": null,
                "isDeprecated": false,
                "deprecationReason": null
              },
              {
                "name": "Second",
                "description": null,
                "isDeprecated": false,
                "deprecationReason": null
              }
            ],
            "possibleTypes": null
          },
          {
            "kind": "ENUM",
            "name": "myEnum",
            "description": null,
            "fields": null,
            "inputFields": null,
            "interfaces": null,
            "enumValues": [
              {
                "name": "MyFirstCase",
                "description": null,
                "isDeprecated": false,
                "deprecationReason": null
              }
            ],
            "possibleTypes": null
          },
          {
            "kind": "INPUT_OBJECT",
            "name": "MyTypeInput",
            "description": null,
            "fields": null,
            "inputFields": [
              {
                "name": "name",
                "description": null,
                "type": {
                  "kind": "NON_NULL",
                  "name": null,
                  "ofType": {
                    "kind": "SCALAR",
                    "name": "String",
                    "ofType": null
                  }
                },
                "defaultValue": "null"
              }
            ],
            "interfaces": null,
            "enumValues": null,
            "possibleTypes": null
          },
          {
            "kind": "OBJECT",
            "name": "myTypeManual",
            "description": null,
            "fields": [
              {
                "name": "name",
                "description": null,
                "args": [],
                "type": {
                  "kind": "NON_NULL",
                  "name": null,
                  "ofType": {
                    "kind": "SCALAR",
                    "name": "String",
                    "ofType": null
                  }
                },
                "isDeprecated": false,
                "deprecationReason": null
              }
            ],
            "inputFields": null,
            "interfaces": [],
            "enumValues": null,
            "possibleTypes": null
          },
          {
            "kind": "OBJECT",
            "name": "MyType",
            "description": null,
            "fields": [
              {
                "name": "name",
                "description": null,
                "args": [],
                "type": {
                  "kind": "NON_NULL",
                  "name": null,
                  "ofType": {
                    "kind": "SCALAR",
                    "name": "String",
                    "ofType": null
                  }
                },
                "isDeprecated": false,
                "deprecationReason": null
              }
            ],
            "inputFields": null,
            "interfaces": [],
            "enumValues": null,
            "possibleTypes": null
          },
          {
            "kind": "OBJECT",
            "name": "MyImplementationNum2",
            "description": null,
            "fields": [
              {
                "name": "getSomeOtherName",
                "description": null,
                "args": [
                  {
                    "name": "name",
                    "description": null,
                    "type": {
                      "kind": "NON_NULL",
                      "name": null,
                      "ofType": {
                        "kind": "SCALAR",
                        "name": "String",
                        "ofType": null
                      }
                    },
                    "defaultValue": "null"
                  }
                ],
                "type": {
                  "kind": "NON_NULL",
                  "name": null,
                  "ofType": {
                    "kind": "SCALAR",
                    "name": "String",
                    "ofType": null
                  }
                },
                "isDeprecated": false,
                "deprecationReason": null
              },
              {
                "name": "someName",
                "description": null,
                "args": [],
                "type": {
                  "kind": "NON_NULL",
                  "name": null,
                  "ofType": {
                    "kind": "SCALAR",
                    "name": "String",
                    "ofType": null
                  }
                },
                "isDeprecated": false,
                "deprecationReason": null
              },
              {
                "name": "someThirdName",
                "description": null,
                "args": [],
                "type": {
                  "kind": "NON_NULL",
                  "name": null,
                  "ofType": {
                    "kind": "SCALAR",
                    "name": "String",
                    "ofType": null
                  }
                },
                "isDeprecated": false,
                "deprecationReason": null
              }
            ],
            "inputFields": null,
            "interfaces": [
              {
                "kind": "INTERFACE",
                "name": "IMyInterface",
                "ofType": null
              }
            ],
            "enumValues": null,
            "possibleTypes": null
          },
          {
            "kind": "OBJECT",
            "name": "MyImplementation",
            "description": null,
            "fields": [
              {
                "name": "getSomeOtherName",
                "description": null,
                "args": [
                  {
                    "name": "name",
                    "description": null,
                    "type": {
                      "kind": "NON_NULL",
                      "name": null,
                      "ofType": {
                        "kind": "SCALAR",
                        "name": "String",
                        "ofType": null
                      }
                    },
                    "defaultValue": "null"
                  }
                ],
                "type": {
                  "kind": "NON_NULL",
                  "name": null,
                  "ofType": {
                    "kind": "SCALAR",
                    "name": "String",
                    "ofType": null
                  }
                },
                "isDeprecated": false,
                "deprecationReason": null
              },
              {
                "name": "getSomeThirdName",
                "description": null,
                "args": [
                  {
                    "name": "name",
                    "description": null,
                    "type": {
                      "kind": "NON_NULL",
                      "name": null,
                      "ofType": {
                        "kind": "SCALAR",
                        "name": "String",
                        "ofType": null
                      }
                    },
                    "defaultValue": "null"
                  }
                ],
                "type": {
                  "kind": "NON_NULL",
                  "name": null,
                  "ofType": {
                    "kind": "SCALAR",
                    "name": "String",
                    "ofType": null
                  }
                },
                "isDeprecated": false,
                "deprecationReason": null
              },
              {
                "name": "someName",
                "description": null,
                "args": [],
                "type": {
                  "kind": "NON_NULL",
                  "name": null,
                  "ofType": {
                    "kind": "SCALAR",
                    "name": "String",
                    "ofType": null
                  }
                },
                "isDeprecated": false,
                "deprecationReason": null
              },
              {
                "name": "someOtherName",
                "description": null,
                "args": [],
                "type": {
                  "kind": "NON_NULL",
                  "name": null,
                  "ofType": {
                    "kind": "SCALAR",
                    "name": "String",
                    "ofType": null
                  }
                },
                "isDeprecated": false,
                "deprecationReason": null
              }
            ],
            "inputFields": null,
            "interfaces": [
              {
                "kind": "INTERFACE",
                "name": "IMyInterface",
                "ofType": null
              }
            ],
            "enumValues": null,
            "possibleTypes": null
          },
          {
            "kind": "INTERFACE",
            "name": "IMyInterface",
            "description": null,
            "fields": [
              {
                "name": "getSomeOtherName",
                "description": null,
                "args": [
                  {
                    "name": "name",
                    "description": null,
                    "type": {
                      "kind": "NON_NULL",
                      "name": null,
                      "ofType": {
                        "kind": "SCALAR",
                        "name": "String",
                        "ofType": null
                      }
                    },
                    "defaultValue": "null"
                  }
                ],
                "type": {
                  "kind": "NON_NULL",
                  "name": null,
                  "ofType": {
                    "kind": "SCALAR",
                    "name": "String",
                    "ofType": null
                  }
                },
                "isDeprecated": false,
                "deprecationReason": null
              },
              {
                "name": "someName",
                "description": null,
                "args": [],
                "type": {
                  "kind": "NON_NULL",
                  "name": null,
                  "ofType": {
                    "kind": "SCALAR",
                    "name": "String",
                    "ofType": null
                  }
                },
                "isDeprecated": false,
                "deprecationReason": null
              }
            ],
            "inputFields": null,
            "interfaces": null,
            "enumValues": null,
            "possibleTypes": [
              {
                "kind": "OBJECT",
                "name": "MyImplementationNum2",
                "ofType": null
              },
              {
                "kind": "OBJECT",
                "name": "MyImplementation",
                "ofType": null
              }
            ]
          },
          {
            "kind": "OBJECT",
            "name": "Query",
            "description": null,
            "fields": [
              {
                "name": "getMyType",
                "description": null,
                "args": [],
                "type": {
                  "kind": "NON_NULL",
                  "name": null,
                  "ofType": {
                    "kind": "OBJECT",
                    "name": "MyType",
                    "ofType": null
                  }
                },
                "isDeprecated": false,
                "deprecationReason": null
              },
              {
                "name": "myAutoUnion",
                "description": null,
                "args": [],
                "type": {
                  "kind": "NON_NULL",
                  "name": null,
                  "ofType": {
                    "kind": "UNION",
                    "name": "MyUnion",
                    "ofType": null
                  }
                },
                "isDeprecated": false,
                "deprecationReason": null
              },
              {
                "name": "myAutoUnionList",
                "description": null,
                "args": [],
                "type": {
                  "kind": "NON_NULL",
                  "name": null,
                  "ofType": {
                    "kind": "LIST",
                    "name": null,
                    "ofType": {
                      "kind": "NON_NULL",
                      "name": null,
                      "ofType": {
                        "kind": "UNION",
                        "name": "MyUnion",
                        "ofType": null
                      }
                    }
                  }
                },
                "isDeprecated": false,
                "deprecationReason": null
              },
              {
                "name": "myEnum",
                "description": null,
                "args": [],
                "type": {
                  "kind": "ENUM",
                  "name": "myEnum",
                  "ofType": null
                },
                "isDeprecated": false,
                "deprecationReason": null
              },
              {
                "name": "myEnumNonNull",
                "description": null,
                "args": [],
                "type": {
                  "kind": "NON_NULL",
                  "name": null,
                  "ofType": {
                    "kind": "ENUM",
                    "name": "myEnum",
                    "ofType": null
                  }
                },
                "isDeprecated": false,
                "deprecationReason": null
              },
              {
                "name": "myImpl",
                "description": null,
                "args": [],
                "type": {
                  "kind": "NON_NULL",
                  "name": null,
                  "ofType": {
                    "kind": "OBJECT",
                    "name": "MyImplementation",
                    "ofType": null
                  }
                },
                "isDeprecated": false,
                "deprecationReason": null
              },
              {
                "name": "myImplList",
                "description": null,
                "args": [],
                "type": {
                  "kind": "NON_NULL",
                  "name": null,
                  "ofType": {
                    "kind": "LIST",
                    "name": null,
                    "ofType": {
                      "kind": "NON_NULL",
                      "name": null,
                      "ofType": {
                        "kind": "INTERFACE",
                        "name": "IMyInterface",
                        "ofType": null
                      }
                    }
                  }
                },
                "isDeprecated": false,
                "deprecationReason": null
              },
              {
                "name": "myInterface",
                "description": null,
                "args": [],
                "type": {
                  "kind": "NON_NULL",
                  "name": null,
                  "ofType": {
                    "kind": "INTERFACE",
                    "name": "IMyInterface",
                    "ofType": null
                  }
                },
                "isDeprecated": false,
                "deprecationReason": null
              },
              {
                "name": "myQuery",
                "description": null,
                "args": [
                  {
                    "name": "myArg",
                    "description": null,
                    "type": {
                      "kind": "NON_NULL",
                      "name": null,
                      "ofType": {
                        "kind": "SCALAR",
                        "name": "Int",
                        "ofType": null
                      }
                    },
                    "defaultValue": "1"
                  }
                ],
                "type": {
                  "kind": "NON_NULL",
                  "name": null,
                  "ofType": {
                    "kind": "LIST",
                    "name": null,
                    "ofType": {
                      "kind": "NON_NULL",
                      "name": null,
                      "ofType": {
                        "kind": "SCALAR",
                        "name": "Int",
                        "ofType": null
                      }
                    }
                  }
                },
                "isDeprecated": false,
                "deprecationReason": null
              },
              {
                "name": "mySecondEnum",
                "description": null,
                "args": [],
                "type": {
                  "kind": "ENUM",
                  "name": "MySecondEnum",
                  "ofType": null
                },
                "isDeprecated": false,
                "deprecationReason": null
              },
              {
                "name": "mySecondEnumNonNull",
                "description": null,
                "args": [],
                "type": {
                  "kind": "NON_NULL",
                  "name": null,
                  "ofType": {
                    "kind": "ENUM",
                    "name": "MySecondEnum",
                    "ofType": null
                  }
                },
                "isDeprecated": false,
                "deprecationReason": null
              },
              {
                "name": "withInput",
                "description": null,
                "args": [
                  {
                    "name": "myArg",
                    "description": null,
                    "type": {
                      "kind": "NON_NULL",
                      "name": null,
                      "ofType": {
                        "kind": "INPUT_OBJECT",
                        "name": "MyTypeInput",
                        "ofType": null
                      }
                    },
                    "defaultValue": "null"
                  }
                ],
                "type": {
                  "kind": "NON_NULL",
                  "name": null,
                  "ofType": {
                    "kind": "OBJECT",
                    "name": "MyType",
                    "ofType": null
                  }
                },
                "isDeprecated": false,
                "deprecationReason": null
              }
            ],
            "inputFields": null,
            "interfaces": [],
            "enumValues": null,
            "possibleTypes": null
          }
        ],
        "directives": [
          {
            "name": "include",
            "description": "Directs the executor to include this field or fragment only when the 'if' argument is true.",
            "locations": [
              "FIELD",
              "FRAGMENT_SPREAD",
              "INLINE_FRAGMENT"
            ],
            "args": [
              {
                "name": "if",
                "description": "Included when true.",
                "type": {
                  "kind": "NON_NULL",
                  "name": null,
                  "ofType": {
                    "kind": "SCALAR",
                    "name": "Boolean",
                    "ofType": null
                  }
                },
                "defaultValue": "null"
              }
            ]
          },
          {
            "name": "skip",
            "description": "Directs the executor to skip this field or fragment when the 'if' argument is true.",
            "locations": [
              "FIELD",
              "FRAGMENT_SPREAD",
              "INLINE_FRAGMENT"
            ],
            "args": [
              {
                "name": "if",
                "description": "Skipped when true.",
                "type": {
                  "kind": "NON_NULL",
                  "name": null,
                  "ofType": {
                    "kind": "SCALAR",
                    "name": "Boolean",
                    "ofType": null
                  }
                },
                "defaultValue": "null"
              }
            ]
          },
          {
            "name": "deprecated",
            "description": "Marks an element of a GraphQL schema as no longer supported.",
            "locations": [
              "FIELD_DEFINITION",
              "ENUM_VALUE"
            ],
            "args": [
              {
                "name": "reason",
                "description": "Explains why this element was deprecated, usually also including a suggestion for how to access supported similar data. Formatted in [Markdown](https://daringfireball.net/projects/markdown/).",
                "type": {
                  "kind": "SCALAR",
                  "name": "String",
                  "ofType": null
                },
                "defaultValue": "\"No longer supported\""
              }
            ]
          }
        ]
      }
    }
  }
"""

[<Test>]
let ``Introspection query`` () =
    queryEqual IntrosepctionQuery ExpectedIntrospectionResult mySchema
