module GraphQL.FSharp.UnitTests.Builder.Field

open System
open System.Threading.Tasks
open NUnit.Framework
open Swensen.Unquote
open FSharp.Utils.Tasks
open GraphQL.FSharp.Builder
open GraphQL.FSharp.Types
open GraphQL.FSharp.Utils
open GraphQL.Types

open GraphQL.FSharp.TestUtils.Assert

type MyInferenceType = {
    Name: string
}

[<Test>]
let ``type inference`` () =
    field<MyInferenceType, _, _>  __ [
        name "myInferenceTypeField"
    ]
    |> fieldEqual "myInferenceTypeField" (fun () -> upcast NonNullGraphType (GraphQLTypeReference "MyInferenceType"))

[<Test>]
let ``type inference with validation return type`` () =
    field<Result<MyInferenceType, _ list>, _, _>  __ [
        name "myInferenceTypeField"
    ]
    |> fieldEqual "myInferenceTypeField" (fun () -> upcast GraphQLTypeReference "MyInferenceType")

[<Test>]
let ``type inference with result return type`` () =
    field<Result<MyInferenceType, _>, _, _>  __ [
        name "myInferenceTypeField"
    ]
    |> fieldEqual "myInferenceTypeField" (fun () -> upcast GraphQLTypeReference "MyInferenceType")

[<Test>]
let ``type inference with option return type`` () =
    field<MyInferenceType option, _, _>  __ [
        name "myInferenceTypeField"
    ]
    |> fieldEqual "myInferenceTypeField" (fun () -> upcast GraphQLTypeReference "MyInferenceType")

[<Test>]
let ``configure test`` () =
    field __ [
        name "getTask"
        resolve.method (fun _ _ -> vtask { return "Hello" })
        configureUnit "" (fun this -> this.Name <- "getTaskChanged"; this.ResolvedType <- FloatGraphType ())
    ]
    |> fieldEqual "getTaskChanged" (nullable FloatGraphType)

module ``quotation tests`` =
    type MyObjectType =
        {
            Name: string
        }
        member this.SomeOtherProperty = this.Name

    [<Test>]
    let ``FieldName basic`` () =
        (|FieldName|_|) <@ fun x -> x.Name @> =! Some "Name"

    [<Test>]
    let ``FieldName additional property`` () =
        (|FieldName|_|) <@ fun (x: MyObjectType) -> x.SomeOtherProperty @> =! Some "SomeOtherProperty"

    [<Test>]
    let ``FieldName invalid expression`` () =
        (|FieldName|_|) <@ fun _ -> "Something" @> =! None


[<Test>]
let ``Builder Field task return types`` () =
    field __ [
        name "getTask"
        resolve.method (fun _ _ -> vtask { return "Hello" })
    ]
    |> fieldEqual "getTask" (nonNull StringGraphType)

type MyTaskObject() =
    member __.GetNameStored () = vtask { return "Hello " }

[<Test>]
let ``Builder Field task return types inferred`` () =
    field __ [
        resolve.method (fun (x: MyTaskObject) _ -> x.GetNameStored())
    ]
    |> fieldEqual "GetNameStored" (nonNull StringGraphType)

[<Test>]
let ``Builder Field option types`` () =
    field __ [
        name "optionField"
        resolve.method (fun _ _ -> vtask { return (Some "hello") })
    ]
    |> fieldEqual "optionField" (nullable StringGraphType)

[<CLIMutable>]
type MyOptionObject = {
    NameOption: string option
}

[<Test>]
let ``Builder Field option types inferred`` () =
    field __ [
        resolve.property (fun this -> vtask { return this.NameOption })
    ]
    |> fieldEqual "NameOption" (nullable StringGraphType)


[<CLIMutable>]
type MyType = {
    Name: string
}

// TODO: fix this later
// [<Test>]
// let ``Builder Field getter invalid argument`` () =
//     raises<ArgumentException>
//         <@
//             field __ [
//                 resolve.property (fun x -> vtask { return (x.Name.ToString ()) })
//             ]
//         @>

[<Test>]
let ``Builder Field valid getter`` () =
    field __ [
        resolve.property (fun x -> vtask { return x.Name })
    ]
    |> fieldEqual "Name" (nonNull StringGraphType)

[<CLIMutable>]
type SomeType = {
    Testing: bool
}

[<Test>]
let ``Builder Field inferred field type without default value should be non nullable`` () =
    field __ [
        resolve.property (fun x -> vtask { return x.Testing })
    ]
    |> fieldEqual "Testing" (nonNull BooleanGraphType)

[<Test>]
let ``Builder Field inferred field type with default value should be nullable`` () =
    field __ [
        resolve.property (fun x -> vtask { return x.Testing })
        defaultValue false
    ]
    |> fieldEqual "Testing" (nullable BooleanGraphType)
