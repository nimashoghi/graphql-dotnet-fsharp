module GraphQL.FSharp.UnitTests.Builder.Field

open System
open System.Threading.Tasks
open NUnit.Framework
open Swensen.Unquote
open GraphQL.FSharp.Builder
open GraphQL.FSharp.Utils
open GraphQL.Types

open GraphQL.FSharp.TestUtils.Assert

module ``configure test`` =
    [<Test>]
    let ``basic test`` () =
        field {
            name "getTask"
            resolveAsync (fun _ -> Task.FromResult "Hello")
            configure (fun this -> this.Name <- "getTaskChanged"; this.ResolvedType <- FloatGraphType ())
        }
        |> fieldEqual "getTaskChanged" (nullable FloatGraphType)



module Quotations =
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

    type MyAsyncObject =
        {
            GetSomeAsyncMethod: unit -> string Task
        }
        member this.GetSomeAsyncMethod2 () = this.GetSomeAsyncMethod ()

    [<Test>]
    let ``AsyncFieldName record property`` () =
        (|AsyncFieldName|_|) <@ fun x -> x.GetSomeAsyncMethod () @> =! Some "GetSomeAsyncMethod"

    [<Test>]
    let ``AsyncFieldName method`` () =
        (|AsyncFieldName|_|) <@ fun (x: MyAsyncObject) -> x.GetSomeAsyncMethod2 () @> =! Some "GetSomeAsyncMethod2"


[<Test>]
let ``Builder Field task return types`` () =
    field {
        name "getTask"
        resolveAsync (fun _ -> Task.FromResult "Hello")
    }
    |> fieldEqual "getTask" (nonNull StringGraphType)

type MyTaskObject() =
    member __.GetNameStored () = Task.FromResult "Hello"

[<Test>]
let ``Builder Field task return types inferred`` () =
    field {
        getAsync (fun (x: MyTaskObject) -> x.GetNameStored ())
    }
    |> fieldEqual "GetNameStored" (nonNull StringGraphType)

[<Test>]
let ``Builder Field option types`` () =
    field {
        name "optionField"
        resolve (fun _ -> Some "hello")
    }
    |> fieldEqual "optionField" (nullable StringGraphType)

[<CLIMutable>]
type MyOptionObject = {
    NameOption: string option
}

[<Test>]
let ``Builder Field option types inferred`` () =
    field {
        get (fun obj -> obj.NameOption)
    }
    |> fieldEqual "NameOption" (nullable StringGraphType)


[<CLIMutable>]
type MyType = {
    Name: string
}

[<Test>]
let ``Builder Field getter invalid argument`` () =
    raises<ArgumentException>
        <@
            field {
                get (fun x -> x.Name.ToString())
            }
        @>

[<Test>]
let ``Builder Field valid getter`` () =
    field {
        get (fun x -> x.Name)
    }
    |> fieldEqual "Name" (nonNull StringGraphType)

[<CLIMutable>]
type SomeType = {
    Testing: bool
}

[<Test>]
let ``Builder Field inferred field type without default value should be non nullable`` () =
    field {
        get (fun x -> x.Testing)
    }
    |> fieldEqual "Testing" (nonNull BooleanGraphType)

[<Test>]
let ``Builder Field inferred field type with default value should be nullable`` () =
    field {
        get (fun x -> x.Testing)
        defaultValue false
    }
    |> fieldEqual "Testing" (nullable BooleanGraphType)
