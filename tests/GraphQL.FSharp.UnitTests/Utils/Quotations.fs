module GraphQL.FSharp.UnitTests.Utils.Quotations

open System.Threading.Tasks
open NUnit.Framework
open Swensen.Unquote
open FSharp.Utils.Tasks
open GraphQL.FSharp.Utils.Quotations

type Underlying () =
    member val Property: int Task = Task.FromResult 1 with get, set
    member __.Method () = Task.FromResult 1

[<Test>]
let ``method name test without task builder`` () =
    (|MethodName|_|) <@ fun (this: Underlying) -> this.Method () @> =! Some "Method"

[<Test>]
let ``property name test without task builder`` () =
    (|FieldName|_|) <@ fun (this: Underlying) -> this.Property @> =! Some "Property"

[<Test>]
let ``method name test with task builder`` () =
    (|MethodName|_|) <@ fun (this: Underlying) -> task { return! this.Method () } @> =! Some "Method"

[<Test>]
let ``property name test with task builder`` () =
    (|FieldName|_|) <@ fun (this: Underlying) -> task { return! this.Property } @> =! Some "Property"


[<Test>]
let ``method name test with vtask builder`` () =
    (|MethodName|_|) <@ fun (this: Underlying) -> vtask { return! this.Method () } @> =! Some "Method"

[<Test>]
let ``property name test with vtask builder`` () =
    (|FieldName|_|) <@ fun (this: Underlying) -> vtask { return! this.Property } @> =! Some "Property"
