module GraphQL.FSharp.UnitTests.Util.Quotations

open NUnit.Framework
open Swensen.Unquote
open FSharp.Quotations
open GraphQL.FSharp.Utils.Quotations

module ``ValueWithType`` =
    // TODO: Add to tests
    type MyType () =
        member __.DoSomething ([<ReflectedDefinition true>] expr: Expr<int>) =
            match expr with
            | WithValueTyped (value, expr) -> value, expr
            | _ -> failwith "Could not get value!"

        member __.DoSomethingInvalid ([<ReflectedDefinition false>] expr: Expr<int>) =
            match expr with
            | WithValueTyped (value, expr) -> value, expr
            | _ -> failwith "Could not get value!"

    [<Test>]
    let ``valid method should get a result`` () =
        MyType().DoSomething 1 =! (1, <@ 1 @>)

    [<Test>]
    let ``invalid method should not get a result`` () =
        raises<exn> <@ MyType().DoSomethingInvalid 1 @>
