module GraphQL.FSharp.UnitTests.Inference

open System
open NUnit.Framework
open Swensen.Unquote
open GraphQL.FSharp.Inference
open GraphQL.Types

module ``getDefaultType`` =
    [<Test>]
    let ``already exists test`` () =
        [
            typeof<string>, StringGraphType () :> ScalarGraphType
            typeof<bool>, BooleanGraphType () :> ScalarGraphType
            typeof<float>, FloatGraphType () :> ScalarGraphType
            typeof<int>, IntGraphType () :> ScalarGraphType
            typeof<Guid>, IdGraphType () :> ScalarGraphType
            typeof<DateTimeOffset>, DateTimeOffsetGraphType () :> ScalarGraphType
        ]
        |> List.iter (
            fun (systemType, resolvedType) ->
                getDefaultType systemType =! Some resolvedType
        )

    type MyType = {Name: string}

    [<Test>]
    let ``does not exist test`` () =
        getDefaultType typeof<MyType> =! None

module ``getDefaultTypeOrReference`` =

    [<Test>]
    let ``already exists test`` () =
        [
            typeof<string>, StringGraphType () :> IGraphType
            typeof<bool>, BooleanGraphType () :> IGraphType
            typeof<float>, FloatGraphType () :> IGraphType
            typeof<int>, IntGraphType () :> IGraphType
            typeof<Guid>, IdGraphType () :> IGraphType
            typeof<DateTimeOffset>, DateTimeOffsetGraphType () :> IGraphType
        ]
        |> List.iter (
            fun (systemType, resolvedType) ->
                getDefaultTypeOrReference systemType =! resolvedType
        )

    type MyType = {Name: string}

    [<Test>]
    let ``does not exist test`` () =
        getDefaultTypeOrReference typeof<MyType> =! upcast GraphQLTypeReference "MyType"
