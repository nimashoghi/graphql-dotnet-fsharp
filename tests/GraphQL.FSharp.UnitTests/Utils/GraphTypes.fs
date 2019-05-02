module GraphQL.FSharp.UnitTests.Utils.GraphTypes

open NUnit.Framework
open Swensen.Unquote
open GraphQL.FSharp.Utils.GraphTypes
open GraphQL.Types

module ``prettyPrintName`` =
    [<Test>]
    let ``basic`` () =
        prettyPrintName (IntGraphType ()) =! "Int"
        prettyPrintName (FloatGraphType ()) =! "Float"

    [<Test>]
    let ``basic reference`` () =
        prettyPrintName (GraphQLTypeReference "Int") =! "Int"
        prettyPrintName (GraphQLTypeReference "Float") =! "Float"

    [<Test>]
    let ``non null`` () =
        prettyPrintName (NonNullGraphType <| IntGraphType ()) =! "Int!"
        prettyPrintName (NonNullGraphType <| FloatGraphType ()) =! "Float!"

    [<Test>]
    let ``non null reference`` () =
        prettyPrintName (NonNullGraphType <| GraphQLTypeReference "Int") =! "Int!"
        prettyPrintName (NonNullGraphType <| GraphQLTypeReference "Float") =! "Float!"

    [<Test>]
    let ``basic list`` () =
        prettyPrintName (ListGraphType <| IntGraphType ()) =! "[Int]"
        prettyPrintName (ListGraphType <| FloatGraphType ()) =! "[Float]"

    [<Test>]
    let ``basic list reference`` () =
        prettyPrintName (ListGraphType <| GraphQLTypeReference "Int") =! "[Int]"
        prettyPrintName (ListGraphType <| GraphQLTypeReference "Float") =! "[Float]"

    [<Test>]
    let ``non null list of basic type`` () =
        prettyPrintName (NonNullGraphType <| (ListGraphType <| IntGraphType ())) =! "[Int]!"
        prettyPrintName (NonNullGraphType <| (ListGraphType <| FloatGraphType ())) =! "[Float]!"

    [<Test>]
    let ``non null list of basic type reference`` () =
        prettyPrintName (NonNullGraphType <| (ListGraphType <| GraphQLTypeReference "Int")) =! "[Int]!"
        prettyPrintName (NonNullGraphType <| (ListGraphType <| GraphQLTypeReference "Float")) =! "[Float]!"

    [<Test>]
    let ``non null list of non null type`` () =
        prettyPrintName (NonNullGraphType <| (ListGraphType <| (NonNullGraphType <| IntGraphType ()))) =! "[Int!]!"
        prettyPrintName (NonNullGraphType <| (ListGraphType <| (NonNullGraphType <| FloatGraphType ()))) =! "[Float!]!"

    [<Test>]
    let ``non null list of non null type reference`` () =
        prettyPrintName (NonNullGraphType <| (ListGraphType <| (NonNullGraphType <| GraphQLTypeReference "Int"))) =! "[Int!]!"
        prettyPrintName (NonNullGraphType <| (ListGraphType <| (NonNullGraphType <| GraphQLTypeReference "Float"))) =! "[Float!]!"

module ``prettyPrintNameDetailed`` =
    [<Test>]
    let ``basic`` () =
        prettyPrintNameDetailed (IntGraphType ()) =! "Int"
        prettyPrintNameDetailed (FloatGraphType ()) =! "Float"

    [<Test>]
    let ``basic reference`` () =
        prettyPrintNameDetailed (GraphQLTypeReference "Int") =! "(Int ref)"
        prettyPrintNameDetailed (GraphQLTypeReference "Float") =! "(Float ref)"

    [<Test>]
    let ``non null`` () =
        prettyPrintNameDetailed (NonNullGraphType <| IntGraphType ()) =! "Int!"
        prettyPrintNameDetailed (NonNullGraphType <| FloatGraphType ()) =! "Float!"

    [<Test>]
    let ``non null reference`` () =
        prettyPrintNameDetailed (NonNullGraphType <| GraphQLTypeReference "Int") =! "(Int ref)!"
        prettyPrintNameDetailed (NonNullGraphType <| GraphQLTypeReference "Float") =! "(Float ref)!"

    [<Test>]
    let ``basic list`` () =
        prettyPrintNameDetailed (ListGraphType <| IntGraphType ()) =! "[Int]"
        prettyPrintNameDetailed (ListGraphType <| FloatGraphType ()) =! "[Float]"

    [<Test>]
    let ``basic list reference`` () =
        prettyPrintNameDetailed (ListGraphType <| GraphQLTypeReference "Int") =! "[(Int ref)]"
        prettyPrintNameDetailed (ListGraphType <| GraphQLTypeReference "Float") =! "[(Float ref)]"

    [<Test>]
    let ``non null list of basic type`` () =
        prettyPrintNameDetailed (NonNullGraphType <| (ListGraphType <| IntGraphType ())) =! "[Int]!"
        prettyPrintNameDetailed (NonNullGraphType <| (ListGraphType <| FloatGraphType ())) =! "[Float]!"

    [<Test>]
    let ``non null list of basic type reference`` () =
        prettyPrintNameDetailed (NonNullGraphType <| (ListGraphType <| GraphQLTypeReference "Int")) =! "[(Int ref)]!"
        prettyPrintNameDetailed (NonNullGraphType <| (ListGraphType <| GraphQLTypeReference "Float")) =! "[(Float ref)]!"

    [<Test>]
    let ``non null list of non null type`` () =
        prettyPrintNameDetailed (NonNullGraphType <| (ListGraphType <| (NonNullGraphType <| IntGraphType ()))) =! "[Int!]!"
        prettyPrintNameDetailed (NonNullGraphType <| (ListGraphType <| (NonNullGraphType <| FloatGraphType ()))) =! "[Float!]!"

    [<Test>]
    let ``non null list of non null type reference`` () =
        prettyPrintNameDetailed (NonNullGraphType <| (ListGraphType <| (NonNullGraphType <| GraphQLTypeReference "Int"))) =! "[(Int ref)!]!"
        prettyPrintNameDetailed (NonNullGraphType <| (ListGraphType <| (NonNullGraphType <| GraphQLTypeReference "Float"))) =! "[(Float ref)!]!"
