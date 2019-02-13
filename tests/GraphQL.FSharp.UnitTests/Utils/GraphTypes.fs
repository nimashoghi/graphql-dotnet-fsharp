module GraphQL.FSharp.UnitTests.Utils.GraphTypes

open NUnit.Framework
open Swensen.Unquote
open GraphQL.FSharp.Utils.GraphTypes
open GraphQL.Types

module ``graphTypeName`` =
    [<Test>]
    let ``basic`` () =
        graphTypeName (IntGraphType ()) =! "Int"
        graphTypeName (FloatGraphType ()) =! "Float"

    [<Test>]
    let ``basic reference`` () =
        graphTypeName (GraphQLTypeReference "Int") =! "Int"
        graphTypeName (GraphQLTypeReference "Float") =! "Float"

    [<Test>]
    let ``non null`` () =
        graphTypeName (NonNullGraphType <| IntGraphType ()) =! "Int!"
        graphTypeName (NonNullGraphType <| FloatGraphType ()) =! "Float!"

    [<Test>]
    let ``non null reference`` () =
        graphTypeName (NonNullGraphType <| GraphQLTypeReference "Int") =! "Int!"
        graphTypeName (NonNullGraphType <| GraphQLTypeReference "Float") =! "Float!"

    [<Test>]
    let ``basic list`` () =
        graphTypeName (ListGraphType <| IntGraphType ()) =! "[Int]"
        graphTypeName (ListGraphType <| FloatGraphType ()) =! "[Float]"

    [<Test>]
    let ``basic list reference`` () =
        graphTypeName (ListGraphType <| GraphQLTypeReference "Int") =! "[Int]"
        graphTypeName (ListGraphType <| GraphQLTypeReference "Float") =! "[Float]"

    [<Test>]
    let ``non null list of basic type`` () =
        graphTypeName (NonNullGraphType <| (ListGraphType <| IntGraphType ())) =! "[Int]!"
        graphTypeName (NonNullGraphType <| (ListGraphType <| FloatGraphType ())) =! "[Float]!"

    [<Test>]
    let ``non null list of basic type reference`` () =
        graphTypeName (NonNullGraphType <| (ListGraphType <| GraphQLTypeReference "Int")) =! "[Int]!"
        graphTypeName (NonNullGraphType <| (ListGraphType <| GraphQLTypeReference "Float")) =! "[Float]!"

    [<Test>]
    let ``non null list of non null type`` () =
        graphTypeName (NonNullGraphType <| (ListGraphType <| (NonNullGraphType <| IntGraphType ()))) =! "[Int!]!"
        graphTypeName (NonNullGraphType <| (ListGraphType <| (NonNullGraphType <| FloatGraphType ()))) =! "[Float!]!"

    [<Test>]
    let ``non null list of non null type reference`` () =
        graphTypeName (NonNullGraphType <| (ListGraphType <| (NonNullGraphType <| GraphQLTypeReference "Int"))) =! "[Int!]!"
        graphTypeName (NonNullGraphType <| (ListGraphType <| (NonNullGraphType <| GraphQLTypeReference "Float"))) =! "[Float!]!"

module ``graphTypeNameDetailed`` =
    [<Test>]
    let ``basic`` () =
        graphTypeNameDetailed (IntGraphType ()) =! "Int"
        graphTypeNameDetailed (FloatGraphType ()) =! "Float"

    [<Test>]
    let ``basic reference`` () =
        graphTypeNameDetailed (GraphQLTypeReference "Int") =! "(Int ref)"
        graphTypeNameDetailed (GraphQLTypeReference "Float") =! "(Float ref)"

    [<Test>]
    let ``non null`` () =
        graphTypeNameDetailed (NonNullGraphType <| IntGraphType ()) =! "Int!"
        graphTypeNameDetailed (NonNullGraphType <| FloatGraphType ()) =! "Float!"

    [<Test>]
    let ``non null reference`` () =
        graphTypeNameDetailed (NonNullGraphType <| GraphQLTypeReference "Int") =! "(Int ref)!"
        graphTypeNameDetailed (NonNullGraphType <| GraphQLTypeReference "Float") =! "(Float ref)!"

    [<Test>]
    let ``basic list`` () =
        graphTypeNameDetailed (ListGraphType <| IntGraphType ()) =! "[Int]"
        graphTypeNameDetailed (ListGraphType <| FloatGraphType ()) =! "[Float]"

    [<Test>]
    let ``basic list reference`` () =
        graphTypeNameDetailed (ListGraphType <| GraphQLTypeReference "Int") =! "[(Int ref)]"
        graphTypeNameDetailed (ListGraphType <| GraphQLTypeReference "Float") =! "[(Float ref)]"

    [<Test>]
    let ``non null list of basic type`` () =
        graphTypeNameDetailed (NonNullGraphType <| (ListGraphType <| IntGraphType ())) =! "[Int]!"
        graphTypeNameDetailed (NonNullGraphType <| (ListGraphType <| FloatGraphType ())) =! "[Float]!"

    [<Test>]
    let ``non null list of basic type reference`` () =
        graphTypeNameDetailed (NonNullGraphType <| (ListGraphType <| GraphQLTypeReference "Int")) =! "[(Int ref)]!"
        graphTypeNameDetailed (NonNullGraphType <| (ListGraphType <| GraphQLTypeReference "Float")) =! "[(Float ref)]!"

    [<Test>]
    let ``non null list of non null type`` () =
        graphTypeNameDetailed (NonNullGraphType <| (ListGraphType <| (NonNullGraphType <| IntGraphType ()))) =! "[Int!]!"
        graphTypeNameDetailed (NonNullGraphType <| (ListGraphType <| (NonNullGraphType <| FloatGraphType ()))) =! "[Float!]!"

    [<Test>]
    let ``non null list of non null type reference`` () =
        graphTypeNameDetailed (NonNullGraphType <| (ListGraphType <| (NonNullGraphType <| GraphQLTypeReference "Int"))) =! "[(Int ref)!]!"
        graphTypeNameDetailed (NonNullGraphType <| (ListGraphType <| (NonNullGraphType <| GraphQLTypeReference "Float"))) =! "[(Float ref)!]!"
