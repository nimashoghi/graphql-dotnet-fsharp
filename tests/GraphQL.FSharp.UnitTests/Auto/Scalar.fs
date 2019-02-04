module GraphQL.FSharp.UnitTests.Auto.Scalar

open NUnit.Framework
open Swensen.Unquote
open GraphQL.Language.AST

open GraphQL.FSharp

module ``basic PhoneNumber test`` =
    type PhoneNumber = PhoneNumber of string
    let PhoneNumberGraph = Auto.Scalar<PhoneNumber>

    [<Test>]
    let ``serialization test`` () =
        PhoneNumberGraph.Name =! "PhoneNumber"
        PhoneNumberGraph.ParseValue (box "12321321") =! (box <| PhoneNumber "12321321")
        PhoneNumberGraph.ParseLiteral (StringValue "12321321") =! (box <| PhoneNumber "12321321")
        PhoneNumberGraph.Serialize (box <| PhoneNumber "12321321") =! (box "12321321")
