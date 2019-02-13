module GraphQL.FSharp.UnitTests.Builder.Main

open System
open NUnit.Framework
open Swensen.Unquote
open GraphQL.FSharp.TestUtils.Assert
open GraphQL.FSharp.Builder
open GraphQL.FSharp.Types
open GraphQL.Types

let testReference x y = Object.ReferenceEquals (x, y) =! true

module ``edit`` =
    [<Test>]
    let ``argument`` () =
        let old = TypedQueryArgument<int> ()
        let arg =
            old
            |> Edit.argument {
                name "Sup"
                ``type`` IntGraphType
            }
        arg
        |> argumentEqual "Sup" (nullable IntGraphType) None
        testReference old arg

    [<Test>]
    let ``directive`` () =
        let old = DirectiveGraphType ("", [])
        let directive =
            old
            |> Edit.directive {
                name "newName"
                locations [ArgumentDefinition]
            }
        testReference old directive
        directive.Name =! "newName"
        (Seq.toList directive.Locations) =! [ArgumentDefinition.GraphQLDirectiveLocation]
