module GraphQL.FSharp.BuilderDirective

open System.Collections.Generic
open System.Reflection
open GraphQL.Types

open GraphQL.FSharp.BuilderBase
open GraphQL.FSharp.Types
open GraphQL.FSharp.Utils

[<Literal>]
let DirectiveLocationsFieldName = "_directiveLocations"

let internal getDirectiveLocations (x: DirectiveGraphType) =
    box x
    |> typeof<DirectiveGraphType>
        .GetField(DirectiveLocationsFieldName, BindingFlags.NonPublic ||| BindingFlags.Instance)
        .GetValue
    :?> List<DirectiveLocation>

type DirectiveBuilderOperation = DirectiveGraphType -> DirectiveGraphType
type DirectiveBuilderState = DirectiveBuilderOperation list

type DirectiveBuilderBase () =
    inherit ConfigurableBuilder<DirectiveGraphType> ()

    member __.Yield (_: unit) = [] : DirectiveBuilderState

    [<CustomOperation "name">]
    member __.CustomOperation_Name (state: DirectiveBuilderState, name) =
        state
        |> operation (setName name)

    [<CustomOperation "description">]
    member __.CustomOperation_Description (state: DirectiveBuilderState, description) =
        state
        |> operation (setDescription description)

    [<CustomOperation "arguments">]
    member __.CustomOperation_Arguments (state: DirectiveBuilderState, arguments) =
        state
        |> operation (setArguments arguments)

    [<CustomOperation "locations">]
    member __.CustomOperation_Locations (state: DirectiveBuilderState, locations) =
        state
        |> unitOperation (fun this ->
            locations
            |> List.map (fun (x: DirectiveLocationUnion) -> x.GraphQLDirectiveLocation)
            |> List.toSeq
            |> (getDirectiveLocations this).AddRange
        )

type DirectiveBuilder (?value) =
    inherit DirectiveBuilderBase ()

    member __.Run (state: DirectiveBuilderState) =
        value
        |> Option.defaultValue (DirectiveGraphType ("", []))
        |> apply state

type DirectiveEditBuilder () =
    inherit DirectiveBuilderBase ()

    member __.Run (state: DirectiveBuilderState) = apply state
