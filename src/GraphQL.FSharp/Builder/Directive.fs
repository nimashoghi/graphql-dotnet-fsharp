module GraphQL.FSharp.BuilderDirective

open GraphQL.FSharp.BuilderBase
open GraphQL.FSharp.Types
open GraphQL.FSharp.Utils

type DirectiveBuilderBase () =
    inherit EntityBuilder<DirectiveGraphTypeEx> ()

    [<CustomOperation "arguments">]
    member __.CustomOperation_Arguments (state: State<DirectiveGraphTypeEx>, arguments) =
        state
        |> operation (setArguments arguments)

    [<CustomOperation "locations">]
    member __.CustomOperation_Locations (state: State<DirectiveGraphTypeEx>, locations) =
        state
        |> unitOperation (fun this ->
            locations
            |> List.map (fun (x: DirectiveLocationUnion) -> x.GraphQLDirectiveLocation)
            |> List.toSeq
            |> (getDirectiveLocations this).AddRange
        )

type DirectiveBuilder (?value) =
    inherit DirectiveBuilderBase ()

    member __.Run (state: State<DirectiveGraphTypeEx>) =
        value
        |> Option.defaultValue (DirectiveGraphTypeEx ())
        |> apply state

type DirectiveEditBuilder () =
    inherit DirectiveBuilderBase ()

    member __.Run (state: State<DirectiveGraphTypeEx>) = apply state
