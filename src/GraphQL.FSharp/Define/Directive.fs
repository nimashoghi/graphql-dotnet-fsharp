[<AutoOpen>]
module GraphQL.FSharp.DefineDirective

open GraphQL.Types

open GraphQL.FSharp.Types
open GraphQL.FSharp.Utils

type Define with
    static member Directive (name, locations, ?description, ?arguments) =
        if isNull name || name = ""
        then invalidArg "name" "value cannot be null"

        if List.isEmpty locations
        then invalidArg "locations" "locations cannot be empty"

        let locations =
            locations
            |> List.map (fun (location: DirectiveLocationUnion) -> location.GraphQLDirectiveLocation)
            |> List.toSeq

        let arguments =
            arguments
            |> Option.map (List.toSeq >> QueryArguments)

        DirectiveGraphType (
            name,
            locations,
            Description = Option.toObj description,
            Arguments = Option.toObj arguments
        )
