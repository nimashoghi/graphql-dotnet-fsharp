[<AutoOpen>]
module GraphQL.FSharp.DefineDirective

open GraphQL.Types

open GraphQL.FSharp.Types
open GraphQL.FSharp.Utils

type Define with
    static member Directive (name, locations, ?description, ?arguments) =
        let directive =
            DirectiveGraphType (
                name,
                locations
                |> List.map (fun (location: DirectiveLocationUnion) -> location.GraphQLDirectiveLocation)
                |> List.toSeq
            )

        description
        |> Option.iter (fun description -> directive.Description <- description)

        arguments
        |> Option.map List.toSeq
        |> Option.iter (fun arguments -> directive.Arguments <- QueryArguments arguments)

        directive