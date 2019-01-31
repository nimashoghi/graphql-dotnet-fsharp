[<AutoOpen>]
module GraphQL.FSharp.Builder.Directive

open GraphQL.Types

open GraphQL.FSharp.Types
open GraphQL.FSharp.Utils

[<CLIMutable>]
type DirectiveBuilderState = {
    mutable Name: string
    mutable Description: string
    mutable Arguments: QueryArgument list
    mutable Locations: DirectiveLocationUnion list
}

let internal (|Nullable|) x =
    match box x with
    | null -> None
    | _ -> Some x

let inline private set f (x: DirectiveBuilderState) = f x; x

type DirectiveBuilder () =
    inherit BuilderBase<DirectiveBuilderState> ()

    [<CustomOperation "arguments">]
    member __.Arguments (x, arguments) = set (fun x -> x.Arguments <- arguments) x

    [<CustomOperation "locations">]
    member __.Locations (x, locations) = set (fun x -> x.Locations <- locations) x

    member __.Run
        {
            Name = Nullable name
            Description = Nullable description
            Arguments = Nullable arguments
            Locations = Nullable locations
        } =
        let name = Option.defaultValue "" name
        let locations =
            Option.defaultValue [] locations
            |> List.map (fun location -> location.GraphQLDirectiveLocation)
            |> List.toSeq

        let directive = DirectiveGraphType (name, locations)

        description |> Option.iter (fun description -> directive.Description <- description)
        arguments |> Option.iter (fun arguments -> directive.Arguments <- QueryArguments arguments)

        directive

let directive = DirectiveBuilder ()
