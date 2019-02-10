module GraphQL.FSharp.BuilderDirective

open System.Collections.Generic
open System.Reflection
open GraphQL.Types

open GraphQL.FSharp.BuilderBase
open GraphQL.FSharp.Utils

let inline private set f (x: #DirectiveGraphType) = f x; x

let internal getDirectiveLocations (x: DirectiveGraphType) =
    box x
    |> typeof<DirectiveGraphType>.GetField("_directiveLocations", BindingFlags.NonPublic ||| BindingFlags.Instance).GetValue
    :?> List<DirectiveLocation>

type DirectiveBuilder () =
    member __.Yield (_: unit) = DirectiveGraphType ("", [])

    [<CustomOperation "name">]
    member __.CustomOperation_Name (this: DirectiveGraphType, name) =
        setName name this

    [<CustomOperation "description">]
    member __.CustomOperation_Description (this: DirectiveGraphType, description) =
        setDescription description this

    [<CustomOperation "arguments">]
    member __.CustomOperation_Arguments (this: DirectiveGraphType, arguments) =
        setArguments arguments this

    [<CustomOperation "locations">]
    member __.CustomOperation_Locations (this: DirectiveGraphType, locations) =
        set (fun this ->
            locations
            |> List.toSeq
            |> (getDirectiveLocations this).AddRange
        ) this
