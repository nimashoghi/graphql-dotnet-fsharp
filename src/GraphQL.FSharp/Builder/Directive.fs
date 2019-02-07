[<AutoOpen>]
module GraphQL.FSharp.Builder.Directive

open System.Collections.Generic
open System.Reflection
open GraphQL.Types

open GraphQL.FSharp.Builder.Base
open GraphQL.FSharp.Utils

let internal getDirectiveLocations (x: DirectiveGraphType) =
    box x
    |> typeof<DirectiveGraphType>.GetField("_directiveLocations", BindingFlags.NonPublic ||| BindingFlags.Instance).GetValue
    :?> List<DirectiveLocation>

let inline private set f (x: #DirectiveGraphType) = f x; x

type DirectiveGraphType with
    member this.Yield (_: unit) = ``yield`` this

    [<CustomOperation "name">]
    member __.CustomOperation_Name (this: DirectiveGraphType, name) =
        this |> setName name

    [<CustomOperation "description">]
    member __.CustomOperation_Description (this: DirectiveGraphType, description) =
        this |> setDescription description

    [<CustomOperation "arguments">]
    member __.CustomOperation_Arguments (this: DirectiveGraphType, arguments) =
        set (fun this -> this.Arguments <- arguments) this

    [<CustomOperation "locations">]
    member __.CustomOperation_Locations (this: DirectiveGraphType, locations) =
        set (fun this ->
            locations
            |> List.toSeq
            |> (getDirectiveLocations this).AddRange
        ) this

let directive = builder (fun () -> DirectiveGraphType ("", []))
