module GraphQL.FSharp.Builder.Object

open System
open FSharp.Reflection
open GraphQL.Resolvers
open GraphQL.Types
open Iris.Option.Builders

open GraphQL.FSharp.Model
open GraphQL.FSharp.Builder.Helpers

let inline private set f (x: ObjectGraphType) = f x; x

// TODO: add DeprecationReason
type ObjectBuilder() =
    member __.Yield _ = ObjectGraphType ()

    [<CustomOperation "name">]
    member __.Name (object, name) = set (fun x -> x.Name <- name) object

    [<CustomOperation "description">]
    member __.Description (object, description) = set (fun x -> x.Description <- description) object

    [<CustomOperation "fields">]
    member __.Fields (object, fields) = set (fun x -> fields |> List.iter (x.AddField >> ignore)) object

    [<CustomOperation "implement">]
    member __.Implement (object, ``interface``) = set (fun x -> x.AddResolvedInterface ``interface``) object
