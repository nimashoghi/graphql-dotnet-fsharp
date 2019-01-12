module GraphQL.FSharp.Builder.Object

open GraphQL.Types

let inline private set f (x: ObjectGraphType) = f x; x

// TODO: add DeprecationReason
type ObjectBuilder() =
    inherit BuilderBase<ObjectGraphType>()

    [<CustomOperation "fields">]
    member __.Fields (object, fields) = set (fun x -> fields |> List.iter (x.AddField >> ignore)) object

    [<CustomOperation "implement">]
    member __.Implement (object, ``interface``) = set (fun x -> x.AddResolvedInterface ``interface``) object

let inline private set f (x: InterfaceGraphType) = f x; x

type InterfaceBuilder() =
    inherit BuilderBase<InterfaceGraphType>()

    [<CustomOperation "fields">]
    member __.Fields (object, fields) = set (fun x -> fields |> List.iter (x.AddField >> ignore)) object

let inline private set f (x: InputObjectGraphType) = f x; x

type InputObjectBuilder() =
    inherit BuilderBase<InputObjectGraphType>()

    [<CustomOperation "fields">]
    member __.Fields (object, fields) = set (fun x -> fields |> List.iter (x.AddField >> ignore)) object
