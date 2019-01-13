module GraphQL.FSharp.Builder.Object

open GraphQL.Types

let inline private set f (x: ObjectGraphType) = f x; x

type ObjectBuilder(?initial) =
    inherit BuilderMetadataBase<ObjectGraphType>(?initial = initial)

    [<CustomOperation "fields">]
    member __.Fields (object, fields) =
        set (fun x ->
            fields
            |> List.iter (x.AddField >> ignore)) object

    [<CustomOperation "implement">]
    member __.Implement (object, ``interface``) =
        set (fun x -> x.AddResolvedInterface ``interface``) object

let object = ObjectBuilder ()

let private objectWithName name =
    let object = ObjectGraphType ()
    object.Name <- name
    object

let query = objectWithName "Query"
let mutation = objectWithName "Mutation"
let subscription = objectWithName "Subscription"
