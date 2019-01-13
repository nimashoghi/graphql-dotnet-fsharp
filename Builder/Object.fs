module GraphQL.FSharp.Builder.Object

open GraphQL.Types

let inline private set f (x: ObjectGraphType) = f x; x

type ObjectBuilder(?name) =
    inherit BuilderMetadataBase<ObjectGraphType>()

    [<CustomOperation "fields">]
    member __.Fields (object, fields) =
        set (fun x ->
            fields
            |> List.iter (x.AddField >> ignore)) object

    [<CustomOperation "implement">]
    member __.Implement (object, ``interface``) =
        set (fun x -> x.AddResolvedInterface ``interface``) object

    member __.Run argument =
        match name with
        | Some name -> set (fun x -> x.Name <- name) argument
        | None -> argument

let object = ObjectBuilder ()
let query = ObjectBuilder "Query"
let mutation = ObjectBuilder "Mutation"
let subscription = ObjectBuilder "Subscription"
