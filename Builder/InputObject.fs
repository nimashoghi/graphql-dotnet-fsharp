[<AutoOpen>]
module GraphQL.FSharp.Builder.InputObject

open GraphQL.Types

let inline private set f (x: InputObjectGraphType) = f x; x

type InputObjectBuilder() =
    inherit BuilderMetadataBase<InputObjectGraphType>()

    [<CustomOperation "fields">]
    member __.Fields (object, fields) =
        set (fun x ->
            fields
            |> List.iter (x.AddField >> ignore)) object

let input = InputObjectBuilder ()
