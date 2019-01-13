[<AutoOpen>]
module GraphQL.FSharp.Builder.Interface

open GraphQL.Types

let inline private set f (x: InterfaceGraphType) = f x; x

type InterfaceBuilder() =
    inherit BuilderMetadataBase<InterfaceGraphType>()

    [<CustomOperation "fields">]
    member __.Fields (object, fields) =
        set (fun x ->
            fields
            |> List.iter (x.AddField >> ignore)) object

// TODO: rename?
let ``interface`` = InterfaceBuilder ()
