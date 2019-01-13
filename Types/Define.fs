[<AutoOpen>]
module GraphQL.FSharp.Types.Define

open GraphQL.Types

// TODO: Move this
[<AutoOpen>]
module Util =
    let optionList lst =
        match lst with
        | Some value -> value
        | None -> []

let inline setBasicProps name description deprecationReason (``type``: ^t) =
    (^t : (member Name: string) ``type``) <- name
    Option.iter
        (fun description -> (^t : (member Description: string) ``type``) <- description)
        description
    Option.iter
        (fun deprecationReason -> (^t : (member DeprecationReason: string) ``type``) <- deprecationReason)
        deprecationReason
    ``type``

type IGraphType with
    member this.Nullable
        with get () = this.GetMetadata<bool> ("Nullable", false)
        and set value = this.Metadata.["Nullable"] <- value

type Define private () = class end
type Convert private () = class end
