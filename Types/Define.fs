[<AutoOpen>]
module GraphQL.FSharp.Types.Define

open GraphQL.Types
open Iris.Option.Builders

// TODO: Move this
[<AutoOpen>]
module Util =
    let optionList lst =
        match lst with
        | Some value -> value
        | None -> []

let inline setBasicProps name description deprecationReason (``type``: ^t) =
    (^t : (member Name: string) ``type``) <- name
    maybeUnit {
        let! description = description
        (^t : (member Description: string) ``type``) <- description
    }
    maybeUnit {
        let! deprecationReason = deprecationReason
        (^t : (member DeprecationReason: string) ``type``) <- deprecationReason
    }
    ``type``

type IGraphType with
    member this.Nullable
        with get () = this.GetMetadata<bool> ("Nullable", false)
        and set value = this.Metadata.["Nullable"] <- value

type Define private () = class end
type Convert private () = class end
