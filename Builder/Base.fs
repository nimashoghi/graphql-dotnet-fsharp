[<AutoOpen>]
module GraphQL.FSharp.Builder.Base

open System.Collections.Generic

[<AbstractClass>]
type BuilderBase< ^t when
            ^t : (new: unit -> ^t) and
            ^t : (member Name: string) and
            ^t : (member Description: string) and
            ^t : (member DeprecationReason: string) and
            ^t : (member Metadata: IDictionary<string, obj>)>(?initial: ^t) =
    member inline __.Yield _ =
        match initial with
        | Some value -> value
        | None -> new ^t()

    [<CustomOperation "name">]
    member inline __.Name (x: ^t, name) =
        (^t : (member Name: string) x) <- name

    [<CustomOperation "description">]
    member inline __.Description (x: ^t, description) =
        (^t : (member Description: string) x) <- description

    [<CustomOperation "deprecationReason">]
    member inline __.DeprecationReason (x: ^t, deprecationReason) =
        (^t : (member DeprecationReason: string) x) <- deprecationReason

    [<CustomOperation "metadata">]
    member inline __.Metadata (x: ^t, metadata: _ list) =
        (^t : (member Metadata: IDictionary<string, obj>) x) <- dict metadata
