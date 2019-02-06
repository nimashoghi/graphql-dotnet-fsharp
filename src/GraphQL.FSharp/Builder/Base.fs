[<AutoOpen>]
module GraphQL.FSharp.Builder.Base

open System.Collections.Generic

let inline setName value (x: ^t) =
    (^t : (member set_Name: string -> unit) x, value)
    x

let inline setDescription value (x: ^t) =
    (^t : (member set_Description: string -> unit) x, value)
    x

let inline setDeprecationReason value (x: ^t) =
    (^t : (member set_DeprecationReason: string -> unit) x, value)
    x

let merge (lhs: IDictionary<'a, 'b>) (rhs: IDictionary<'a, 'b>) =
    dict [
        for pair in lhs do yield pair.Key, pair.Value
        for pair in rhs do yield pair.Key, pair.Value
    ]

let inline setMetadata value (x: ^t) =
    let metadata = (^t : (member Metadata: IDictionary<string, obj>) x)
    (^t : (member set_Metadata: IDictionary<string, obj> -> unit) x, merge metadata value)
    x
