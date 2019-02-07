[<AutoOpen>]
module GraphQL.FSharp.Builder.Base

open System
open System.Collections.Generic
open System.Runtime.CompilerServices

// TODO: test this properly
[<AutoOpen>]
module Instance =
    (*
        let instanceDict<'t when 't: equality> =
            Dictionary<'t, unit -> 't>
                {
                    new IEqualityComparer<'t> with
                        member __.Equals (x, y) = Object.ReferenceEquals (x, y)
                        member __.GetHashCode x = RuntimeHelpers.GetHashCode x
                }

        let builder (f: unit -> 't) =
            let instance = f ()
            instanceDict<'t>.[instance] <- f
            instance

        let ``yield`` (x: 't) =
            match instanceDict<'t>.TryGetValue x with
            | true, f -> f ()
            | _ -> x
    *)

    let instanceDict =
        Dictionary<obj, unit -> obj>
            {
                new IEqualityComparer<obj> with
                    member __.Equals (x, y) = Object.ReferenceEquals (x, y)
                    member __.GetHashCode x = x.GetHashCode ()
            }

    let builder (f: unit -> 't) : 't =
        let instance = f ()
        instanceDict.[box instance] <- f >> box
        instance

    let ``yield`` (x: 't) =
        match instanceDict.TryGetValue x with
        | true, f ->
            match f () with
            | :? 't as value -> value
            | _ -> x
        | _ -> x

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
