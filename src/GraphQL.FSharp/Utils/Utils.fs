namespace GraphQL.FSharp.Utils

open System.Collections.Generic

module Option =
    let ofBox (x: 't) =
        x
        |> box
        |> Option.ofObj
        |> Option.map unbox<'t>

    let toBox x =
        x
        |> Option.map box
        |> Option.defaultValue (box null)

module Array =
    let some array =
        array
        |> Array.filter Option.isSome
        |> Array.map Option.get

module List =
    let some list =
        list
        |> List.filter Option.isSome
        |> List.map Option.get

module Seq =
    let some seq =
        seq
        |> Seq.filter Option.isSome
        |> Seq.map Option.get

module Dictionary =
    let merge (lhs: IDictionary<'a, 'b>) (rhs: IDictionary<'a, 'b>) =
        dict [
            yield! lhs |> Seq.map (|KeyValue|)
            yield! rhs |> Seq.map (|KeyValue|)
        ]
