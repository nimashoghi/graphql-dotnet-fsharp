module GraphQL.FSharp.Utils

open System.Text.RegularExpressions

let (|Regex|_|) pattern input =
    match Regex.Match (input, pattern) with
    | m when m.Success ->
        m.Groups
        |> Seq.cast<Group>
        |> Seq.toList
        |> List.map (fun group -> group.Value)
        |> List.tail
        |> Some
    | _ -> None

module Option =
    let ofBox (x: 't) =
        x
        |> box
        |> Option.ofObj
        |> Option.map unbox<'t>

    let ``or`` ``else`` x =
        match x with
        | Some value -> value
        | None -> ``else``

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
