namespace GraphQL.FSharp.Utils

open System.Collections.Generic
open Newtonsoft.Json

module Dictionary =
    let merge (lhs: IDictionary<'a, 'b>) (rhs: IDictionary<'a, 'b>) =
        dict [
            yield! lhs |> Seq.map (|KeyValue|)
            yield! rhs |> Seq.map (|KeyValue|)
        ]

    // FIXME: This is a hacky way to do this right now.
    let ofObj x =
        try
            JsonConvert.SerializeObject x
            |> JsonConvert.DeserializeObject<Dictionary<string, obj>>
            |> Some
        with _ -> None
