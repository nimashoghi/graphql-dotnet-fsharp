namespace GraphQL.FSharp

open GraphQL.FSharp.AutoBase
open GraphQL.FSharp.Utils

type Argument<'t> =
    static member Get name ctx =
        getArgument typeof<'t> ctx name
        |> Option.map (fun x -> x :?> 't)
        |> Option.get

    static member GetDefault name defaultValue ctx =
        getArgument typeof<'t> ctx name
        |> Option.map (fun value -> value :?> 't)
        |> Option.defaultValue defaultValue
