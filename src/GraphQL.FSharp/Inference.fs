module GraphQL.FSharp.Inference

open System
open GraphQL.Types

open GraphQL.FSharp.Utils

let rec unwrapType nonNullDefault get ``type`` =
    let graphType, isNull =
        match ``type`` with
        | Nullable innerType
        | Result innerType // TODO: Check result and null-ness
        | Option innerType ->
            let ``type`` = unwrapType false get innerType
            ``type``, true

        | Enumerable innerType ->
            let ``type`` =
                unwrapType nonNullDefault get innerType
                |> ListGraphType
                :> IGraphType
            ``type``, false

        | Observable innerType
        | Task innerType ->
            let ``type`` = unwrapType nonNullDefault get innerType
            ``type``, true

        | innerType ->
            let ``type`` =
                get innerType
            ``type``, false

    if nonNullDefault && not isNull
    then NonNullGraphType graphType :> IGraphType
    else graphType

let private graphType (f: unit -> #ScalarGraphType) = f >> (fun graph -> graph :> ScalarGraphType)

let defaultTypes = dict [
    typeof<string>, graphType StringGraphType
    typeof<bool>, graphType BooleanGraphType
    typeof<float>, graphType FloatGraphType
    typeof<float32>, graphType FloatGraphType
    typeof<decimal>, graphType DecimalGraphType
    typeof<int8>, graphType IntGraphType
    typeof<int16>, graphType IntGraphType
    typeof<int32>, graphType IntGraphType
    typeof<int64>, graphType IntGraphType
    typeof<uint8>, graphType IntGraphType
    typeof<uint16>, graphType IntGraphType
    typeof<uint32>, graphType IntGraphType
    typeof<uint64>, graphType IntGraphType
    typeof<Guid>, graphType IdGraphType
    typeof<DateTime>, graphType DateTimeGraphType
    typeof<DateTimeOffset>, graphType DateTimeOffsetGraphType
    typeof<TimeSpan>, graphType TimeSpanMillisecondsGraphType
]

let getDefaultType ``type`` =
    match defaultTypes.TryGetValue ``type`` with
    | true, f -> Some (f ())
    | false, _ -> None

let getDefaultTypeOrReference ``type`` =
    getDefaultType ``type``
    |> Option.map (fun ``type`` -> ``type`` :> IGraphType)
    |> Option.defaultValue (GraphQLTypeReference ``type``.Name :> IGraphType)

let (|UnwrappedNonNull|)  ``type`` = unwrapType true getDefaultTypeOrReference ``type``

let createReference ``type`` = unwrapType true getDefaultTypeOrReference ``type``
