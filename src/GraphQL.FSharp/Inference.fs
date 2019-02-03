module GraphQL.FSharp.Inference

open System
open GraphQL.Types

open GraphQL.FSharp.Utils.Type

let rec unwrapType checkNullability graphTypeGetter ``type`` =
    let graphType, isNull =
        match ``type`` with
        | Nullable innerType
        | Option innerType
        | Result innerType  ->
            let ``type`` = unwrapType false graphTypeGetter innerType
            ``type``, true

        | Enumerable innerType ->
            let ``type`` =
                unwrapType checkNullability graphTypeGetter innerType
                |> ListGraphType
                :> IGraphType
            ``type``, false

        | Observable innerType
        | Task innerType ->
            let ``type`` = unwrapType checkNullability graphTypeGetter innerType
            ``type``, true

        | innerType ->
            let ``type`` =
                graphTypeGetter innerType
                |> Option.toObj
            ``type``, false

    if checkNullability && not isNull
    then NonNullGraphType graphType :> IGraphType
    else graphType

let private graphType (f: unit -> #IGraphType) = f >> (fun graph -> graph :> IGraphType)

let defaultTypes = dict [
    typeof<string>, graphType StringGraphType
    typeof<bool>, graphType BooleanGraphType
    typeof<float>, graphType FloatGraphType
    typeof<float32>, graphType FloatGraphType
    typeof<decimal>, graphType DecimalGraphType
    typeof<int8>, graphType IntGraphType
    typeof<int16>, graphType ShortGraphType
    typeof<int32>, graphType IntGraphType
    typeof<int64>, graphType ULongGraphType
    typeof<uint8>, graphType UIntGraphType
    typeof<uint16>, graphType UShortGraphType
    typeof<uint32>, graphType UIntGraphType
    typeof<uint64>, graphType ULongGraphType
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
    |> Option.defaultValue (GraphQLTypeReference ``type``.Name :> IGraphType)
    |> Some

let createReference ``type`` = unwrapType true getDefaultTypeOrReference ``type``
let createReferenceNoChecks ``type`` = unwrapType false getDefaultTypeOrReference ``type``
let createReferenceConfigure ``type`` checkNullability = unwrapType checkNullability getDefaultTypeOrReference ``type``
