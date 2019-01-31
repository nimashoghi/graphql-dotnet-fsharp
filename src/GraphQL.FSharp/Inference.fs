module GraphQL.FSharp.Inference

open System
open GraphQL.Types

open GraphQL.FSharp.Utils.Type
open GraphQL.FSharp.Registry

let rec infer checkNullability get (``type``: Type) =
    let graphType, isNull =
        match ``type`` with
        | Nullable ``type``
        | Option ``type`` ->
            infer false get ``type``, true
        | Enumerable ``type`` ->
            (infer checkNullability get ``type``
            |> ListGraphType
            :> IGraphType), false
        | Observable ``type``
        | Task ``type``
        | ``type`` -> (get ``type`` |> Option.toObj), false

    if checkNullability && not isNull
    then NonNullGraphType graphType :> IGraphType
    else graphType

let inferObject ``type`` = infer false Object.get ``type``
let inferInput ``type`` = infer false InputObject.get ``type``

let inferObjectNull ``type`` = infer true Object.get ``type``
let inferInputNull ``type`` = infer true InputObject.get ``type``

let inferObjectConfigure ``type`` checkNullability = infer checkNullability Object.get ``type``


let graphType (f: unit -> #IGraphType) = f >> (fun graph -> graph :> IGraphType)

let defaultTypes = dict [
    typeof<string>, graphType StringGraphType
    typeof<bool>, graphType BooleanGraphType
    typeof<float>, graphType FloatGraphType
    typeof<float32>, graphType FloatGraphType
    typeof<decimal>, graphType DecimalGraphType
    typeof<int>, graphType IntGraphType
    typeof<int8>, graphType IntGraphType
    typeof<int16>, graphType IntGraphType
    typeof<int32>, graphType IntGraphType
    typeof<int64>, graphType IntGraphType
    typeof<uint8>, graphType IntGraphType
    typeof<uint16>, graphType IntGraphType
    typeof<uint32>, graphType IntGraphType
    typeof<uint64>, graphType IntGraphType
    typeof<Guid>, graphType IdGraphType
    // typeof<Date>, graphType DateGraphType
    typeof<DateTime>, graphType DateTimeGraphType
    typeof<DateTimeOffset>, graphType DateTimeOffsetGraphType
    // typeof<TimeSpan>, graphType TimeSpanSecondsGraphType
    typeof<TimeSpan>, graphType TimeSpanMillisecondsGraphType
]

let getDefaultType ``type`` =
    match defaultTypes.TryGetValue ``type`` with
    | true, f -> Some (f ())
    | false, _ -> None

let rec createReference' checkNullability get (``type``: Type) =
    let graphType, isNull =
        match ``type`` with
        | Nullable innerType
        | Option innerType ->
            let ``type`` = createReference' false get innerType
            ``type``, true

        | Enumerable innerType ->
            let ``type`` =
                createReference' checkNullability get innerType
                |> ListGraphType
                :> IGraphType
            ``type``, false

        | Observable innerType
        | Task innerType
        | innerType ->
            let ``type`` =
                get innerType
                |> Option.toObj
            ``type``, false

    if checkNullability && not isNull
    then NonNullGraphType graphType :> IGraphType
    else graphType

let createReference ``type`` = createReference' true (fun t -> getDefaultType t |> Option.defaultValue (GraphQLTypeReference t.Name :> IGraphType) |> Some) ``type``
