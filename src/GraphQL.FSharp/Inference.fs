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
