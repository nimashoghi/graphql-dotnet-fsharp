module GraphQL.FSharp.BuilderBase

open System.Collections.Generic
open GraphQL.Types

open GraphQL.FSharp.Utils

let inline setName value (x: ^t) =
    (^t : (member set_Name: string -> unit) x, value)
    x

let inline setDescription value (x: ^t) =
    (^t : (member set_Description: string -> unit) x, value)
    x

let inline setDeprecationReason value (x: ^t) =
    (^t : (member set_DeprecationReason: string -> unit) x, value)
    x

let inline setDefaultValue value (x: ^t) =
    (^t : (member set_DefaultValue: obj -> unit) x, value)
    x

let inline setArguments value (x: ^t) =
    (^t : (member set_Arguments: QueryArguments -> unit) x, value)
    x

let inline setResolvedType value (x: ^t) =
    (^t : (member set_ResolvedType: IGraphType -> unit) x, value)
    x

let inline setMetadata value (x: ^t) =
    let metadata = (^t : (member Metadata: IDictionary<string, obj>) x)
    (^t : (member set_Metadata: IDictionary<string, obj> -> unit) x, Dictionary.merge metadata value)
    x
