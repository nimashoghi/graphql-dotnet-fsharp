module GraphQL.FSharp.BuilderBase

open System.Collections.Generic
open GraphQL.Types

open GraphQL.FSharp.BuilderUtils
open GraphQL.FSharp.Types
open GraphQL.FSharp.Utils

let inline setName value (x: ^t) =
    (^t: (member set_Name: string -> unit) x, value)
    x

let inline setDescription value (x: ^t) =
    (^t: (member set_Description: string -> unit) x, value)
    x

let inline setDeprecationReason value (x: ^t) =
    (^t: (member set_DeprecationReason: string -> unit) x, value)
    x

let inline setDefaultValue value (x: ^t) =
    (^t: (member set_DefaultValue: obj -> unit) x, value)
    x

let inline setArguments value (x: ^t) =
    (^t: (member set_Arguments: QueryArguments -> unit) x, value)
    x

let inline setGraphType value (x: ^t) =
    (^t: (member set_GraphType: IGraphType -> unit) x, value)
    x

let inline setMetadata value (x: ^t) =
    let metadata = (^t: (member Metadata: IDictionary<string, obj>) x)
    (^t: (member set_Metadata: IDictionary<string, obj> -> unit) x, Dictionary.merge metadata value)
    x

type ConfigureBuilder<'t> () =
    [<CustomOperation "configure">]
    member inline __.Configure (state: 't, f) =
        f state

    member inline __.Configure (state: 't, f) =
        f state
        state

type EntityBuilder<'t when
                  't: (member set_Name: string -> unit) and
                  't: (member set_Description: string -> unit) and
                  't: (member Metadata: IDictionary<string, obj>) and
                  't: (member set_Metadata: IDictionary<string, obj> -> unit)> () =
    inherit ConfigureBuilder<'t> ()

    [<CustomOperation "name">]
    member inline __.Name (state: 't, name) =
        setName name state

    [<CustomOperation "description">]
    member inline __.Description (state: 't, description) =
        setDescription description state

    [<CustomOperation "metadata">]
    member inline __.Metadata (state: 't, metadata) =
        setMetadata metadata state

let inline handleNonNullTypes (x: ^t) =
    if not <| isNull (^t: (member DefaultValue: obj) x) then
        match Option.ofObj (^t: (member ResolvedType: IGraphType) x) with
        | Some graph when (graph :? NonNullGraphType) ->
            let nonNull = graph :?> NonNullGraphType
            (^t: (member set_ResolvedType: IGraphType -> unit) x, nonNull.ResolvedType)
        | _ -> ()
    x

type TypedEntityBuilder<'t when
                       't: (member set_Name: string -> unit) and
                       't: (member set_Description: string -> unit) and
                       't: (member Metadata: IDictionary<string, obj>) and
                       't: (member set_Metadata: IDictionary<string, obj> -> unit) and
                       't: (member DefaultValue: obj) and
                       't: (member set_DefaultValue: obj -> unit) and
                       't: (member GraphType: IGraphType) and
                       't: (member set_GraphType: IGraphType -> unit) and
                       't: (member ResolvedType: IGraphType) and
                       't: (member set_ResolvedType: IGraphType -> unit)> () =
    inherit EntityBuilder<'t> ()

    [<CustomOperation "defaultValue">]
    member inline __.DefaultValue (state: 't, value: 'value) =
        setDefaultValue value state
        |> setType typeof<'value>

    [<CustomOperation "type">]
    member inline __.Type (state: 't, ``type``) =
        setGraphType ``type`` state

    member inline __.Run (state: 't) = handleNonNullTypes state

type BasicGraphTypeBuilder<'t when
                          't: (member set_Name: string -> unit) and
                          't: (member set_Description: string -> unit) and
                          't: (member Metadata: IDictionary<string, obj>) and
                          't: (member set_Metadata: IDictionary<string, obj> -> unit) and
                          't: (member set_DeprecationReason: string -> unit)> () =
    inherit EntityBuilder<'t> ()

    [<CustomOperation "deprecationReason">]
    member inline __.DeprecationReason (state: 't, deprecationReason) =
        setDeprecationReason deprecationReason state

type TypedFieldBuilder<'t when
                      't: (member set_Name: string -> unit) and
                      't: (member set_Description: string -> unit) and
                      't: (member Metadata: IDictionary<string, obj>) and
                      't: (member set_Metadata: IDictionary<string, obj> -> unit) and
                      't: (member DefaultValue: obj) and
                      't: (member set_DefaultValue: obj -> unit) and
                      't: (member GraphType: IGraphType) and
                      't: (member set_GraphType: IGraphType -> unit) and
                      't: (member ResolvedType: IGraphType) and
                      't: (member set_ResolvedType: IGraphType -> unit) and
                      't: (member set_DeprecationReason: string -> unit)> () =
    inherit TypedEntityBuilder<'t> ()

    [<CustomOperation "deprecationReason">]
    member inline __.DeprecationReason (state: 't, deprecationReason) =
        setDeprecationReason deprecationReason state

type ComplexGraphTypeBuilder<'t, 'source when
                            't :> ComplexGraphType<'source> and
                            't: (member set_Name: string -> unit) and
                            't: (member set_Description: string -> unit) and
                            't: (member Metadata: IDictionary<string, obj>) and
                            't: (member set_Metadata: IDictionary<string, obj> -> unit) and
                            't: (member set_DeprecationReason: string -> unit)> () =
    inherit BasicGraphTypeBuilder<'t> ()

    [<CustomOperation "fields">]
    member inline __.Fields (state: 't, fields: Field<'source> list) =
        fields
        |> List.iter (state.AddField >> ignore)

        state
