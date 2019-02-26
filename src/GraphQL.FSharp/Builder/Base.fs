module GraphQL.FSharp.BuilderBase

open System.Collections.Generic
open GraphQL.Types

open GraphQL.FSharp.Inference
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

let isInvalidType (``type``: IGraphType) = isNull ``type`` || Object.ReferenceEquals (``type``, invalidGraphType)

let inline setType systemType (source: ^t) =
    let resolvedType = (^t: (member ResolvedType: IGraphType) source)
    let setResolvedType ``type`` = (^t: (member set_ResolvedType: IGraphType -> unit) (source, ``type``))
    if isInvalidType resolvedType then setResolvedType (createReference systemType)
    source

let inline setMetadata value (x: ^t) =
    let metadata = (^t: (member Metadata: IDictionary<string, obj>) x)
    (^t: (member set_Metadata: IDictionary<string, obj> -> unit) x, Dictionary.merge metadata value)
    x

let inline handleNonNullTypes (x: ^t) =
    if not <| isNull (^t: (member DefaultValue: obj) x) then
        match Option.ofObj (^t: (member ResolvedType: IGraphType) x) with
        | Some graph when (graph :? NonNullGraphType) ->
            let nonNull = graph :?> NonNullGraphType
            (^t: (member set_ResolvedType: IGraphType -> unit) x, nonNull.ResolvedType)
        | _ -> ()
    x

type Operation<'t> = 't -> 't
type State<'t> = Operation<'t> list

let inline runState (state: State<'t>) =
    List.foldBack (fun f this -> f this) state (new 't ())

let inline appendToState (state: State<'t>) (f: 't -> unit) =
    (fun x -> f x; x) :: state

type StateBuilder<'t when 't: (new: unit -> 't)> () =
    member inline __.Run (state: State<'t>) =
        runState state

type ConfigureBuilder<'t when 't: (new: unit -> 't)> () =
    inherit StateBuilder<'t> ()

    [<CustomOperation "configure">]
    member inline __.Configure (state: State<'t>, f): State<'t> =
        f :: state

    member inline __.Configure (state: State<'t>, f): State<'t> =
        (fun arg -> f arg; arg) :: state

type EntityBuilder<'t when
                  't: (new: unit -> 't) and
                  't: (member set_Name: string -> unit) and
                  't: (member set_Description: string -> unit) and
                  't: (member Metadata: IDictionary<string, obj>) and
                  't: (member set_Metadata: IDictionary<string, obj> -> unit)> () =
    inherit ConfigureBuilder<'t> ()

    [<CustomOperation "name">]
    member inline __.Name (state: State<'t>, name) =
        setName name :: state

    [<CustomOperation "description">]
    member inline __.Description (state: State<'t>, description) =
        setDescription description :: state

    [<CustomOperation "metadata">]
    member inline __.Metadata (state: State<'t>, metadata) =
        setMetadata metadata ::  state

type TypedEntityBuilder<'t when
                       't: (new: unit -> 't) and
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
    member inline __.DefaultValue (state: State<'t>, value: 'value) =
        setType typeof<'value> :: setDefaultValue value :: state

    [<CustomOperation "type">]
    member inline __.Type (state: State<'t>, ``type``) =
        setGraphType ``type`` :: state

    member inline __.Run (state: State<'t>) =
        handleNonNullTypes :: state
        |> runState

type BasicGraphTypeBuilder<'t when
                          't: (new: unit -> 't) and
                          't: (member set_Name: string -> unit) and
                          't: (member set_Description: string -> unit) and
                          't: (member Metadata: IDictionary<string, obj>) and
                          't: (member set_Metadata: IDictionary<string, obj> -> unit) and
                          't: (member set_DeprecationReason: string -> unit)> () =
    inherit EntityBuilder<'t> ()

    [<CustomOperation "deprecate">]
    member inline __.DeprecationReason (state: State<'t>, deprecationReason) =
        setDeprecationReason deprecationReason :: state

type TypedFieldBuilder<'t when
                      't: (new: unit -> 't) and
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

    [<CustomOperation "deprecate">]
    member inline __.DeprecationReason (state: State<'t>, deprecationReason) =
        setDeprecationReason deprecationReason :: state

type ComplexGraphTypeBuilder<'t, 'source when
                            't: (new: unit -> 't) and
                            't :> ComplexGraphType<'source> and
                            't: (member set_Name: string -> unit) and
                            't: (member set_Description: string -> unit) and
                            't: (member Metadata: IDictionary<string, obj>) and
                            't: (member set_Metadata: IDictionary<string, obj> -> unit) and
                            't: (member set_DeprecationReason: string -> unit)> () =
    inherit BasicGraphTypeBuilder<'t> ()

    [<CustomOperation "fields">]
    member inline __.Fields (state: State<'t>, fields: Field<'source> list) =
        let operation (this: 't) =
            fields
            |> List.iter (this.AddField >> ignore)

            this

        operation :: state
