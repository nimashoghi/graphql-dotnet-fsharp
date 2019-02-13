module GraphQL.FSharp.BuilderBase

open System.Collections.Generic
open GraphQL.Types

open GraphQL.FSharp.Types
open GraphQL.FSharp.Utils

let inline operation (f: 'a -> 'a) (lst: ('a -> 'a) list) = f :: lst
let unitOperation (f: 'a -> unit) (lst: ('a -> 'a) list) = (fun x -> f x; x) :: lst
let apply (lst: ('a -> 'a) list) x =
    lst
    |> List.rev
    |> List.fold (fun state f -> f state) x

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

type Operation<'t> = 't -> 't
type State<'t> = Operation<'t> list

type ConfigureBuilder<'t> () =
    [<CustomOperation "configure">]
    member inline __.Configure (state: State<'t>, f) =
        state
        |> operation f

    member inline __.Configure (state: State<'t>, f) =
        state
        |> operation (fun x -> f x; x)

type EntityBuilder<'t when
                         't: (member set_Name: string -> unit) and
                         't: (member set_Description: string -> unit) and
                         't: (member Metadata: IDictionary<string, obj>) and
                         't: (member set_Metadata: IDictionary<string, obj> -> unit)> () =
   inherit ConfigureBuilder<'t> ()

    member inline __.Yield (_: unit) = [] : State<'t>

    [<CustomOperation "name">]
    member inline __.Name (state: State<'t>, name) =
        state
        |> operation (setName name)

    [<CustomOperation "description">]
    member inline __.Description (state: State<'t>, description) =
        state
        |> operation (setDescription description)

    [<CustomOperation "metadata">]
    member inline __.Metadata (state: State<'t>, metadata) =
        state
        |> operation (setMetadata metadata)

type TypedEntityBuilder<'t when
                              't: (member set_Name: string -> unit) and
                              't: (member set_Description: string -> unit) and
                              't: (member Metadata: IDictionary<string, obj>) and
                              't: (member set_Metadata: IDictionary<string, obj> -> unit) and
                              't: (member set_DefaultValue: obj -> unit) and
                              't: (member set_ResolvedType: IGraphType -> unit)> () =
    inherit EntityBuilder<'t> ()

    [<CustomOperation "defaultValue">]
    member inline __.DefaultValue (state: State<'t>, value) =
        state
        |> operation (setDefaultValue value)

    [<CustomOperation "type">]
    member inline __.Type (state: State<'t>, ``type``) =
        state
        |> operation (setResolvedType ``type``)

    member inline __.Type (state: State<'t>, ``type``) =
        state
        |> operation (fun this -> this |> setResolvedType (``type`` ()))

type BasicGraphTypeBuilder<'t when
                          't: (member set_Name: string -> unit) and
                          't: (member set_Description: string -> unit) and
                          't: (member Metadata: IDictionary<string, obj>) and
                          't: (member set_Metadata: IDictionary<string, obj> -> unit) and
                          't: (member set_DeprecationReason: string -> unit)> () =
    inherit EntityBuilder<'t> ()

    [<CustomOperation "deprecationReason">]
    member inline __.DeprecationReason (state: State<'t>, deprecationReason) =
        state
        |> operation (setDeprecationReason deprecationReason)

type TypedFieldBuilder<'t when
                      't: (member set_Name: string -> unit) and
                      't: (member set_Description: string -> unit) and
                      't: (member Metadata: IDictionary<string, obj>) and
                      't: (member set_Metadata: IDictionary<string, obj> -> unit) and
                      't: (member set_DefaultValue: obj -> unit) and
                      't: (member set_ResolvedType: IGraphType -> unit) and
                      't: (member set_DeprecationReason: string -> unit)> () =
    inherit TypedEntityBuilder<'t> ()

    [<CustomOperation "deprecationReason">]
    member inline __.DeprecationReason (state: State<'t>, deprecationReason) =
        state
        |> operation (setDeprecationReason deprecationReason)

type ComplexGraphTypeBuilder<'t, 'source when
                            't :> ComplexGraphType<'source> and
                            't: (member set_Name: string -> unit) and
                            't: (member set_Description: string -> unit) and
                            't: (member Metadata: IDictionary<string, obj>) and
                            't: (member set_Metadata: IDictionary<string, obj> -> unit) and
                            't: (member set_DeprecationReason: string -> unit)> () =
    inherit BasicGraphTypeBuilder<'t> ()

    [<CustomOperation "fields">]
    member inline __.Fields (state: State<'t>, fields: TypedFieldType<'source> list) =
        state
        |> unitOperation (fun this ->
            fields
            |> List.iter (this.AddField >> ignore)
        )
