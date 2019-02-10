module GraphQL.FSharp.BuilderObject

open GraphQL.Types

open GraphQL.FSharp.BuilderBase
open GraphQL.FSharp.Types

let inline private set f (x: #ObjectGraphType<_>) = f x; x

module Nullable =
    let defaultValue value x = x |> Option.ofObj |> Option.defaultValue value
    let iter f x = x |> (Option.ofObj >> Option.iter f)

// TODO: Handle field.Type
let mergeWith (source: TypedFieldType<'source>) (destination: FieldType) =
    source.ResolvedType |> Nullable.iter (fun value -> destination.ResolvedType <- value)
    source.Resolver |> Nullable.iter (fun value -> destination.Resolver <- value)
    source.Arguments |> Nullable.iter (fun value ->
        if isNull destination.Arguments
        then destination.Arguments <- QueryArguments []

        value
        |> Nullable.defaultValue (QueryArguments [])
        |> Seq.iter destination.Arguments.Add
    )

    match destination with
    | :? EventStreamFieldType as streamDestination ->
        source.Subscriber |> Nullable.iter (fun value -> streamDestination.Subscriber <- value)
        source.AsyncSubscriber |> Nullable.iter (fun value -> streamDestination.AsyncSubscriber <- value)
    | _ -> ()

    if source.GetMetadata<bool> ("HasDefaultValue", false)
    then destination.DefaultValue <- source.DefaultValue

    source.Description |> Nullable.iter (fun value -> destination.Description <- value)
    source.DeprecationReason |> Nullable.iter (fun value -> destination.DeprecationReason <- value)
    source.Metadata |> Nullable.iter (fun value ->
        destination.Metadata <- dict [
            yield!
                destination.Metadata
                |> Nullable.defaultValue (dict [])
                |> Seq.map (fun pair -> pair.Key, pair.Value)
            yield!
                value
                |> Seq.map (fun pair -> pair.Key, pair.Value)
        ])

    destination

let addField (object: ObjectGraphType<'source>) (field: TypedFieldType<'source>) =
    if not <| object.HasField field.Name
    then object.AddField field
    else object.GetField field.Name |> mergeWith field

let addAllFields (fields: TypedFieldType<'source> list) (this: ObjectGraphType<'source>) =
    fields
    |> List.iter (addField this >> ignore)

    this

type ObjectBuilder<'source> () =
    // TODO: Make it so we return a new object here
    member __.Yield (_: unit) = ObjectGraphType<'source> ()

    [<CustomOperation "name">]
    member __.CustomOperation_Name (this: ObjectGraphType<'source>, name) =
        this |> setName name

    [<CustomOperation "description">]
    member __.CustomOperation_Description (this: ObjectGraphType<'source>, description) =
        this |> setDescription description

    [<CustomOperation "deprecationReason">]
    member __.CustomOperation_DeprecationReason (this: ObjectGraphType<'source>, deprecationReason) =
        this |> setDeprecationReason deprecationReason

    [<CustomOperation "metadata">]
    member __.CustomOperation_Metadata (this: ObjectGraphType<'source>, metadata) =
        this |> setMetadata metadata

    [<CustomOperation "fields">]
    member __.CustomOperation_Fields (this: ObjectGraphType<'source>, fields: TypedFieldType<'source> list) =
        set (addAllFields fields >> ignore) this

    [<CustomOperation "interfaces">]
    member __.CustomOperation_Interfaces (this: ObjectGraphType<'source>, ``interface``) =
        set (fun x ->
            ``interface``
            |> List.iter x.AddResolvedInterface) this
