module GraphQL.FSharp.Builder.Object

open System
open Apollo
open FSharp.Reflection
open GraphQL.Resolvers
open GraphQL.Types
open Iris.Option.Builders

open GraphQL.FSharp.Model
open GraphQL.FSharp.Builder.Field
open GraphQL.FSharp.Builder.Helpers

type ObjectWrapper<'source> = {
    name: string option
    description: string option
    fields: (SchemaInfo -> ComplexGraphType<'source> -> unit) list
    effects: (SchemaInfo -> unit) list
}

let newObject<'source> : ObjectWrapper<'source> = {
    name = None
    description = None
    fields = []
    effects = []
}

let merge lhs rhs = {
    name = Option.orElse lhs.name rhs.name
    description = Option.orElse lhs.description rhs.description
    fields = lhs.fields @ rhs.fields
    effects = lhs.effects @ rhs.effects
}

let inline private get (x: ObjectWrapper<_>) = x

type ComplexObjectBuilder<'source when 'source : not struct and 'source : (new: unit -> 'source)>() =
    abstract member Yield: _ -> ObjectWrapper<'source>
    default __.Yield _ = newObject<'source>

    /// Sets the name of this object
    [<CustomOperation "name">]
    member __.Name (object, name) = {get object with name = Some name}

    /// Sets the description of this object
    [<CustomOperation "description">]
    member __.Description (object, description) = {get object with description = Some description}

    /// Adds fields to the object
    [<CustomOperation "fields">]
    member __.Fields (object, fields) = {get object with fields = object.fields @ fields}

    /// **Description**
    ///   * Registers the provided effects.
    [<CustomOperation "effects">]
    member __.Effects (object, effects) = {get object with effects = object.effects @ effects}

    /// Imports another complex object type into the current object type
    [<CustomOperation "import">]
    member __.Import (object, other) = merge object other

type ObjectBuilder<'source when 'source : not struct and 'source : (new: unit -> 'source)>() =
    inherit ComplexObjectBuilder<'source>()

    member __.Run (object: ObjectWrapper<'source>) =
        fun (schema: SchemaInfo) -> maybeOrThrow {
            let graph = ObjectGraphType<'source>()

            let! name = object.name
            graph.Name <- name

            maybeUnit {
                let! description = object.description
                graph.Description <- description
            }

            List.iter (fun object ->
                object schema (graph :> ComplexGraphType<'source>))
                object.fields

            return graph :> IGraphType // TODO: Should we do this?
        }

type InputObjectBuilder<'source when 'source : not struct and 'source : (new: unit -> 'source)>() =
    inherit ComplexObjectBuilder<'source>()

    member __.Run (object: ObjectWrapper<'source>) =
        fun (schema: SchemaInfo) -> maybeOrThrow {
            let graph = InputObjectGraphType<'source>()

            let! name = object.name
            graph.Name <- name

            let! description = object.description
            graph.Description <- description

            List.iter (fun object -> object schema (graph :> ComplexGraphType<'source>)) object.fields

            return graph :> IGraphType // TODO: Should we do this?
        }

exception NotRecordException of Type

let internal throwIfNotRecord ``type`` =
    if not (FSharpType.IsRecord ``type``)
    then raise (NotRecordException ``type``)

let internal field<'value> = FieldBuilder<'value>()

let internal getRecordElements<'source> =
    let fields = FSharpType.GetRecordFields typeof<'source>
    fields
    |> Array.map (fun f ->
        fun (schema: SchemaInfo) (graphType: ComplexGraphType<'source>) ->
            let fieldType = FieldType()
            fieldType.Name <- f.Name
            // TODO: Set nullable based on option types
            setType schema f.PropertyType false fieldType
            fieldType.Resolver <- FuncFieldResolver<'source, obj>(fun ctx -> f.GetValue ctx.Source)
            graphType.AddField fieldType |> ignore
            ())
    |> Array.toList

// TODO: Implement
type RecordBuilder<'source when 'source : not struct and 'source : (new: unit -> 'source)>() =
    inherit ObjectBuilder<'source>()

    do throwIfNotRecord typeof<'source>

    override __.Yield _ =
        let wrapper = newObject<'source>
        {
            wrapper with
                name = Some typeof<'source>.Name
                fields = wrapper.fields @ getRecordElements<'source>
        }

type QueryBuilder() =
    inherit ObjectBuilder<obj>()

    override __.Yield _ = {newObject<obj> with name = Some "Query"}

type MutationBuilder() =
    inherit ObjectBuilder<obj>()

    override __.Yield _ = {newObject<obj> with name = Some "Mutation"}

type SubscriptionBuilder() =
    inherit ObjectBuilder<obj>()

    override __.Yield _ = {newObject<obj> with name = Some "Subscription"}
