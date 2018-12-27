module GraphQL.FSharp.Builder.Object

open GraphQL.Types
open Iris.Option.Builders

open GraphQL.FSharp.Model

type ObjectWrapper<'source> = {
    name: string option
    description: string option
    fields: (SchemaInfo -> ComplexGraphType<'source> -> unit) list
}

let newObject<'source> : ObjectWrapper<'source> = {
    name = None
    description = None
    fields = []
}

let merge lhs rhs = {
    name = Option.orElse lhs.name rhs.name
    description = Option.orElse lhs.description rhs.description
    fields = lhs.fields @ rhs.fields
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

            return graph
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

            return graph
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
