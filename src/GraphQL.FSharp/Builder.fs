module GraphQL.FSharp.Builder

open GraphQL.FSharp.BuilderArgument
open GraphQL.FSharp.BuilderDirective
open GraphQL.FSharp.BuilderEnum
open GraphQL.FSharp.BuilderField
open GraphQL.FSharp.BuilderInputObject
open GraphQL.FSharp.BuilderInterface
open GraphQL.FSharp.BuilderObject
open GraphQL.FSharp.BuilderSchema
open GraphQL.FSharp.BuilderUnion
open GraphQL.FSharp.Types

let argument<'t> = ArgumentBuilder<'t> ()
let argumentOf ``type`` = ArgumentBuilder<obj> ``type``

let directive = DirectiveBuilder ()

let enum = EnumerationBuilder ()

let field<'source> = FieldBuilder<'source> ()
let endpoint<'source> name = FieldBuilder<'source> (name = name)
// TODO: Add tests for fieldOf
let fieldOf ``type`` = FieldBuilder<'source> (``type`` = ``type``)

let input<'source> = InputObjectBuilder<'source> ()

// FIXME: rename?
let ``interface``<'source> = InterfaceBuilder<'source> ()

let object<'source> = ObjectBuilder<'source> ()
let query list =
    object<obj> {
        name "Query"
        fields list
    }
    : Query
let mutation list =
    object<obj> {
        name "Mutation"
        fields list
    }
    : Mutation
let subscription list =
    object<obj> {
        name "Subscription"
        fields list
    }
    : Subscription

let schema = SchemaBuilder ()

let union = UnionBuilder ()

// module Edit =
//     let argument value = ArgumentBuilder (value = value)

//     let directive value = DirectiveBuilder (value = value)

//     let enum value = EnumerationBuilder (value = value)

//     let field value = FieldBuilder (value = value)

//     let input value = InputObjectBuilder (value = value)

//     let ``interface`` value = InterfaceBuilder (value = value)

//     let object value = ObjectBuilder (value = value)

//     let schema value = SchemaBuilder (value = value)

//     let union value = UnionBuilder (value = value)
