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

[<AutoOpen>]
module Operators =
    let inline (=>) lhs rhs = (lhs, rhs)

let argument<'t> = ArgumentBuilder<'t> ()
let argumentOf ``type`` = ArgumentBuilder<obj> ``type``

let directive = DirectiveBuilder ()

let enum = EnumerationBuilder ()

let field<'source> = FieldBuilder<'source> ()
let endpoint<'source> name = FieldBuilder<'source> (name = name)
let fieldOf ``type`` = FieldBuilder<'source> (``type`` = ``type``)

let input<'source> = InputObjectBuilder<'source> ()

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

module Edit =
    let argument<'t> = ArgumentEditorBuilder<'t> ()
    let directive = DirectiveEditBuilder ()
    let enum = EnumerationEditBuilder ()
    let field<'source> = FieldEditBuilder<'source> ()
    let input<'source> = InputObjectEditBuilder<'source> ()
    let ``interface``<'source> = InterfaceEditBuilder<'source> ()
    let object<'source> = ObjectEditBuilder<'source> ()
    let schema = SchemaEditBuilder ()
    let union = UnionEditBuilder ()
