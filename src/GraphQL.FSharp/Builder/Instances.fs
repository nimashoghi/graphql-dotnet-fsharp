[<AutoOpen>]
module GraphQL.FSharp.Builder

open GraphQL.FSharp.BuilderTypes
open GraphQL.FSharp.Types

let argument<'t> ``type`` = ArgumentBuilder<'t> (``type`` = ``type``)

let directive = DirectiveBuilder ()

let enum = EnumerationBuilder ()

let field<'arguments, 'field, 'source> ``type`` = FieldBuilder<'arguments, 'field, 'source> (``type`` = ``type``)
let endpoint<'arguments, 'field> ``type`` name = FieldBuilder<'arguments, 'field, obj> (name = name, ``type`` = ``type``)

let input<'source> = InputObjectBuilder<'source> ()

let ``interface``<'source> = InterfaceBuilder<'source> ()

let object<'source> = ObjectBuilder<'source> ()
let query list =
    object<obj> {
        name "Query"
        fields list
    }
    |> Query
let mutation list =
    object<obj> {
        name "Mutation"
        fields list
    }
    |> Mutation
let subscription list =
    object<obj> {
        name "Subscription"
        fields list
    }
    |> Subscription

let schema = SchemaBuilder ()

let union = UnionBuilder ()
