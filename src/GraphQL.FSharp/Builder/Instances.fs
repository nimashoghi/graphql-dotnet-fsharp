module GraphQL.FSharp.Builder

open GraphQL.FSharp
open GraphQL.FSharp.Types

let argument<'t> = ArgumentBuilder<'t> ()

let directive = DirectiveBuilder ()

let enum = EnumerationBuilder ()

let field<'field, 'source> = FieldBuilder<'field, 'source> ()
let endpoint<'field> name = FieldBuilder<'field, obj> (name = name)

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

let union<'source> = UnionBuilder<'source> ()

// TODO: place this somewhere else
do
    Option.main ()
    Result.main ()
