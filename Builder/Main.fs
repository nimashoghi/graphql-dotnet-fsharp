[<AutoOpen>]
module GraphQL.FSharp.Builder.Main

open GraphQL.Types

open GraphQL.FSharp.Builder.Argument
open GraphQL.FSharp.Builder.Field
open GraphQL.FSharp.Builder.Object
open GraphQL.FSharp.Builder.Schema
open GraphQL.FSharp.Builder.Union

let argument<'input> = ArgumentBuilder<'input> ()

let field<'value> = FieldBuilder<'value>()

let complex<'source when 'source : not struct and 'source : (new: unit -> 'source)> = ComplexObjectBuilder<'source> ()
let object<'source when 'source : not struct and 'source : (new: unit -> 'source)> = ObjectBuilder<'source> ()
let input<'source when 'source : not struct and 'source : (new: unit -> 'source)> = InputObjectBuilder<'source> ()

let record<'source when 'source : not struct and 'source : (new: unit -> 'source)> = RecordBuilder<'source> ()

let union = UnionBuilder()
// TODO: Add Union and Interface

let query = QueryBuilder()
let mutation = MutationBuilder()
let subscription = SubscriptionBuilder()

let schema = SchemaBuilder()
