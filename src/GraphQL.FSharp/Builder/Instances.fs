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
let group endpointName operation list =
    endpoint
        (
            object<obj> {
                name (sprintf "%s%s" endpointName operation)
                fields list
            }
        )
        endpointName
        {
            manualResolve (fun _ -> obj ())
        }
let query name list = group name "Query" list
let mutation name list = group name "Mutation" list
let subscription name list = group name "Subscription" list

let schema = SchemaBuilder ()

let union = UnionBuilder ()
