[<AutoOpen>]
module GraphQL.FSharp.Builder.Instances

open GraphQL.Conversion
open GraphQL.Types

open GraphQL.FSharp.Builder.Operations
open GraphQL.FSharp.Inference
open GraphQL.FSharp.Types

type Endpoints = Field<obj> list
type Types = IGraphType list

let inline (=>) x y = x, y

let argument<'argument> ``type`` parameters =
    graphOrSystemType ``type`` typeof<'argument> :: parameters
    |> flattenOperations
    |> reduceWith Argument<'argument>

// TODO: Test subscriptions properly
// TOOD: Test new anon record stuff properly
// FIXME: Subscription that return primitives are broken in the current version of GraphQL + GraphQL.Server
let field<'field,'arguments, 'source> ``type`` parameters =
    Field.ensureCorrectType ()
    :: graphOrSystemTypeField ``type``
    :: addArguments ()
    :: parameters
    |> flattenOperations
    |> reduceWith Field<'field, 'arguments, 'source>

let endpoint endpointName ``type`` parameters =
    name endpointName :: parameters
    |> field ``type``

let object<'t> parameters =
    name (typeName typeof<'t>) :: parameters
    |> flattenOperations
    |> reduceWith Object<'t>

let ``interface``<'t> parameters =
    name (typeName typeof<'t>) :: parameters
    |> flattenOperations
    |> reduceWith Interface<'t>

let input<'t> parameters =
    name (typeName typeof<'t>) :: parameters
    |> flattenOperations
    |> reduceWith InputObject<'t>

let enum<'t> parameters =
    name (typeName typeof<'t>) :: parameters
    |> flattenOperations
    |> reduceWith Enumeration<'t>

let union<'t> parameters =
    name (typeName typeof<'t>) :: parameters
    |> flattenOperations
    |> reduceWith Union<'t>

let schema parameters =
    let schema =
        new Schema (
            FieldNameConverter =
                {
                    new IFieldNameConverter with
                        member __.NameFor (name, _) = name
                }
        )
    schema.RegisterType<EmptyObjectGraphType> ()
    parameters
    |> flattenOperations
    |> reduce schema

let endpoints (endpoints: Field<obj> list): Endpoints = endpoints
