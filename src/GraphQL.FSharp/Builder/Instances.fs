[<AutoOpen>]
module GraphQL.FSharp.Builder.Instances

open GraphQL.Conversion

open GraphQL.FSharp.Builder.Operations
open GraphQL.FSharp.Types

type Endpoints = Field<obj> list
let endpoints (endpoints: Endpoints) = endpoints

let argument<'argument> ``type`` parameters =
    graphOrSystemType ``type`` typeof<'argument> :: parameters
    |> flattenOperations
    |> reduceWith Argument<'argument>

// TODO: Test subscriptions properly
// TOOD: Test new anon record stuff properly
// FIXME: Subscriptions are broken in the current version of GraphQL + GraphQL.Server
let field<'field,'arguments, 'source> ``type`` parameters =
    graphOrSystemTypeField ``type`` :: parameters
    |> flattenOperations
    // |> Analyzers.field
    |> reduceWith Field<'field, 'arguments, 'source>

let object<'t> parameters =
    name typeof<'t>.Name :: parameters
    |> flattenOperations
    |> reduceWith Object<'t>

let ``interface``<'t> parameters =
    name typeof<'t>.Name :: parameters
    |> flattenOperations
    |> reduceWith Interface<'t>

let input<'t> parameters =
    name typeof<'t>.Name :: parameters
    |> flattenOperations
    |> reduceWith InputObject<'t>

let enum<'t> parameters =
    name typeof<'t>.Name :: parameters
    |> flattenOperations
    |> reduceWith Enumeration<'t>

let union<'t> parameters =
    name typeof<'t>.Name :: parameters
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

let (=>) x y = x, y
