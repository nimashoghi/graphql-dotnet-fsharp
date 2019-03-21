[<AutoOpen>]
module GraphQL.FSharp.Builder.Instances

open GraphQL.Conversion

open GraphQL.FSharp.Builder.Operations
open GraphQL.FSharp.Types

type Endpoints = Field<obj> list
let endpoints (endpoints: Endpoints) = endpoints

let argument<'argument> ``type`` parameters =
    graphOrSystemType ``type`` typeof<'argument> :: parameters
    |> reduceWith Argument<'argument>

// TODO: Move type handling here
// TODO: type parameter order
// TODO: Warn if no resolver
let field<'field,'arguments, 'source> ``type`` parameters =
    graphOrSystemType ``type`` typeof<'field> :: parameters
    |> reduceWith Field<'arguments, 'field, 'source>

let object<'t> parameters =
    name typeof<'t>.Name :: parameters
    |> reduceWith Object<'t>

let ``interface``<'t> parameters =
    name typeof<'t>.Name :: parameters
    |> reduceWith Interface<'t>

let input<'t> parameters =
    name typeof<'t>.Name :: parameters
    |> reduceWith InputObject<'t>

let enum<'t> parameters =
    name typeof<'t>.Name :: parameters
    |> reduceWith Enumeration<'t>

let union<'t> parameters =
    name typeof<'t>.Name :: parameters
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
    reduce schema parameters

let (=>) x y = x, y
