[<AutoOpen>]
module GraphQL.FSharp.Builder.Instances

open GraphQL.Conversion
open GraphQL.Types

open GraphQL.FSharp.Builder.Operations
open GraphQL.FSharp.Types

type Endpoints = Field<obj> list
let inline endpoints (endpoints: Endpoints) = endpoints

let inline argument<'argument> ``type`` parameters =
    graphOrSystemType ``type`` typeof<'argument> :: parameters
    |> reduceWith Argument<'argument>

// TODO: Move type handling here
// TODO: type parameter order
let inline field<'field,'arguments, 'source> ``type`` parameters =
    graphOrSystemType ``type`` typeof<'field> :: parameters
    |> reduceWith Field<'arguments, 'field, 'source>

let inline object<'t> parameters =
    name typeof<'t>.Name :: parameters
    |> reduceWith Object<'t>

let inline ``interface``<'t> parameters =
    name typeof<'t>.Name :: parameters
    |> reduceWith Interface<'t>

let inline input<'t> parameters =
    name typeof<'t>.Name :: parameters
    |> reduceWith InputObject<'t>

let inline enum<'t> parameters =
    name typeof<'t>.Name :: parameters
    |> reduceWith Enumeration<'t>

let inline union<'t> parameters =
    name typeof<'t>.Name :: parameters
    |> reduceWith Union<'t>

let inline schema parameters =
    let schema =
        new Schema (
            FieldNameConverter =
                {
                    new IFieldNameConverter with
                        member __.NameFor (name, _) = name
                }
        )
    reduce schema parameters

let inline (=>) x y = x, y
