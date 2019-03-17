[<AutoOpen>]
module GraphQL.FSharp.Builder.Instances

open GraphQL.Conversion
open GraphQL.Types

open GraphQL.FSharp.Builder.Operations
open GraphQL.FSharp.Types

type Endpoints = Field<obj> list
let inline endpoints (endpoints: Endpoints) = endpoints

let inline field (``type``: #IGraphType) parameters =
    graphType ``type`` :: parameters
    |> reduceWith Field<'arguments, 'field, 'source>

let inline object parameters =
    reduceWith Object<'t> parameters

let inline ``interface`` parameters =
    reduceWith Interface<'t> parameters

let inline input parameters =
    reduceWith InputObject<'t> parameters

let inline enum parameters =
    reduceWith Enumeration<'t> parameters

let inline union parameters =
    reduceWith Union<'t> parameters

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
