[<AutoOpen>]
module GraphQL.FSharp.Types.Scalar

open GraphQL.Types
open GraphQL.Language.AST

let createScalar
    (serialize: 'value -> obj)
    (parseValue: obj -> 'value)
    (parseLiteral: IValue -> 'value) =
    {
        new ScalarGraphType() with
            member __.Serialize value = serialize (value :?> 'value)
            member __.ParseValue value = parseValue value |> box
            member __.ParseLiteral value = parseLiteral value |> box
    }

type Define with
    // TODO: Write tests
    static member Scalar<'t when 't : null>
        (name: string,
         coerceInput: obj -> 't option,
         coerceValue: IValue -> 't option,
         ?description: string,
         ?deprecationReason: string) =
        // TODO: Should this be `box`?
        let ``type`` =
            createScalar
                box
                (coerceInput >> Option.toObj)
                (coerceValue >> Option.toObj)
            |> setBasicProps
                name
                description
                deprecationReason
        ``type``
