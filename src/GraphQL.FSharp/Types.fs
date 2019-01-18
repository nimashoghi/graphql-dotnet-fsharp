module GraphQL.FSharp.Types

open GraphQL.Types

type TypedFieldType<'source>() =
    inherit EventStreamFieldType()

type EnumerationGraphTypeEx<'t> () =
    inherit EnumerationGraphType<'t> ()

    override __.ChangeEnumCase x = x
