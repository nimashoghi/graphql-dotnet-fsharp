module GraphQL.FSharp.Types

open GraphQL.Types

type TypedFieldType<'source>() =
    inherit EventStreamFieldType()
