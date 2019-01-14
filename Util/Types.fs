[<AutoOpen>]
module GraphQL.FSharp.Util.Types

open GraphQL.Types

type TypedFieldType<'source>() =
    inherit EventStreamFieldType()
