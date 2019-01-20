module GraphQL.FSharp.Types

open GraphQL.Types

type TypedFieldType<'source>() =
    inherit EventStreamFieldType()

type EnumerationGraphTypeEx<'t> () =
    inherit EnumerationGraphType<'t> ()

    do
        base.Values
        |> Seq.iter (fun value ->
            if value.Description = null
            then value.Description <- "")

    override __.ChangeEnumCase x = x
