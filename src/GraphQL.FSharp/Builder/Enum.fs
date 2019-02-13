module GraphQL.FSharp.BuilderEnum

open GraphQL.Types

open GraphQL.FSharp.BuilderBase

type EnumerationBuilderBase () =
    inherit BasicGraphTypeBuilder<EnumerationGraphType> ()

    [<CustomOperation "cases">]
    member __.CustomOperation_Cases (state: State<EnumerationGraphType>, values) =
        state
        |> unitOperation (fun this ->
            values
            |> List.iter this.AddValue
        )

type EnumerationBuilder (?value) =
    inherit EnumerationBuilderBase ()

    member __.Run (state: State<EnumerationGraphType>) =
        value
        |> Option.defaultValue (EnumerationGraphType ())
        |> apply state

type EnumerationEditBuilder () =
    inherit EnumerationBuilderBase ()

    member __.Run (state: State<EnumerationGraphType>) = apply state
