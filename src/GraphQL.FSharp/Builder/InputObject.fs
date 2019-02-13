module GraphQL.FSharp.BuilderInputObject

open GraphQL.Types

open GraphQL.FSharp.BuilderBase

type InputObjectBuilderBase<'source> () =
    inherit ComplexGraphTypeBuilder<InputObjectGraphType<'source>, 'source> ()

type InputObjectBuilder<'source> (?value) =
    inherit InputObjectBuilderBase<'source> ()

    member __.Run (state: State<InputObjectGraphType<'source>>) =
        value
        |> Option.defaultValue (InputObjectGraphType<'source> ())
        |> apply state

type InputObjectEditBuilder<'source> () =
    inherit InputObjectBuilderBase<'source> ()

    member __.Run (state: State<InputObjectGraphType<'source>>) = apply state
