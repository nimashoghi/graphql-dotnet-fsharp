module GraphQL.FSharp.BuilderInterface

open GraphQL.Types

open GraphQL.FSharp.BuilderBase

type InterfaceBuilderBase<'source> () =
    inherit ComplexGraphTypeBuilder<InterfaceGraphType<'source>, 'source> ()

type InterfaceBuilder<'source> (?value) =
    inherit InterfaceBuilderBase<'source> ()

    member __.Run (state: State<InterfaceGraphType<'source>>) =
        value
        |> Option.defaultValue (InterfaceGraphType<'source> ())
        |> apply state

type InterfaceEditBuilder<'source> () =
    inherit InterfaceBuilderBase<'source> ()

    member __.Run (state: State<InterfaceGraphType<'source>>) = apply state
