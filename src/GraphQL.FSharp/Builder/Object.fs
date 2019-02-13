module GraphQL.FSharp.BuilderObject

open GraphQL.Types

open GraphQL.FSharp.BuilderBase

type ObjectBuilderBase<'source> () =
    inherit ComplexGraphTypeBuilder<ObjectGraphType<'source>, 'source> ()

    [<CustomOperation "interfaces">]
    member __.Interfaces (state: State<ObjectGraphType<'source>>, interfaces) =
        state
        |> unitOperation (fun this ->
            interfaces
            |> List.iter this.AddResolvedInterface
        )

type ObjectBuilder<'source> (?value) =
    inherit ObjectBuilderBase<'source> ()

    member __.Run (state: State<ObjectGraphType<'source>>) =
        value
        |> Option.defaultValue (ObjectGraphType<'source> ())
        |> apply state

type ObjectEditBuilder<'source> () =
    inherit ObjectBuilderBase<'source> ()

    member __.Run (state: State<ObjectGraphType<'source>>) = apply state
