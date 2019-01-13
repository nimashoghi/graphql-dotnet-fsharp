[<AutoOpen>]
module GraphQL.FSharp.Types.Union

open GraphQL.Types

type Define with
    static member Union (name, types, ?description, ?deprecationReason) =
        let ``type`` = UnionGraphType () |> setBasicProps name description deprecationReason
        ``type``.PossibleTypes <- List.toSeq types
        ``type``
