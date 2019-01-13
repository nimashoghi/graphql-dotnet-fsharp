[<AutoOpen>]
module GraphQL.FSharp.Types.Interface

open GraphQL.Types

type Define with
    static member Interface (name, fields, ?description, ?deprecationReason) =
        let ``type`` = InterfaceGraphType () |> setBasicProps name description deprecationReason
        List.iter (``type``.AddField >> ignore) fields
        ``type``
