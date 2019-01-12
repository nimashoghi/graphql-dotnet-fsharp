[<AutoOpen>]
module GraphQL.FSharp.Types.Interface

open GraphQL.Types

type Define with
    static member Interface
        (name: string,
         ?fields: FieldType list,
         ?description: string,
         ?deprecationReason: string) =
        let ``type`` = InterfaceGraphType () |> setBasicProps name description deprecationReason
        for field in optionList fields do
            ``type``.AddField field |> ignore
        ``type``
