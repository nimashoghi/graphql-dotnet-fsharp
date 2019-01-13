[<AutoOpen>]
module GraphQL.FSharp.Types.Field

open GraphQL.Types

type Define with
    static member Field (name, ?``type``, ?args, ?description, ?deprecationReason) =
        let field = FieldType () |> setBasicProps name description deprecationReason
        Option.iter (fun ``type`` -> field.ResolvedType <- ``type``) ``type``
        Option.iter (fun args ->
            field.Arguments <-
                args
                |> List.toArray
                |> QueryArguments) args
        field
