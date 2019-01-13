module GraphQL.FSharp.Types.InputObject

open GraphQL.Types

type Define with
    static member InputObject (name, fields, ?description, ?deprecationReason) =
        let object = InputObjectGraphType () |> setBasicProps name description deprecationReason
        List.iter (object.AddField >> ignore) fields
        object
