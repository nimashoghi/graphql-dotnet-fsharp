[<AutoOpen>]
module GraphQL.FSharp.Types.Object

open System
open GraphQL.Types

type Define with
    static member Object
        (name: string,
         fields: FieldType list,
         ?isTypeOf: obj -> bool,
         ?description: string,
         ?deprecationReason: string) =
        let object = ObjectGraphType () |> setBasicProps name description deprecationReason

        isTypeOf |> Option.iter (fun isTypeOf -> object.IsTypeOf <- Func<_, _> isTypeOf)

        for field in fields do
            object.AddField field |> ignore

        object

    static member InputObject
        (name: string,
         fields: FieldType list,
         ?description: string,
         ?deprecationReason: string) =
        let object = InputObjectGraphType () |> setBasicProps name description deprecationReason

        for field in fields do
            object.AddField field |> ignore

        object
