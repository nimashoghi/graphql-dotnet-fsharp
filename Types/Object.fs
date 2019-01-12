[<AutoOpen>]
module GraphQL.FSharp.Types.Object

open System
open GraphQL.Types
open Iris.Option.Builders

type Define with
    static member Object
        (name: string,
         ?fields: FieldType list,
         ?isTypeOf: obj -> bool,
         ?description: string,
         ?deprecationReason: string) =
        let object = ObjectGraphType () |> setBasicProps name description deprecationReason

        maybeUnit {
            let! isTypeOf = isTypeOf
            object.IsTypeOf <- Func<_, _> isTypeOf
        }

        for field in optionList fields do
            object.AddField field |> ignore

        object
