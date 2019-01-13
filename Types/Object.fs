[<AutoOpen>]
module GraphQL.FSharp.Types.Object

open System
open GraphQL.Types

type Define with
    static member Object (name, fields, ?isTypeOf, ?description, ?deprecationReason) =
        let object = ObjectGraphType () |> setBasicProps name description deprecationReason
        isTypeOf |> Option.iter (fun isTypeOf -> object.IsTypeOf <- Func<_, _> isTypeOf)
        List.iter (object.AddField >> ignore) fields
        object
