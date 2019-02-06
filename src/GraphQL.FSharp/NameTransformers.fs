module GraphQL.FSharp.NameTransformers

open System
open System.Reflection

open GraphQL.FSharp.Utils.Attributes

let getterMethod (methodInfo: MethodInfo) (name: string) =
    if hasAttribute<GetterAttribute> methodInfo.MethodAttributes then
        if name.StartsWith "Get"
        then Some (name.Substring "Get".Length)
        else None
    else None

let methodTransformers: (MethodInfo -> string -> string option) list = [getterMethod]
let propertyTransformers: (PropertyInfo -> string -> string option) list = []
let typeTransformers: (Type -> string -> string option) list = []

let rec runTransformers transformers info name =
    match transformers with
    | head :: tail ->
        head info name
        |> Option.defaultValue name
        |> runTransformers tail info
    | _ -> name

let transformMethodName info name = runTransformers methodTransformers info name
let transformPropertyName info name = runTransformers propertyTransformers info name
let transformTypeName info name = runTransformers typeTransformers info name
