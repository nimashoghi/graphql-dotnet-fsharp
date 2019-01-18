module GraphQL.FSharp.Tests.Assert

open Swensen.Unquote
open GraphQL.Types

let enumEqual name list (enum: EnumerationGraphType) =
    Seq.length enum.Values =! List.length list

    enum.Name =! name

    let values = dict list

    for enumValue in enum.Values do
        test <@ values.ContainsKey enumValue.Name @>
        enumValue.Value =! values.[enumValue.Name]

let objectEqual name list (object: #IComplexGraphType) =
    Seq.length object.Fields =! List.length list

    object.Name =! name

    let ``types`` = dict list

    for field in object.Fields do
        test <@ ``types``.ContainsKey field.Name @>
        field.ResolvedType =! ``types``.[field.Name]
