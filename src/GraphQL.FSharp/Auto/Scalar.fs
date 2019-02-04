module internal GraphQL.FSharp.AutoScalar

open System
open FSharp.Reflection
open GraphQL.Types

open GraphQL.FSharp.Inference

let unwrapUnion (``type``: Type) (case: UnionCaseInfo) value =
    match FSharpValue.GetUnionFields (value, ``type``) with
    | (case', [|field|]) when case = case' -> field
    | _ -> invalidArg "type" "Invalid union type for scalars!"

let wrapUnion (case: UnionCaseInfo) value =
    FSharpValue.MakeUnion (case, [|value|])

let (|Scalar|_|) (``type``: Type) =
    if not <| FSharpType.IsUnion ``type`` then None else
    match FSharpType.GetUnionCases ``type`` with
    | [|case|] when case.Name = ``type``.Name ->
        match case.GetFields () with
        | [|field|] ->
            getDefaultType field.PropertyType
            |> Option.map (fun ``type`` -> ``type``, case, case.Name)
        | _ -> None
    | _ -> None

let wrapType wrapScalar unwrapScalar name (``type``: ScalarGraphType) =
    {
        new ScalarGraphType (Name = name) with
            override __.Serialize value = unwrapScalar value |> ``type``.Serialize
            override __.ParseValue value = ``type``.ParseValue value |> wrapScalar
            override __.ParseLiteral value = ``type``.ParseLiteral value |> wrapScalar
    }

let Scalar<'scalar> =
    match typeof<'scalar> with
    | Scalar (``type``, case, name) ->
        wrapType (wrapUnion case) (unwrapUnion typeof<'scalar> case) name ``type``
    | _ -> invalidArg "'scalar" "'scalar is not a supported scalar type!"
