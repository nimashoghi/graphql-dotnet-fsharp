module GraphQL.FSharp.AutoImplementation.Union

open FSharp.Reflection
open GraphQL.Types

open GraphQL.FSharp.Inference
open GraphQL.FSharp.Registry
open GraphQL.FSharp.Utils

// TODO: Single case discriminated unions
// e.g. type Email = Email of string

let private isValidUnion<'union> =
    FSharpType.IsUnion typeof<'union>

let private makeUnionCase<'union> (case: UnionCaseInfo) =
    let object = ObjectGraphType ()

    setInfo case object

    addMethods<'union> inferObject object

    case.GetFields ()
    |> Array.map (makePropField inferObject)
    |> Array.iter (object.AddField >> ignore)

    object.IsTypeOf <- (fun x ->
        match x with
        | :? 'union ->
            FSharpValue.GetUnionFields (x, typeof<'union>)
            |> (fun (case, _) -> case.Tag)
            |> ((=) case.Tag)
        | _ -> false)

    getUnionCaseAttribute<TypeAttribute> case
    |> Option.iter (updateType object >> ignore)

    object

let private addUnionFields<'union> (union: UnionGraphType) =
    FSharpType.GetUnionCases typeof<'union>
    |> Array.map makeUnionCase<'union>
    |> Array.iter union.AddPossibleType

let Union<'union> =
    if not isValidUnion<'union>
    then invalidArg "union" "type parameter must be a discriminated union"

    let union = UnionGraphType ()

    setInfo typeof<'union> union

    getTypeAttribute<TypeAttribute> typeof<'union>
    |> Option.iter (updateType union >> ignore)

    addUnionFields<'union> union
    Object.register (typeof<'union>, union)

    union
