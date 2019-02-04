module internal GraphQL.FSharp.AutoUnion

open FSharp.Reflection
open GraphQL.Types

open GraphQL.FSharp.AutoBase
open GraphQL.FSharp.Inference
open GraphQL.FSharp.Types
open GraphQL.FSharp.Utils

let internal isValidUnion<'union> =
    FSharpType.IsUnion typeof<'union>

let internal addFields<'union>
    (case: UnionCaseInfo)
    (object: ObjectGraphType<obj>) =
    case.GetFields ()
    |> Array.map (makePropField createReference)
    |> Array.iter (object.AddField >> ignore)

    object

let internal setIsTypeOf<'union>
    (case: UnionCaseInfo)
    (object: ObjectGraphType<obj>) =
    object.IsTypeOf <- (fun x ->
        match x with
        | :? 'union ->
            FSharpValue.GetUnionFields (x, typeof<'union>)
            |> (fun (case, _) -> case.Tag)
            |> ((=) case.Tag)
        | _ -> false)

    object

let internal makeUnionCase<'union> (case: UnionCaseInfo) =
    ObjectGraphType<obj> (Name = case.Name)
    |> addMethods createReference
    |> addFields<'union> case
    |> setIsTypeOf<'union> case
    |> updateType case.CaseAttributes

let internal addUnionFields<'union> (union: UnionGraphType) =
    FSharpType.GetUnionCases typeof<'union>
    |> Array.map makeUnionCase<'union>
    |> Array.iter union.AddPossibleType

    union

let Union<'union> =
    if not isValidUnion<'union>
    then invalidArg "union" "type parameter must be a discriminated union"

    UnionGraphType<'union> (Name = typeof<'union>.Name)
    |> updateType typeof<'union>.TypeAttributes
    |> addUnionFields<'union>
