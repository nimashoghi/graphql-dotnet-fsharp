module internal GraphQL.FSharp.AutoUnion

open FSharp.Reflection
open GraphQL.Types

open GraphQL.FSharp.AutoBase
open GraphQL.FSharp.Inference
open GraphQL.FSharp.NameTransformers
open GraphQL.FSharp.Resolvers
open GraphQL.FSharp.Types
open GraphQL.FSharp.Utils
open GraphQL.FSharp.Utils.Attributes

let isValidUnion<'union> =
    FSharpType.IsUnion typeof<'union>

let addFields<'union>
    (case: UnionCaseInfo)
    (object: ObjectGraphType<obj>) =
    case.GetFields ()
    |> Array.map (makePropField createReference)
    |> Array.iter (object.AddField >> ignore)

    object

let setIsTypeOf<'union>
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

// TODO: Test this
let addTag (case: UnionCaseInfo) (object: ObjectGraphType<obj>) =
    let field = EventStreamFieldType (Name = "Tag")
    field.ResolvedType <- NonNullGraphType (IntGraphType ())
    field.Resolver <- resolve (fun _ -> case.Tag)
    object.AddField field |> ignore

    object

let makeUnionCase<'union> (case: UnionCaseInfo) =
    ObjectGraphType<obj> (Name = sprintf "%s%s" typeof<'union>.Name case.Name)
    |> addTag case
    |> addMethods createReference
    |> addFields<'union> case
    |> setIsTypeOf<'union> case
    |> updateType case.CaseAttributes

let addUnionFields<'union> (union: UnionGraphType) =
    FSharpType.GetUnionCases typeof<'union>
    |> Array.map makeUnionCase<'union>
    |> Array.iter union.AddPossibleType

    union

let Union<'union> =
    if not isValidUnion<'union>
    then invalidArg "union" "type parameter must be a discriminated union"

    UnionGraphType<'union> (Name = transformTypeName typeof<'union> typeof<'union>.Name)
    |> updateType typeof<'union>.TypeAttributes
    |> addUnionFields<'union>
