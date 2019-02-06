module internal GraphQL.FSharp.AutoEnum

open System
open FSharp.Reflection
open GraphQL.Types

open GraphQL.FSharp.AutoBase
open GraphQL.FSharp.Types
open GraphQL.FSharp.NameTransformers
open GraphQL.FSharp.Utils.Attributes

let isValidEnum<'enum> =
    typeof<'enum>.IsEnum ||
    (FSharpType.IsUnion typeof<'enum> &&
     FSharpType.GetUnionCases typeof<'enum>
     |> Array.forall (fun case ->
         case.GetFields ()
         |> Array.isEmpty))

let (|Enum|Union|) (``type``: Type) =
    if ``type``.IsEnum
    then Enum
    elif FSharpType.IsUnion ``type``
    then Union
    else invalidArg "type" "The provided type must be an enum or a discriminated union!"

let makeEnumValue (case: UnionCaseInfo) =
    let enumValue = EnumValueDefinition ()
    enumValue.Name <- case.Name
    enumValue.Description <- null
    enumValue.Value <- FSharpValue.MakeUnion (case, [||])

    enumValue
    |> updateEnumValue case.CaseAttributes

let addCases<'enum> (enum: EnumerationGraphType) =
    FSharpType.GetUnionCases typeof<'enum>
    |> Array.map makeEnumValue
    |> Array.iter enum.AddValue

    enum

let unionEnum<'enum> () =
    EnumerationGraphType (Name = transformTypeName typeof<'enum> typeof<'enum>.Name)
    |> addCases<'enum>

let Enum<'enum> =
    if not isValidEnum<'enum> then invalidArg "enum" "type parameter must be an enum"

    let graphType =
        match typeof<'enum> with
        | Enum -> EnumerationGraphTypeEx<'enum> () :> EnumerationGraphType
        | Union -> unionEnum<'enum> ()

    graphType
    |> updateType typeof<'enum>.TypeAttributes
