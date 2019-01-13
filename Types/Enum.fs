[<AutoOpen>]
module GraphQL.FSharp.Types.Enum

open System
open FSharp.Reflection
open Iris.Option.Builders
open GraphQL.Types
open GraphQL.Language.AST
open GraphQL.Utilities

let isValidEnumUnion ``type`` =
    FSharpType.GetUnionCases ``type``
    |> Array.exists (fun case -> Array.length (case.GetFields ()) > 0)
    |> not

let (|ClassicEnum|FSharpUnionEnum|) ``type`` =
    // C# style enums created in F# return false for IsUnion
    if FSharpType.IsUnion ``type`` then
        assert (isValidEnumUnion ``type``)
        FSharpUnionEnum
    else ClassicEnum

let private createFSharpGraphType<'t> () =
    let systemType = typeof<'t>

    let ``type`` = EnumerationGraphType()
    ``type``.Name <- StringUtils.ToPascalCase systemType.Name
    for case in FSharpType.GetUnionCases systemType do
        ``type``.AddValue (
            name = case.Name,
            description = null,
            value = FSharpValue.MakeUnion (case, [||]),
            deprecationReason = null)
    ``type``

let private createEnumType<'t> () =
    match typeof<'t> with
    | FSharpUnionEnum -> createFSharpGraphType<'t> ()
    | ClassicEnum -> EnumerationGraphType<'t> () :> EnumerationGraphType

type Convert with
    static member Enum<'t>
        (name: string,
         ?description: string,
         ?deprecationReason: string) =
        let ``type`` = createEnumType<'t> () |> setBasicProps name description deprecationReason
        ``type``


type Define with
    static member EnumValue
        (name: string,
         value: 'value,
         ?description: string,
         ?deprecationReason: string) =
        let valueDef = EnumValueDefinition () |> setBasicProps name description deprecationReason
        valueDef.Value <- value
        valueDef

    static member Enum
        (name: string,
         cases: EnumValueDefinition list,
         ?description: string,
         ?deprecationReason: string) =
        let ``type`` = EnumerationGraphType () |> setBasicProps name description deprecationReason
        for case in cases do ``type``.AddValue case
        ``type``
