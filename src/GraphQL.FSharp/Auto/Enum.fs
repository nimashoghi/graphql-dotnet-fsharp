module GraphQL.FSharp.AutoImplementation.Enum

open System
open FSharp.Reflection
open GraphQL.Types

open GraphQL.FSharp.Registry

let private isValidEnum<'enum> =
    typeof<'enum>.IsEnum ||
    (FSharpType.IsUnion typeof<'enum> &&
     FSharpType.GetUnionCases typeof<'enum>
     |> Array.forall (fun case ->
         case.GetFields ()
         |> Array.isEmpty))

let private (|Enum|Union|) (``type``: Type) =
    if ``type``.IsEnum
    then Enum
    elif FSharpType.IsUnion ``type``
    then Union
    else failwith "The provided type must be an enum or a discriminated union!"

let private unionEnum<'enum> () =
    let enum = EnumerationGraphType ()
    setInfo typeof<'enum> enum

    FSharpType.GetUnionCases typeof<'enum>
    |> Array.iter (fun case ->
        enum.AddValue (
            name = case.Name,
            description = null,
            value = FSharpValue.MakeUnion (case, [||])))

    enum

let Enum<'enum> =
    assert isValidEnum<'enum>

    let graphType =
        match typeof<'enum> with
        | Enum -> EnumerationGraphType<'enum> () :> EnumerationGraphType
        | Union -> unionEnum<'enum> ()

    Object.register (typeof<'enum>, graphType)

    graphType
