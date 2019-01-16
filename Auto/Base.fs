namespace GraphQL.FSharp

open System

[<RequireQualifiedAccess>]
type Strategy =
| OptIn
| OptOut

[<AttributeUsage(
    AttributeTargets.Parameter
    ||| AttributeTargets.Property
    ||| AttributeTargets.Field
    ||| AttributeTargets.Method,
    AllowMultiple = false,
    Inherited = true)>]
type OptOut() =
    inherit Attribute()

[<AttributeUsage(
    AttributeTargets.Property
    ||| AttributeTargets.Field
    ||| AttributeTargets.Method,
    AllowMultiple = false,
    Inherited = true)>]
type FieldAttribute() =
    inherit Attribute()

    member val Name: string = null with get, set
    member val Description: string = null with get, set
    member val DeprecationReason: string = null with get, set
    member val Metadata: (string * obj) list = [] with get, set
    member val Type: Type = null with get, set
    member val DefaultValue: obj = null with get, set
    member val ArgumentStrategy: Strategy = Strategy.OptOut with get, set

[<AttributeUsage(
    AttributeTargets.Interface
    ||| AttributeTargets.Class
    ||| AttributeTargets.Struct,
    AllowMultiple = false,
    Inherited = true)>]
type TypeAttribute() =
    inherit Attribute()

    member val Name: string = null with get, set
    member val Description: string = null with get, set
    member val DeprecationReason: string = null with get, set
    member val Metadata: (string * obj) list = [] with get, set
    member val FieldStrategy: Strategy = Strategy.OptOut with get, set

[<AttributeUsage(
    AttributeTargets.Parameter,
    AllowMultiple = false,
    Inherited = true)>]
type ArgumentAttribute() =
    inherit Attribute()

    member val Name: string = null with get, set
    member val Description: string = null with get, set
    member val Type: Type = null with get, set
    member val DefaultValue: obj = null with get, set

[<AutoOpenAttribute>]
module private AutoUtils =
    open GraphQL.Resolvers

    let resolve f =
        {
            new IFieldResolver with
                member this.Resolve ctx = f ctx |> box
        }
