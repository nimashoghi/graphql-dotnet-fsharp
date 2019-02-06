[<AutoOpen>]
module GraphQL.FSharp.Attributes

open System
open System.Collections.Generic

// TODO: Add attribute targets here

[<AttributeUsage (AttributeTargets.Method, Inherited = true)>]
type GetterAttribute () =
    inherit Attribute ()

[<AttributeUsage (AttributeTargets.Class, Inherited = true)>]
type AutoAttribute () =
    inherit Attribute ()

[<AttributeUsage (AttributeTargets.All, Inherited = true)>]
type FieldAttribute () =
    inherit Attribute ()

// FIXME: Should Inherited be true here?
[<AttributeUsage (AttributeTargets.All, Inherited = true)>]
type IgnoreAttribute () =
    inherit Attribute ()

[<AttributeUsage (AttributeTargets.All, Inherited = true)>]
type AttributeWithValue<'t> (value) =
    inherit Attribute ()

    member val Value: 't = value with get, set

[<AttributeUsage (AttributeTargets.All, Inherited = true)>]
type NameAttribute (value) =
    inherit AttributeWithValue<string> (value)

[<AttributeUsage (AttributeTargets.All, Inherited = true)>]
type DescriptionAttribute (value) =
    inherit AttributeWithValue<string> (value)

[<AttributeUsage (AttributeTargets.All, Inherited = true)>]
type DeprecationReasonAttribute (value) =
    inherit AttributeWithValue<string> (value)

[<AttributeUsage (AttributeTargets.All, Inherited = true)>]
type MetadataAttribute (value) =
    inherit AttributeWithValue<IDictionary<string, obj>> (dict value)

[<AttributeUsage (AttributeTargets.All, Inherited = true)>]
type TypeAttribute (value) =
    inherit AttributeWithValue<Type> (value)

[<AttributeUsage (AttributeTargets.All, Inherited = true)>]
type DefaultValueAttribute (value) =
    inherit AttributeWithValue<obj> (value)

[<AttributeUsage (AttributeTargets.All, Inherited = true)>]
type ValueAttribute (value) =
    inherit AttributeWithValue<obj> (value)
