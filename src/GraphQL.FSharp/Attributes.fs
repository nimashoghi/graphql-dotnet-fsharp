[<AutoOpen>]
module GraphQL.FSharp.Attributes

open System
open System.Collections.Generic

// TODO: Add attribute targets here

type AttributeWithValue<'t> (value) =
    inherit Attribute ()

    member val Value: 't = value with get, set

type NameAttribute (value) =
    inherit AttributeWithValue<string> (value)

type DescriptionAttribute (value) =
    inherit AttributeWithValue<string> (value)

type DeprecationReasonAttribute (value) =
    inherit AttributeWithValue<string> (value)

type MetadataAttribute (value) =
    inherit AttributeWithValue<IDictionary<string, obj>> (dict value)

type TypeAttribute (value) =
    inherit AttributeWithValue<Type> (value)

type DefaultValueAttribute (value) =
    inherit AttributeWithValue<obj> (value)

type ValueAttribute (value) =
    inherit AttributeWithValue<obj> (value)
