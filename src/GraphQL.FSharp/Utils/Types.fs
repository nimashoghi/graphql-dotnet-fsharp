[<AutoOpen>]
module GraphQL.FSharp.Utils.Types

open System
open System.Reflection
open System.Runtime.CompilerServices
open FSharp.Reflection

let isAnonymousRecord (``type``: Type) =
    Attribute.IsDefined (``type``, typeof<CompilerGeneratedAttribute>, false)
    && ``type``.IsGenericType && ``type``.Name.Contains "AnonymousType"
    && ``type``.Name.StartsWith "<>"
    && ``type``.Attributes &&& TypeAttributes.NotPublic = TypeAttributes.NotPublic
    && FSharpType.IsRecord ``type``
