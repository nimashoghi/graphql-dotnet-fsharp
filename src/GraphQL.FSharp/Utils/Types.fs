[<AutoOpen>]
module GraphQL.FSharp.Utils.Types

open System
open System.Reflection
open System.Runtime.CompilerServices
open FSharp.Reflection

let isAnonymous (``type``: Type) =
    FSharpType.IsRecord ``type``
    && Attribute.IsDefined (``type``, typeof<CompilerGeneratedAttribute>, false)
    && ``type``.IsGenericType && ``type``.Name.Contains "AnonymousType"
    && ``type``.Name.StartsWith "<>"
    && ``type``.Attributes &&& TypeAttributes.NotPublic = TypeAttributes.NotPublic

let (|NormalRecord|AnonymousRecord|NotRecord|) (``type``: Type) =
    if not <| FSharpType.IsRecord ``type`` then NotRecord
    else if isAnonymous ``type`` then AnonymousRecord
    else NormalRecord
