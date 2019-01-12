module GraphQL.FSharp.Builder.Field

open System
open GraphQL.Types
open GraphQL.Resolvers

let inline private set f (x: FieldType) = f x; x

type FieldBuilder() =
    member __.Yield _ = FieldType()

    [<CustomOperation "name">]
    member __.Name (field, name) = set (fun x -> x.Name <- name) field

    [<CustomOperation "description">]
    member __.Description (field, description) = set (fun x -> x.Description <- description) field

    [<CustomOperation "type">]
    member __.Type (field, ``type``) = set (fun x -> x.Type <- ``type``) field
    member __.Type (field, ``type``) = set (fun x -> x.ResolvedType <- ``type``) field

    [<CustomOperation "defaultValue">]
    member __.DefaultValue (field, ``default``) = set (fun x -> x.DefaultValue <- ``default``) field

    [<CustomOperation "arguments">]
    member __.Arguments (field, arguments: _ list) = set (fun x -> x.Arguments <- QueryArguments arguments) field

    [<CustomOperation "resolve">]
    member __.Resolve (field, resolver) =
        let resolver = FuncFieldResolver<_, _> (Func<_, _> resolver)
        set (fun x -> x.Resolver <- resolver) field
