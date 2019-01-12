module GraphQL.FSharp.Builder.Field

open System
open GraphQL.Types
open GraphQL.Resolvers

let inline private set f (x: FieldType) = f x; x

type FieldBuilder() =
    inherit BuilderBase<FieldType>()

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

let field = FieldBuilder()
