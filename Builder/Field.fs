module GraphQL.FSharp.Builder.Field

open System
open GraphQL.Types
open GraphQL.Resolvers

let inline private set f (x: EventStreamFieldType) = f x; x

type FieldBuilder() =
    inherit BuilderBase<EventStreamFieldType>()

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

    [<CustomOperation "subscribe">]
    member __.Subscribe (field, subscribe) =
        let subscriber = EventStreamResolver<_, _> (Func<_, _> subscribe)
        set (fun x -> x.Subscriber <- subscriber) field

    [<CustomOperation "subscribeAsync">]
    member __.SubscribeAsync (field, subscribeAsync) =
        let asyncSubscriber = AsyncEventStreamResolver<_, _> (Func<_, _> subscribeAsync)
        set (fun x -> x.AsyncSubscriber <- asyncSubscriber) field

let field = FieldBuilder()
