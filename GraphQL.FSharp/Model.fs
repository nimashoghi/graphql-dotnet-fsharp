module GraphQL.FSharp.Model

open System.Collections.Concurrent

open Iris.Option

type UserContext() =
    let properties = ConcurrentDictionary<string, obj>()

    member __.Get<'t> name =
        match properties.TryGetValue name with
        | TryGet.Some value when (value :? 't) -> Some (value :?> 't)
        | _ -> None

    member __.Set name value = properties.[name] <- box value
