module GraphQL.FSharp.Builder.Argument

open GraphQL.Types

let inline private set f (x: QueryArgument<_>) = f x; x

type ArgumentBuilder(``type``: IGraphType) =
    member __.Yield _ = QueryArgument ``type``

    [<CustomOperation "name">]
    member __.Name (argument, name) =
        set (fun x -> x.Name <- name) argument

    [<CustomOperation "description">]
    member __.Description (argument, description) =
        set (fun x -> x.Description <- description) argument

    [<CustomOperation "defaultValue">]
    member __.DefaultValue (argument, ``default``) =
        set (fun x -> x.DefaultValue <- ``default``) argument

let argument ``type`` = ArgumentBuilder ``type``
