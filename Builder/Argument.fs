module GraphQL.FSharp.Builder.Argument

open GraphQL.Types

let inline private set f (x: QueryArgument<_>) = f x; x

type ArgumentBuilder(``type``: IGraphType) =
    inherit BuilderFuncBase<QueryArgument>(fun () -> QueryArgument ``type``)

    [<CustomOperation "defaultValue">]
    member __.DefaultValue (argument: QueryArgument<_>, ``default``) =
        set (fun x -> x.DefaultValue <- ``default``) argument

let argument ``type`` = ArgumentBuilder ``type``
