module GraphQL.FSharp.Builder.Argument

open GraphQL.Types

open GraphQL.FSharp.Logging

let inline private set f (x: QueryArgument<_>) = f x; x

type ArgumentBuilder<'input when 'input :> IGraphType>() =
    member __.Yield _ = QueryArgument<'input>()

    [<CustomOperation "name">]
    member __.Name (argument, name) =
        Trace.Log "Argument name set to %s" name
        set (fun x -> x.Name <- name) argument

    [<CustomOperation "description">]
    member __.Description (argument: QueryArgument<_>, description) =
        Trace.Log "Argument \"%s\"'s description set to %s" argument.Name description
        set (fun x -> x.Description <- description) argument

    [<CustomOperation "defaultValue">]
    member __.DefaultValue (argument: QueryArgument<_>, ``default``) =
        Trace.Log "Argument \"%s\"'s default value set to %A" argument.Name ``default``
        set (fun x -> x.DefaultValue <- ``default``) argument

let argument<'input when 'input :> IGraphType> = ArgumentBuilder<'input>()
