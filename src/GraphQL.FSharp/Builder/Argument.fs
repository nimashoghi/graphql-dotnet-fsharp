[<AutoOpen>]
module rec GraphQL.FSharp.Builder.Argument

open System.Collections.Generic
open GraphQL.Types

open GraphQL.FSharp.Inference

let inline private set f (x: QueryArgument) = f x; x

type ArgumentBuilder<'arg when 'arg : (new: unit -> 'arg)>(?``type``: IGraphType) =
    member __.Yield _ =
        match ``type`` with
        | Some ``type`` -> QueryArgument ``type``
        | None -> QueryArgument (inferInput typeof<'arg>)

    [<CustomOperation "name">]
    member __.Name (arg, name) =
        set (fun x -> x.Name <- name) arg

    [<CustomOperation "description">]
    member __.Description (arg, description) =
        set (fun x -> x.Description <- description) arg

    [<CustomOperation "defaultValue">]
    member __.DefaultValue (arg, ``default``: 'arg) =
        set (fun x -> x.DefaultValue <- ``default``) arg

let arg<'arg when 'arg : (new: unit -> 'arg)> = ArgumentBuilder<'arg> ()
let argOf ``type`` = ArgumentBuilder ``type``