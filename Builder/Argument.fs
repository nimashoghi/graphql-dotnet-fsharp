[<AutoOpen>]
module rec GraphQL.FSharp.Builder.Argument

open System.Collections.Generic
open GraphQL.Types

open GraphQL.FSharp.Util.Inference

let inline private set f (x: QueryArgument) = f x; x

type ArgumentBuilder<'argument>(?``type``: IGraphType) =
    member __.Yield _ =
        match ``type`` with
        | Some ``type`` -> QueryArgument ``type``
        | None -> QueryArgument (infer typeof<'argument>)

    [<CustomOperation "name">]
    member __.Name (argument, name) =
        set (fun x -> x.Name <- name) argument

    [<CustomOperation "description">]
    member __.Description (argument, description) =
        set (fun x -> x.Description <- description) argument

    [<CustomOperation "defaultValue">]
    member __.DefaultValue (argument, ``default``: 'argument) =
        set (fun x -> x.DefaultValue <- ``default``) argument

    member inline  __.Get name (ctx: ^t) =
        let dictionary = (^t : (member Arguments: Dictionary<string, obj>) ctx)
        match dictionary.TryGetValue name with
        | true, value when (value :? 'argument) -> value :?> 'argument
        // TODO: Proper error message
        | _ -> failwith "Argument not found."

    member __.New (name, ?``default``) =
        let arg = QueryArgument (infer typeof<'argument>)
        arg.Name <- name
        Option.iter (fun ``default`` -> arg.DefaultValue <- box ``default``) ``default``
        arg

let argument<'argument> = ArgumentBuilder<'argument> ()
let argumentOf ``type`` = ArgumentBuilder ``type``
