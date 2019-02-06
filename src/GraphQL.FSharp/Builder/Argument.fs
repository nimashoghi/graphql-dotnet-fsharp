[<AutoOpen>]
module GraphQL.FSharp.Builder.Argument

open GraphQL.Types

open GraphQL.FSharp.Types
open GraphQL.FSharp.Inference

let inline private set f (x: #QueryArgument) = f (x :> QueryArgument); x
// TODO: Fix problem with FSharp list type as an argument

type QueryArgument with
    member this.Yield _ = this

    [<CustomOperation "name">]
    member __.CustomOperation_Name (arg, name) =
        set (fun x -> x.Name <- name) arg

    [<CustomOperation "description">]
    member __.CustomOperation_Description (arg, description) =
        set (fun x -> x.Description <- description) arg

    [<CustomOperation "defaultValue">]
    member __.CustomOperation_DefaultValue (arg, ``default``: 'arg) =
        set (fun x -> x.DefaultValue <- ``default``) arg

    [<CustomOperation "typed">]
    member __.CustomOperation_Type (arg, ``type``) =
        set (fun x -> x.ResolvedType <- ``type``) arg

let argument<'arg> = TypedQueryArgument<'arg> (createReference typeof<'arg>)
let argumentOf ``type`` = TypedQueryArgument ``type``
