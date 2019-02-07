[<AutoOpen>]
module GraphQL.FSharp.Builder.Argument

open GraphQL.Types

open GraphQL.FSharp.Builder.Base
open GraphQL.FSharp.Inference
open GraphQL.FSharp.Types

let inline private set f (x: #QueryArgument) = f (x :> QueryArgument); x
// TODO: Fix problem with FSharp list type as an argument

type TypedQueryArgument<'t> with
    member this.Yield (_: unit) = ``yield`` this

    [<CustomOperation "name">]
    member __.CustomOperation_Name (this: TypedQueryArgument<'t>, name) =
        set (fun x -> x.Name <- name) this

    [<CustomOperation "description">]
    member __.CustomOperation_Description (this: TypedQueryArgument<'t>, description) =
        set (fun x -> x.Description <- description) this

    [<CustomOperation "defaultValue">]
    member __.CustomOperation_DefaultValue (this: TypedQueryArgument<'t>, ``default``: 't) =
        set (fun x -> x.DefaultValue <- ``default``) this

    [<CustomOperation "typed">]
    member __.CustomOperation_Type (this: TypedQueryArgument<'t>, ``type``) =
        set (fun x -> x.ResolvedType <- ``type``) this

let argument<'t> = builder (fun () -> TypedQueryArgument<'t> (createReference typeof<'t>))
let argumentOf ``type`` = builder (fun () -> TypedQueryArgument<obj> ``type``)
