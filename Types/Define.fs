[<AutoOpen>]
module GraphQL.FSharp.Types.Define

open GraphQL.Types

let inline setNameDescription name description (``type``: ^t) =
    (^t : (member set_Name: string -> unit) ``type``, name)
    Option.iter
        (fun description ->
            (^t : (member set_Description: string -> unit) ``type``, description))
        description
    ``type``

let inline setBasicProps name description deprecationReason (``type``: ^t) =
    let ``type`` = setNameDescription name description ``type``
    Option.iter
        (fun deprecationReason ->
            (^t : (member set_DeprecationReason: string -> unit) ``type`` ,deprecationReason))
        deprecationReason
    ``type``

type IGraphType with
    member this.Nullable
        with get () = this.GetMetadata<bool> ("Nullable", false)
        and set value = this.Metadata.["Nullable"] <- value

type Define private () = class end
