[<AutoOpen>]
module GraphQL.FSharp.Builder.Base

open System.Collections.Generic

[<AbstractClass>]
type BuilderBase< ^t when
                  ^t : (new: unit -> ^t) and
                  ^t : (member set_Name: string -> unit) and
                  ^t : (member set_Description: string -> unit)>() =
    member inline __.Yield _ = new ^t()

    [<CustomOperation "name">]
    member inline __.Name (x: ^t, name) =
        (^t : (member set_Name: string -> unit) x, name)
        x

    [<CustomOperation "description">]
    member inline __.Description (x: ^t, description) =
        (^t : (member set_Description: string -> unit) x, description)
        x
[<AbstractClass>]
type BuilderDeprecationReasonBase< ^t when
                                   ^t : (new: unit -> ^t) and
                                   ^t : (member set_Name: string -> unit) and
                                   ^t : (member set_Description: string -> unit) and
                                   ^t : (member set_DeprecationReason: string -> unit)>() =
    inherit BuilderBase< ^t>()

    [<CustomOperation "deprecationReason">]
    member inline __.DeprecationReason (x: ^t, deprecationReason) =
        (^t : (member set_DeprecationReason: string -> unit) x, deprecationReason)
        x

[<AbstractClass>]
type BuilderMetadataBase< ^t when
                          ^t : (new: unit -> ^t) and
                          ^t : (member set_Name: string -> unit) and
                          ^t : (member set_Description: string -> unit) and
                          ^t : (member set_DeprecationReason: string -> unit) and
                          ^t : (member set_Metadata: IDictionary<string, obj> -> unit)>() =
    inherit BuilderDeprecationReasonBase< ^t>()

    [<CustomOperation "metadata">]
    member inline __.Metadata (x: ^t, metadata: _ list) =
        (^t : (member set_Metadata: IDictionary<string, obj> -> unit) x, dict metadata)
        x
