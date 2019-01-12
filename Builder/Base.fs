[<AutoOpen>]
module GraphQL.FSharp.Builder.Base

[<AbstractClass>]
type BuilderBase< ^t when ^t : (new: unit -> ^t) and ^t : (member Name: string) and ^t : (member Description: string) and ^t : (member DeprecationReason: string)>() =
    member inline __.Yield _ = new ^t()

    [<CustomOperation "name">]
    member inline __.Name (x: ^t, name) = (^t : (member Name: string) x) <- name

    [<CustomOperation "description">]
    member inline __.Description (x: ^t, description) = (^t : (member Description: string) x) <- description

    [<CustomOperation "deprecationReason">]
    member inline __.DeprecationReason (x: ^t, deprecationReason) = (^t : (member DeprecationReason: string) x) <- deprecationReason
