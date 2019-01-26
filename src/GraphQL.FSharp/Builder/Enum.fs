[<AutoOpen>]
module GraphQL.FSharp.Builder.Enum

open GraphQL.Types

let inline private set f (x: EnumerationGraphType) = f x; x

type EnumBuilder () =
    inherit BuilderMetadataBase<EnumerationGraphType >()

    [<CustomOperation "cases">]
    member __.Cases (enum, values) =
        set (fun enum ->
            values
            |> List.iter (fun (name, value) -> enum.AddValue (name, null, box value))) enum

let enum = EnumBuilder ()
