[<AutoOpen>]
module GraphQL.FSharp.ValueConverters

open FSharp.Reflection
open GraphQL

open GraphQL.FSharp.Resolvers

let getCase<'t> name types =
    typedefof<'t>.MakeGenericType types
    |> FSharpType.GetUnionCases
    |> Array.find (fun case -> case.Name = name)

module Option =
    let main () =
        ValueConverter.RegisterWrap (
            wrappedTypeDef = typedefof<_ option>,
            conversion = fun value types ->
                match value with
                | null ->
                    FSharpValue.MakeUnion (
                        unionCase = getCase<_ option> "None" types,
                        args = [||]
                    )
                | value ->
                    FSharpValue.MakeUnion (
                        unionCase = getCase<_ option> "Some" types,
                        args = [|value|]
                    )
        )
        ValueConverter.RegisterUnwrap (
            valueTypeDef = typedefof<_ option>,
            conversion = fun value _ ->
                match value with
                | Option (_, Some value) -> value
                | _ -> null
        )

module Result =
    let main () =
        ValueConverter.RegisterWrap (
            wrappedTypeDef = typedefof<Result<_, _>>,
            conversion = fun value types ->
                match value with
                | null -> failwith "Result value cannot be null!"
                | value ->
                    FSharpValue.MakeUnion (
                        unionCase = getCase<Result<_, _>> "Ok" types,
                        args = [|value|]
                    )
        )
        ValueConverter.RegisterUnwrap (
            valueTypeDef = typedefof<Result<_, _>>,
            conversion = fun value _ ->
                match value with
                | Result (_, _, result) ->
                    match result with
                    | Ok value -> value
                    | Error error -> failwith (string error)
                | _ -> null
        )
