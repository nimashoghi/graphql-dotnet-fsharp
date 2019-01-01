module GraphQL.FSharp.Builder.Union

open System
open FSharp.Reflection
open GraphQL.Types
open Iris.Option.Builders

open GraphQL.FSharp
open GraphQL.FSharp.Builder.Helpers
open GraphQL.FSharp.Model

exception NotUnionException of Type

// TODO: nullable based off of option type
let internal createUnionType<'t> (schema: SchemaInfo) (case: UnionCaseInfo) =
    let field = case.GetFields().[0]
    // TODO: Check unsafe cast
    // TODO: nullable is true here, should it be?
    let result =
        getType schema field.PropertyType true
        |> Option.get
        :?> IObjectGraphType

    result

let internal getUnionCases<'t> (schema: SchemaInfo) =
    FSharpType.GetUnionCases typeof<'t>
    |> Array.map (fun case -> case, createUnionType<'t> schema case)

// Currently, we only support unions with only 1 field.
// TODO: come up with a better solution in the future
let internal checkUnionValidity<'t> () =
    let ``type`` = typeof<'t>

    FSharpType.GetUnionCases ``type``
    |> Array.tryPick (fun case ->
        let invalid =
            case.GetFields()
            |> Array.length
            |> (<>) 1
        if invalid then Some case else None)
    |> Option.iter (fun case ->
        failwithf
            "Union case \"%s.%s\" must have exactly 1 field."
            ``type``.Name
            case.Name)

let internal throwIfInvalid<'t> () =
    let ``type`` = typeof<'t>

    if not (FSharpType.IsUnion ``type``)
    then raise (NotUnionException ``type``)
    else checkUnionValidity<'t> ()

let wrapUnion<'t> =
    let ``type`` = typeof<'t>
    throwIfInvalid<'t> ()

    fun (schema: SchemaInfo) ->
        let union = TypedUnionGraphType<'t>()

        assert (``type``.Name <> null && ``type``.Name <> "")
        union.Name <- ``type``.Name

        let unionCases = getUnionCases<'t> schema
        assert (Array.length unionCases <> 0)

        union.ResolveType <-
            fun object ->
                assert (object <> null)

                match object with
                | :? 't as object ->
                    let unionCase, _ = FSharpValue.GetUnionFields(object, ``type``)
                    let unionCase =
                        unionCases
                        |> Array.filter (fst >> (=) unionCase)
                        |> Array.map (fun (i, j) -> Trace.Log "unionCase is %A" (i, j); i, j)
                        |> Array.map snd
                        |> Array.tryHead
                        |> Option.toObj

                    unionCase
                | _ -> null

        let unionCases = Array.map snd unionCases

        unionCases
        |> Array.iter (fun graphType -> union.AddPossibleType graphType)

        unionCases
        |> Array.map (fun case -> case :> IGraphType)
        |> schema.RegisterTypes

        schema.RegisterType union
        union :> IGraphType

type UnionWrapper = {
    name: string option
    cases: (SchemaInfo -> IGraphType) list
}

let newUnion : UnionWrapper = {
    name = None
    cases = []
}

type UnionBuilder() =
    member __.Yield _ = newUnion

    [<CustomOperation "name">]
    member __.Name (union, name) = {union with name = Some name}

    [<CustomOperation "case">]
    member __.Case (union, case) = {union with cases = case :: union.cases}

    [<CustomOperation "cases">]
    member __.Cases (union, cases) = {union with cases = cases @ union.cases}

    member __.Run ({name = name; cases = cases}: UnionWrapper) =
        fun (schema: SchemaInfo) -> maybeOrThrow {
            let graphType = UnionGraphType()

            let! name = name
            graphType.Name <- name

            let cases = List.map (fun f -> f schema) cases

            cases
            |> List.toArray
            |> schema.RegisterTypes

            cases
            // TODO: Fix this properly
            |> List.map (fun case -> case :?> IObjectGraphType)
            |> List.iter graphType.AddPossibleType

            return graphType :> IGraphType
        }
