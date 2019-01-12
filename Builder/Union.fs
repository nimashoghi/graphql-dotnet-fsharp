module GraphQL.FSharp.Builder.Union

open System
open FSharp.Reflection
open GraphQL.Types
open GraphQL.Resolvers
open Iris.Option.Builders

open GraphQL.FSharp
open GraphQL.FSharp.Builder.Helpers

exception NotUnionException of Type

// TODO: Verify that i is the same as the index for fields
let internal unionFieldResolver<'t> (case: UnionCaseInfo) i =
    let resolveHelper (ctx: ResolveFieldContext) = maybe {
        let! source = Option.ofObj ctx.Source
        let unionCase, fields = FSharpValue.GetUnionFields(source, typeof<'t>)

        // TODO: verify
        if case <> unionCase then return! None else return fields.[i]
    }

    {new IFieldResolver with member __.Resolve ctx = Option.toObj (resolveHelper ctx)}

let internal createUnionType<'t> (schema: SchemaInfo) (case: UnionCaseInfo) =
    let object = ObjectGraphType()
    object.Name <- case.Name

    case.GetFields()
    |> Array.iteri (fun i field ->
        // TODO: nullable based off of option type
        let fieldType = FieldType()
        // TODO: should this name be `field.Name` or `case.Name` or just "Value"
        fieldType.Name <- field.Name
        fieldType.Resolver <- unionFieldResolver<'t> case i
        setType schema field.PropertyType false fieldType
        object.AddField fieldType |> ignore)
    object :> IObjectGraphType

let internal getUnionCases<'t> (schema: SchemaInfo) =
    FSharpType.GetUnionCases typeof<'t>
    |> Array.map (fun case -> case, createUnionType<'t> schema case)

// Currently, we only support unions with only 1 field.
// TODO: come up with a better solution in the future
// let internal checkUnionValidity<'t> () =
//     let ``type`` = typeof<'t>

//     FSharpType.GetUnionCases ``type``
//     |> Array.tryPick (fun case ->
//         let invalid =
//             case.GetFields()
//             |> Array.length
//             |> (<>) 1
//         if invalid then Some case else None)
//     |> Option.iter (fun case ->
//         failwithf
//             "Union case \"%s.%s\" must have exactly 1 field."
//             ``type``.Name
//             case.Name)

let internal throwIfInvalid<'t> () =
    let ``type`` = typeof<'t>

    if not (FSharpType.IsUnion ``type``)
    then raise (NotUnionException ``type``)

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
                    unionCases
                    |> Array.filter (fst >> (=) unionCase)
                    |> Array.map snd
                    |> Array.tryHead
                    |> Option.toObj
                | _ -> null

        let unionCases = Array.map snd unionCases

        unionCases
        |> Array.iter union.AddPossibleType

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
