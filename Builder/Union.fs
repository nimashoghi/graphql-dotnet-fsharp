module GraphQL.FSharp.Builder.Union

open System
open FSharp.Reflection
open GraphQL.Types

open GraphQL.FSharp.Model

exception NotUnionException of Type


open GraphQL.Resolvers
open System.Reflection
open Iris.Option.Builders

let internal unionFieldResolver<'t> (case: UnionCaseInfo) (field: PropertyInfo) =
    let resolveHelper (ctx: ResolveFieldContext) = maybe {
        let! ast = Option.ofObj ctx.FieldAst
        let! name = Option.ofObj ast.Name
        let! source = Option.ofObj ctx.Source

        let unionCase, fields = FSharpValue.GetUnionFields(source, typeof<'t>)

        if case <> unionCase then return! None else
        // TODO: Look at this -- unsafe
        return fields.[0]
    }

    { new IFieldResolver with member __.Resolve ctx = Option.toObj (resolveHelper ctx) }

open GraphQL.FSharp.Builder.Helpers

let internal createUnionType<'t> (schema: SchemaInfo) (case: UnionCaseInfo) =
    let object = ObjectGraphType()
    object.Name <- case.Name
    object.Metadata.["unionCaseInfo"] <- Some case

    for field in case.GetFields() do
        // TODO: nullable based off of option type
        let fieldType = FieldType()
        fieldType.Name <- field.Name
        fieldType.Resolver <- unionFieldResolver<'t> case field
        setType schema field.PropertyType false fieldType
        object.AddField fieldType |> ignore
    object


let internal getUnionCases<'t> (schema: SchemaInfo) =
    FSharpType.GetUnionCases typeof<'t>
    |> Array.map (createUnionType<'t> schema)

let internal throwIfNotUnion<'t> () =
    let ``type`` = typeof<'t>

    if not (FSharpType.IsUnion ``type``)
    then raise (NotUnionException ``type``)

let wrapUnion<'t> =
    let ``type`` = typeof<'t>
    throwIfNotUnion<'t> ()

    fun (schema: SchemaInfo) ->
        let union = UnionGraphType()
        union.Name <- ``type``.Name

        let unionCases = getUnionCases<'t> schema
        union.ResolveType <-
            fun object ->
                if object :? 't then
                    let unionCase, _ = FSharpValue.GetUnionFields(object, ``type``)
                    let unionCaseGraphType =
                        unionCases
                        |> Array.tryPick (fun graphType ->
                            graphType.GetMetadata<UnionCaseInfo option> "unionCaseInfo"
                            |> Option.filter (fun unionCase' -> unionCase = unionCase')
                            |> Option.map (fun _ -> graphType))

                    match unionCaseGraphType with
                    | Some unionCaseGraphType -> unionCaseGraphType :> IObjectGraphType
                    | _ -> null
                else null

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
