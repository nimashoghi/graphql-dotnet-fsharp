module GraphQL.FSharp.Tests.Assert

open NUnit.Framework
open Swensen.Unquote
open GraphQL.Types

let graph (f: unit -> #IGraphType) = fun () -> f () :> IGraphType
let liftGraph (``type``: #IGraphType) = fun () -> ``type`` :> IGraphType

let inline private graphEqual (name, description, deprecationReason, metadata) (graph: #IGraphType) =
    name |> Option.iter (fun name -> graph.Name =! name)
    description |> Option.iter (fun description -> graph.Description =! description)
    deprecationReason |> Option.iter (fun deprecationReason -> graph.DeprecationReason =! deprecationReason)
    metadata |> Option.iter (fun metadata -> graph.Metadata =! metadata)

type Assert with
    static member UnionGraphEqual (?name, ?cases) =
        fun (union: UnionGraphType) ->
            name |> Option.iter (fun name -> union.Name =! name)
            cases |> Option.iter (fun cases ->
                Seq.length union.PossibleTypes =! Seq.length cases

                (cases, union.PossibleTypes)
                ||> Seq.zip
                |> Seq.iter (fun ((name, fields), object) ->
                    object
                    :> IComplexGraphType
                    |> Assert.ObjectGraphEqual (
                        name = name,
                        fields = fields
                    )
                )
            )

    static member EnumGraphEqual (?name, ?values, ?description, ?deprecationReason, ?metadata) =
        fun (enum: EnumerationGraphType) ->
            graphEqual (name, description, deprecationReason, metadata) enum
            values |> Option.iter (fun values ->
                Seq.length enum.Values =! List.length values

                let values = dict values
                for enumValue in enum.Values do
                    test <@ values.ContainsKey enumValue.Name @>
                    enumValue.Value =! values.[enumValue.Name])

    static member ObjectGraphEqual (?name, ?fields, ?description, ?deprecationReason, ?metadata) =
        fun (object: IComplexGraphType) ->
            graphEqual (name, description, deprecationReason, metadata) object
            fields |> Option.iter (fun fields ->
                Seq.length object.Fields =! List.length fields

                let ``types`` =
                    fields
                    |> List.map (fun (name, ``type``) -> name, ``type`` ())
                    |> dict

                for field in object.Fields do
                    test <@ ``types``.ContainsKey field.Name @>
                    field.ResolvedType =! ``types``.[field.Name])

    static member ArgumentEqual (?name, ?``type``, ?defaultValue, ?description) =
        fun (argument: QueryArgument) ->
            name |> Option.iter (fun name -> argument.Name =! name)
            ``type`` |> Option.iter (fun ``type`` -> argument.ResolvedType =! ``type``)
            description |> Option.iter (fun description -> argument.Description =! description)
            defaultValue |> Option.iter (fun defaultValue -> argument.DefaultValue =! defaultValue)


let unionEqual name cases union =
    union
    |> Assert.UnionGraphEqual (
        name = name,
        cases = cases
    )

let enumEqual name values (enum: #EnumerationGraphType) =
    enum
    :> EnumerationGraphType
    |> Assert.EnumGraphEqual (
        name = name,
        values = values
    )

let objectEqual name fields (object: #IComplexGraphType) =
    object
    :> IComplexGraphType
    |> Assert.ObjectGraphEqual (
        name = name,
        fields = fields
    )

let argumentEqual name ``type`` defaultValue argument =
    argument
    |> Assert.ArgumentEqual (
        name = name,
        ``type`` = ``type`` (),
        ?defaultValue = defaultValue
    )
