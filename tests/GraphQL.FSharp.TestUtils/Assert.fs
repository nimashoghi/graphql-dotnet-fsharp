module GraphQL.FSharp.TestUtils.Assert

open GraphQL
open GraphQL.Types
open JsonDiffPatchDotNet
open Newtonsoft.Json
open Newtonsoft.Json.Linq
open NUnit.Framework
open Swensen.Unquote

let nullable (f: unit -> #IGraphType) = fun () -> f () :> IGraphType

let nonNull (f: unit -> #IGraphType) = fun () -> NonNullGraphType (f ()) :> IGraphType
let liftNonNull (``type``: #IGraphType) = fun () -> NonNullGraphType ``type`` :> IGraphType

let inline private graphEqual (name, description, deprecationReason, metadata) (graph: #IGraphType) =
    name |> Option.iter (fun name -> graph.Name =! name)
    description |> Option.iter (fun description -> graph.Description =! description)
    deprecationReason |> Option.iter (fun deprecationReason -> graph.DeprecationReason =! deprecationReason)
    metadata |> Option.iter (fun metadata -> graph.Metadata =! metadata)

type Assert with
    static member UnionGraphEqual (?name, ?cases, ?description, ?deprecationReason, ?metadata) =
        fun (union: UnionGraphType) ->
            graphEqual (name, description, deprecationReason, metadata) union
            cases |> Option.iter (fun cases ->
                Seq.length union.PossibleTypes =! Seq.length cases

                (cases, union.PossibleTypes)
                ||> Seq.zip
                |> Seq.iter (fun ((name, fields), object) ->
                    let fields =
                        [
                            yield! fields
                            yield "Tag", nonNull IntGraphType
                        ]
                        |> List.map (fun (name, value) -> name, null, value)

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

                let values = dict (values |> List.map (fun (name, description, value) -> name, (description, value)))
                for enumValue in enum.Values do
                    test <@ values.ContainsKey enumValue.Name @>
                    let description, value = values.[enumValue.Name]
                    enumValue.Description =! description
                    enumValue.Value =! value)

    static member ObjectGraphEqual (?name, ?fields, ?description, ?deprecationReason, ?metadata) =
        fun (object: IComplexGraphType) ->
            graphEqual (name, description, deprecationReason, metadata) object
            fields |> Option.iter (fun fields ->
                Seq.length object.Fields =! List.length fields

                let types =
                    fields
                    |> List.map (fun (name, description, ``type``) -> name, (description, ``type`` ()))
                    |> dict

                for field in object.Fields do
                    test <@ types.ContainsKey field.Name @>
                    let description, ``type`` = types.[field.Name]
                    field.Description =! description
                    field.ResolvedType =! ``type``)

    static member FieldEqual (?name, ?``type``, ?description, ?deprecationReason, ?metadata) =
        fun (field: FieldType) ->
            name |> Option.iter (fun name -> field.Name =! name)
            description |> Option.iter (fun description -> field.Description =! description)
            deprecationReason |> Option.iter (fun deprecationReason -> field.DeprecationReason =! deprecationReason)
            metadata |> Option.iter (fun metadata -> field.Metadata =! metadata)
            ``type`` |> Option.iter (fun ``type`` -> field.ResolvedType =! ``type`` ())

    static member ArgumentEqual (?name, ?``type``, ?defaultValue, ?description) =
        fun (argument: QueryArgument) ->
            name |> Option.iter (fun name -> argument.Name =! name)
            ``type`` |> Option.iter (fun ``type`` -> argument.ResolvedType =! ``type``)
            description |> Option.iter (fun description -> argument.Description =! description)
            defaultValue |> Option.iter (fun defaultValue -> argument.DefaultValue =! defaultValue)

    static member JsonEqual (expected, result) =
        let cut x =
            x
            // if String.length x >= 256
            // then sprintf "%s..." x.[0 .. (255 - String.length "...")]
            // else x
        let toString (token: JToken) =
            JsonConvert.SerializeObject (
                token,
                Formatting.Indented
            )
            |> cut
        let replaceNewLines (str: string) = str.Replace (@"\r\n", @"\n")

        let result = JObject.Parse (replaceNewLines result)
        let expected = JObject.Parse (replaceNewLines expected)
        if not <| JToken.DeepEquals (result, expected) then
            sprintf
                "expected\n\n%s\n\nbut got\n\n%s\n\ndiff:\n\n%s"
                (toString expected)
                (toString result)
                (toString <| JsonDiffPatch().Diff (result, expected))
            |> Assert.Fail

    static member QueryEqual (query, expected, ?root) =
        fun (schema: Schema) ->
            let root =
                root
                |> Option.orElse (Some null)
                |> Option.get
            let result =
                schema.Execute (fun options ->
                    options.Schema <- schema
                    options.Query <- query
                    options.Root <- root
                    options.FieldNameConverter <- schema.FieldNameConverter
                )

            Assert.JsonEqual (
                expected = expected,
                result = result
            )

let jsonEqual expected result =
    Assert.JsonEqual (
        expected = expected,
        result = result
    )

let queryEqual query expected schema =
    schema
    |> Assert.QueryEqual (
        query = query,
        expected = expected
    )

let fieldEqual name ``type`` field =
    field
    |> Assert.FieldEqual (
        name = name,
        ``type`` = ``type``
    )

let unionEqual name cases (union: #UnionGraphType) =
    union
    :> UnionGraphType
    |> Assert.UnionGraphEqual (
        name = name,
        cases = cases
    )

let enumEqual name values (enum: #EnumerationGraphType) =
    let values =
        values
        |> List.map (fun (name, value) -> name, null, value)

    enum
    :> EnumerationGraphType
    |> Assert.EnumGraphEqual (
        name = name,
        values = values
    )

let objectEqual name fields (object: #IComplexGraphType) =
    let fields =
        fields
        |> List.map (fun (name, ``type``) -> name, null, ``type``)

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
