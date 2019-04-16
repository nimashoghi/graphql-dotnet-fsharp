[<AutoOpen>]
module GraphQL.FSharp.Builder.GraphType

open System
open System.Collections.Generic
open System.Reflection
open FSharp.Reflection
open FSharp.Utils
open GraphQL
open GraphQL.Resolvers
open GraphQL.Types

open GraphQL.FSharp.Builder.Utils
open GraphQL.FSharp.Inference
open GraphQL.FSharp.Types
open GraphQL.FSharp.Utils

//#region Object

let fields (fields: Field<'source> list) = Operation.ConfigureUnit <| fun (object: #ComplexGraphType<'source>) ->
    fields
    |> List.iter (object.AddField >> ignore)

[<RequireQualifiedAccess>]
module Object =
    let auto<'object> (_: _ list) = Operation.ConfigureUnit <| fun (object: Object<'object>) ->
        let ``type`` = typeof<'object>
        assert not ``type``.IsAbstract

        FSharpType.GetRecordFields ``type``
        |> Array.map (
            fun property ->
                let field =
                    Field<'object> (
                        Name = property.Name,
                        Resolver = FuncFieldResolver<obj> (fun ctx -> property.GetValue ctx.Source)
                    )
                field.ResolvedType <- inferField field property.PropertyType
                field
        )
        |> Array.iter (object.AddField >> ignore)

//#endregion

//#region Union

type UnionCase<'union> = {
    Init: Union<'union> -> Union<'union>
}

[<RequireQualifiedAccess>]
module Union =
    let case (case: 'object -> 'union) (``type``: Object<'object>) =
        assert not (isInvalidType ``type``)
        assert FSharpType.IsUnion typeof<'union>

        // TODO: optimize this
        let resolveUnion ``type`` (union: Union<'union>) =
            let previous =
                match union.ResolveType with
                | null -> fun _ -> null
                | f -> f.Invoke
            union.ResolveType <- fun object ->
                match previous object with
                | null when (object :? 'object) -> ``type``
                | _ -> null
            union

        {
            Init = fun union ->
                union.AddPossibleType ``type``
                resolveUnion ``type`` union
        }

    let cases (cases: UnionCase<'union> list) = Operation.Configure <| fun (union: Union<'union>) ->
        cases
        |> List.fold (fun union case -> case.Init union) union

    let (|RecordUnion|NormalUnion|) ``type`` =
        assert (FSharpType.IsUnion ``type`` && not ``type``.IsGenericType)
        let fields =
            FSharpType.GetUnionCases ``type``
            |> Array.map (fun case -> case.GetFields ())

        let isRecordUnion =
            fields
            |> Array.forall (
                fun fields ->
                    Array.length fields = 1
                    && FSharpType.IsRecord fields.[0].PropertyType
            )

        if isRecordUnion
        then RecordUnion
        else NormalUnion

    let addTag (tag: int) (object: Object<obj>) =
        object.AddField (
            Field (
                Name = "_Tag",
                ResolvedType = NonNullGraphType (IntGraphType ()),
                Resolver = FuncFieldResolver<obj> (fun _ -> box tag)
            )
        )
        |> ignore

    let getCaseName (case: UnionCaseInfo) = sprintf "%s%s" case.DeclaringType.Name case.Name

    let autoRecord<'union> (_: _ list) = Operation.ConfigureUnit <| fun (union: Union<'union>) ->
        let ``type`` = typeof<'union>

        let cases = FSharpType.GetUnionCases ``type``

        if isNull union.Name then union.Name <- ``type``.Name

        let getFirstField (case: UnionCaseInfo) =
            let fields = case.GetFields ()
            assert (Array.length fields = 1)
            let field = fields.[0]
            assert FSharpType.IsRecord field.PropertyType
            field

        let caseObjectTypes =
            cases
            |> Array.map (
                fun case ->
                    let object =
                        Object<obj> (
                            Name = getCaseName case
                        )

                    let reader = FSharpValue.PreComputeUnionReader case
                    let reader object = reader object |> Array.head

                    let field = getFirstField case
                    FSharpType.GetRecordFields field.PropertyType
                    |> Array.map (
                        fun property ->
                            Field (
                                Name = property.Name,
                                ResolvedType = infer property.PropertyType,
                                Resolver = FuncFieldResolver<obj> (fun ctx -> property.GetValue (reader ctx.Source))
                            )
                    )
                    |> Array.iter (object.AddField >> ignore)

                    addTag case.Tag object
                    object
            )

        for object in caseObjectTypes do union.AddPossibleType object

        let caseTypes =
            cases
            |> Array.map (fun case -> case.Tag)

        let typeMapping =
            (caseTypes, caseObjectTypes)
            ||> Array.zip
            |> dict

        let tagReader = FSharpValue.PreComputeUnionTagReader ``type``
        union.ResolveType <-
            fun object ->
                if not <| ``type``.IsAssignableFrom (object.GetType ()) then null else
                match typeMapping.TryGetValue (tagReader object) with
                | true, graph -> upcast graph
                | false, _ -> null

    let autoNormal<'union> (_: _ list) = Operation.ConfigureUnit <| fun (union: Union<'union>) ->
        let ``type`` = typeof<'union>

        if isNull union.Name then union.Name <- ``type``.Name

        let cases = FSharpType.GetUnionCases ``type``
        let tagReader = FSharpValue.PreComputeUnionTagReader ``type``

        let getItemName names (property: PropertyInfo) =
            let getName (names: string HashSet) baseName =
                let rec run (names: string HashSet) baseName i =
                    let name = sprintf "%s%i" baseName i

                    if names.Add name
                    then i + 1 |> run names baseName
                    else name

                if names.Add baseName
                then baseName
                else run names baseName 1

            let getName baseName = getName names baseName

            if not <| property.Name.StartsWith "Item"
            then getName property.Name
            else getName property.PropertyType.Name

        let caseObjectTypes =
            cases
            |> Array.map (
                fun case ->
                    let object =
                        Object<obj> (
                            Name = getCaseName case
                        )

                    let reader = FSharpValue.PreComputeUnionReader case
                    let reader i object = Array.get (reader object) i
                    let isValidUnion object = tagReader object = case.Tag

                    case.GetFields ()
                    |> Array.mapi (
                        fun i property ->
                            let reader object = reader i object
                            let names = HashSet []

                            Field (
                                Name = getItemName names property,
                                ResolvedType = infer property.PropertyType,
                                Resolver =
                                    FuncFieldResolver<obj> (
                                        fun ctx ->
                                            if isValidUnion ctx.Source then reader ctx.Source else
                                                if isNull ctx.Errors
                                                then ctx.Errors <- ExecutionErrors ()

                                                sprintf "Field %s does not exist on this union case!" property.Name
                                                |> ExecutionError
                                                |> ctx.Errors.Add

                                                null
                                    )
                            )
                    )
                    |> Array.iter (object.AddField >> ignore)

                    addTag case.Tag object
                    object
            )

        for object in caseObjectTypes do union.AddPossibleType object

        let caseTypes =
            cases
            |> Array.map (fun case -> case.Tag)

        let typeMapping =
            (caseTypes, caseObjectTypes)
            ||> Array.zip
            |> dict

        union.ResolveType <-
            fun object ->
                match object with
                | :? 'union as union ->
                    match typeMapping.TryGetValue (tagReader union) with
                    | true, graph -> upcast graph
                    | false, _ -> null
                | _ -> null

    let auto<'union> (args: _ list) = flatten [
        match typeof<'union> with
        | NormalUnion -> yield autoNormal<'union> args
        | RecordUnion -> yield autoRecord<'union> args
    ]

//#endregion

//#region Enum

let inline value (value: ^value) = Operation.ConfigureUnit <| fun target ->
    (^t: (member set_Value: ^value -> unit) target, value)

[<RequireQualifiedAccess>]
module Enum =
    let case (case: 'enum) (properties: IOperation<EnumerationValue<'enum>> list) =
        let isValidEnum (``type``: Type) =
            ``type``.IsEnum
            || FSharpType.IsUnion ``type``

        assert isValidEnum typeof<'enum>

        let getEnumValueName (case: 'enum) =
            if typeof<'enum>.IsEnum
            then Enum.GetName (typeof<'enum>, box case)
            else
                let case =
                    FSharpValue.GetUnionFields (case, typeof<'enum>)
                    |> fst
                case.Name

        properties
        |> List.append [
            name (getEnumValueName case)
            Operation.Configure (fun value -> value.Value <- box case; value)
        ]
        |> reduceWith EnumerationValue<'enum>

    let cases (values: EnumerationValue<'enum> list) = Operation.ConfigureUnit <| fun (enum: Enumeration<'enum>) ->
        enum.Name <- typeof<'enum>.Name
        List.iter enum.AddValue values

    let auto<'enum> (_: _ list) = Operation.ConfigureUnit <| fun (enum: Enumeration<'enum>) ->
        let ``type`` = typeof<'enum>

        let (|Enum|UnionEnum|) (``type``: Type) =
            if ``type``.IsEnum then Enum
            else if FSharpType.IsUnion ``type`` then
                assert
                    (
                        FSharpType.GetUnionCases ``type``
                        |> Array.map (fun case -> case.GetFields ())
                        |> Array.forall Array.isEmpty
                    )
                UnionEnum
            else failwith "Invalid type!"

        match ``type`` with
        | Enum ->
            Enum.GetNames ``type``
            |> Array.map (
                fun name ->
                    EnumerationValue<'enum> (
                        Name = name,
                        Value = Enum.Parse (``type``, name)
                    )
            )
            |> Array.iter enum.AddValue
        | UnionEnum ->
            FSharpType.GetUnionCases ``type``
            |> Array.map (
                fun case ->
                    EnumerationValue<'enum> (
                        Name = case.Name,
                        Value = FSharpValue.MakeUnion (case, [||])
                    )
            )
            |> Array.iter enum.AddValue

//#endregion

//#region Schema

let internal operation name description set (endpoints: Field<obj> list) = Operation.ConfigureUnit <| fun (schema: #Schema) ->
    let object =
        Object<obj> (
            Name = name,
            Description = description
        )

    endpoints
    |> List.iter (object.AddField >> ignore)

    set schema object

let query endpoints = operation "Query" "The queries accepted in this GraphQL API" (fun schema value -> schema.Query <- value) endpoints
let mutation endpoints = operation "Mutation" "The mutations accepted in this GraphQL API" (fun schema value -> schema.Mutation <- value) endpoints
let subscription endpoints = operation "Subscription" "The subscriptions accepted in this GraphQL API" (fun schema value -> schema.Subscription <- value) endpoints

let types (types: IGraphType list) = Operation.ConfigureUnit <| fun (schema: #Schema) ->
    let abstractClasses ``type`` =
        let rec run (``type``: Type) = [
            let baseType = ``type``.BaseType
            if not (isNull baseType)
            && baseType <> typeof<obj>
            && baseType.IsAbstract then
                yield baseType
                yield! run baseType
        ]
        run ``type``

    let baseTypes (``type``: Type) = [
        yield! ``type``.GetInterfaces ()
        yield! abstractClasses ``type``
    ]

    let extends (object: Type) (``base``: Type) =
        baseTypes object
        |> List.exists ((=) ``base``)

    let handleInterfaces (types: IGraphType list) =
        let objects = types |> List.choose (|ObjectGraphType|_|)
        let interfaces = types |> List.choose (|InterfaceGraphType|_|)
        List.allPairs objects interfaces
        |> List.filter (fun ((_, object), (_, ``interface``)) -> extends object ``interface``)
        |> List.iter (
            fun ((object, _), (``interface``, _)) ->
                ``interface``.AddPossibleType object
                object.AddResolvedInterface ``interface``
        )

        types

    types
    |> handleInterfaces
    |> List.toArray
    |> schema.RegisterTypes

//#endregion
