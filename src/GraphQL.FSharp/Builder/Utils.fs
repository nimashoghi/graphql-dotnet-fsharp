module internal GraphQL.FSharp.BuilderUtils

open System
open GraphQL.Types

open GraphQL.FSharp.Inference
open GraphQL.FSharp.Types
open GraphQL.FSharp.Utils

module Field =
    let setType<'field, 'source> (field: Field<'field, 'source>) =
        if isNull field.ResolvedType
        then field.ResolvedType <- createReference typeof<'field>
        field

module Schema =
    let abstractClasses ``type`` =
        let rec run (``type``: Type) = [
            let baseType = ``type``.BaseType
            if baseType <> null && baseType <> typeof<obj> && baseType.IsAbstract then
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
        (objects, interfaces)
        ||> List.allPairs
        |> List.filter (fun ((_, object), (_, ``interface``)) -> extends object ``interface``)
        |> List.iter (fun ((object, _), (``interface``, _)) ->
            ``interface``.AddPossibleType object
            object.AddResolvedInterface ``interface``)

        types
