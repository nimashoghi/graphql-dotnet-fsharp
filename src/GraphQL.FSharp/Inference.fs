module GraphQL.FSharp.Inference

open System
open System.Text.RegularExpressions
open System.Reflection
open System.Runtime.CompilerServices
open FSharp.Reflection
open FSharp.Utils.Reflection
open GraphQL.Resolvers
open GraphQL.Types

open GraphQL.FSharp.Types

let rec unwrapType nonNullDefault get ``type`` =
    let graphType, isNull =
        match ``type`` with
        | NullableType innerType
        | ResultType (innerType, _)
        | OptionType innerType ->
            let ``type`` = unwrapType false get innerType
            ``type``, true

        | EnumerableType innerType ->
            let ``type`` =
                unwrapType nonNullDefault get innerType
                |> ListGraphType
                :> IGraphType
            ``type``, false

        | ObservableType innerType
        | TaskType innerType ->
            let ``type`` = unwrapType nonNullDefault get innerType
            ``type``, true

        | innerType ->
            let ``type`` = get innerType
            ``type``, false

    if nonNullDefault && not isNull
    then NonNullGraphType graphType :> IGraphType
    else graphType

let private graphType (f: unit -> #ScalarGraphType) = f >> (fun graph -> graph :> ScalarGraphType)

let defaultTypes = dict [
    typeof<string>, graphType StringGraphType
    typeof<bool>, graphType BooleanGraphType
    typeof<float>, graphType FloatGraphType
    typeof<float32>, graphType FloatGraphType
    typeof<decimal>, graphType DecimalGraphType
    typeof<int8>, graphType IntGraphType
    typeof<int16>, graphType IntGraphType
    typeof<int32>, graphType IntGraphType
    typeof<int64>, graphType IntGraphType
    typeof<uint8>, graphType IntGraphType
    typeof<uint16>, graphType IntGraphType
    typeof<uint32>, graphType IntGraphType
    typeof<uint64>, graphType IntGraphType
    typeof<Guid>, graphType IdGraphType
    typeof<DateTime>, graphType DateTimeGraphType
    typeof<DateTimeOffset>, graphType DateTimeOffsetGraphType
    typeof<TimeSpan>, graphType TimeSpanMillisecondsGraphType
]

// TODO: test this
let typeName (``type``: Type) =
    Regex.Replace (
        input = ``type``.Name,
        pattern = @"^I(\w+)Grain$",
        replacement = "$1"
    )

let getDefaultType ``type`` =
    match defaultTypes.TryGetValue ``type`` with
    | true, f -> Some (f ())
    | false, _ -> None

let getDefaultTypeOrReference ``type`` =
    getDefaultType ``type``
    |> Option.map (fun ``type`` -> ``type`` :> IGraphType)
    |> Option.defaultValue (GraphQLTypeReference (typeName ``type``) :> IGraphType)

let createReference ``type`` = unwrapType true getDefaultTypeOrReference ``type``

let isAnonymousRecord (``type``: Type) =
    Attribute.IsDefined (``type``, typeof<CompilerGeneratedAttribute>, false)
    && ``type``.IsGenericType && ``type``.Name.Contains("AnonymousType")
    && ``type``.Name.StartsWith("<>")
    && ``type``.Attributes &&& TypeAttributes.NotPublic = TypeAttributes.NotPublic
    && FSharpType.IsRecord ``type``

let makeAnonymousReturnType name ``type`` =
    let object =
        ObjectGraphType (
            Name = name
        )
    FSharpType.GetRecordFields ``type``
    |> Array.map (
        fun property ->
            Field (
                Name = property.Name,
                ResolvedType = createReference property.PropertyType,
                Resolver = FuncFieldResolver<_> (fun ctx -> property.GetValue ctx.Source)
            )
        )
    |> Array.iter (object.AddField >> ignore)

    object

let fieldName (field: Field<'field, 'arguments, 'source>) = sprintf "%s%sType" typeof<'source>.Name field.Name

let getDefaultTypeOrCreateAnonOrReference (field: Field<'field, 'arguments, 'source>) ``type`` =
    getDefaultType ``type``
    |> Option.map (fun ``type`` -> ``type`` :> IGraphType)
    |> Option.orElseWith (
        fun () ->
            if isAnonymousRecord ``type``
            then Some (upcast makeAnonymousReturnType (fieldName field) ``type``)
            else None
    )
    |> Option.defaultValue (GraphQLTypeReference (typeName ``type``) :> IGraphType)

let createReferenceForField (field: Field<'field, 'arguments, 'source>) =
    unwrapType true (getDefaultTypeOrCreateAnonOrReference field) typeof<'field>
