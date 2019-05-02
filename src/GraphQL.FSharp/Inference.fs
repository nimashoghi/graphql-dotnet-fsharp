module rec GraphQL.FSharp.Inference

open System
open System.Collections.Generic
open System.Reflection
open System.Runtime.CompilerServices
open FSharp.Reflection
open FSharp.Utils.Reflection
open GraphQL.Resolvers
open GraphQL.Types

open GraphQL.FSharp.Types
open GraphQL.FSharp.Utils

let rec internal unwrapType nonNullDefault get ``type`` =
    let graphType, isNull =
        match ``type`` with
        | NullableType innerType
        | ResultType (innerType, _)
        | OptionType innerType
        | ValueOptionType innerType ->
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

let internal defaultTypes: IDictionary<Type, IGraphType> =
    dict [
        typeof<string>, upcast (StringGraphType ())
        typeof<bool>, upcast (BooleanGraphType ())
        typeof<float>, upcast (FloatGraphType ())
        typeof<float32>, upcast (FloatGraphType ())
        typeof<decimal>, upcast (DecimalGraphType ())
        typeof<int8>, upcast (IntGraphType ())
        typeof<int16>, upcast (IntGraphType ())
        typeof<int32>, upcast (IntGraphType ())
        typeof<int64>, upcast (IntGraphType ())
        typeof<uint8>, upcast (IntGraphType ())
        typeof<uint16>, upcast (IntGraphType ())
        typeof<uint32>, upcast (IntGraphType ())
        typeof<uint64>, upcast (IntGraphType ())
        typeof<Guid>, upcast (IdGraphType ())
        typeof<DateTime>, upcast (DateTimeGraphType ())
        typeof<DateTimeOffset>, upcast (DateTimeOffsetGraphType ())
        typeof<TimeSpan>, upcast (TimeSpanMillisecondsGraphType ())
    ]

type IDictionary<'key, 'value> with
    member this.TryGetValueOption (key: 'key) =
        match this.TryGetValue key with
        | true, value -> ValueSome value
        | _ -> ValueNone

let internal hasAnonRecordAttribute (``type``: Type) = ``type``.GetCustomAttribute<AnonymousRecordAttribute> () <> Unchecked.defaultof<_>

let internal getName anonymousName ``type`` =
    match ``type`` with
    | NormalRecord when hasAnonRecordAttribute ``type`` -> ValueSome ``type``.Name
    | AnonymousRecord -> anonymousName
    | _ -> ValueNone

let internal getAnonymousType anonymousName ``type`` =
    getName anonymousName ``type``
    |> ValueOption.map (
        fun name ->
            let object = Object<obj> (Name = name)
            FSharpType.GetRecordFields ``type``
            |> Array.map (
                fun property ->
                    Field (
                        Name = property.Name,
                        ResolvedType = createReference (ValueSome <| sprintf "%s%s" name property.Name) property.PropertyType,
                        Resolver = FuncFieldResolver<_> (fun ctx -> property.GetValue ctx.Source)
                    )
                )
            |> Array.iter (object.AddField >> ignore)
            object :> IGraphType
    )

let internal getDefaultTypeOrCreateAnonOrReference anonymousName ``type`` =
    defaultTypes.TryGetValueOption ``type``
    |> ValueOption.orElseWith (fun () -> getAnonymousType anonymousName ``type``)
    |> ValueOption.defaultValue (GraphQLTypeReference (typeName ``type``) :> IGraphType)

let internal createReference anonymousName ``type`` = unwrapType true (getDefaultTypeOrCreateAnonOrReference anonymousName) ``type``

let infer ``type`` =  createReference ValueNone ``type``
let inferField (field: #Field<'source>) ``type`` = createReference (ValueSome <| sprintf "%s%s" typeof<'source>.Name field.Name) ``type``
