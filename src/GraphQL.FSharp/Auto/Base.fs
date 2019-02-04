module internal GraphQL.FSharp.AutoBase

open System
open System.Collections.Generic
open System.Reflection
open System.Runtime.CompilerServices
open FSharp.Reflection
open GraphQL.Types

open GraphQL.FSharp
open GraphQL.FSharp.Resolvers
open GraphQL.FSharp.Utils

// TODO: Subscriptions

[<AutoOpen>]
module Attribute =
    let getMemberAttribute<'attribute when 'attribute :> Attribute> (``member``: MemberInfo) =
        ``member``.GetCustomAttribute<'attribute> ()
        |> Option.ofBox

[<AutoOpen>]
module internal Update =
    type Type with
        member this.TypeAttributes = this.GetCustomAttributes ()

    type MethodInfo with
        member this.MethodAttributes = this.GetCustomAttributes ()

    type PropertyInfo with
        member this.PropertyAttributes = this.GetCustomAttributes ()

    type ParameterInfo with
        member this.ParameterAttributes = this.GetCustomAttributes ()

    type UnionCaseInfo with
        member this.CaseAttributes =
            this.GetCustomAttributes ()
            |> Seq.filter (fun x -> x :? Attribute)
            |> Seq.map (fun x -> x :?> Attribute)

    let shouldIgnore (attributes: seq<Attribute>) =
        attributes
        |> Seq.exists (fun attribute -> attribute :? IgnoreAttribute)

    let tryGetAttribute<'attribute when 'attribute :> Attribute> (attributes: seq<Attribute>) =
        attributes
        |> Seq.tryFind (fun attribute -> attribute :? 'attribute)
        |> Option.map (fun attribute ->  attribute :?> 'attribute)

    let update<'attribute, 't when 'attribute :> AttributeWithValue<'t>> f (attributes: seq<Attribute>) =
        attributes
        |> tryGetAttribute<'attribute>
        |> Option.map (fun attribute -> attribute.Value)
        |> Option.iter f

        attributes

    let updateEnumValue attributes (x: EnumValueDefinition) =
        attributes
        |> update<NameAttribute, _> x.set_Name
        |> update<DescriptionAttribute, _> x.set_Description
        |> update<DeprecationReasonAttribute, _> x.set_DeprecationReason
        |> update<ValueAttribute, _> x.set_Value
        |> ignore

        x

    let updateArgument attributes (x: QueryArgument) =
        attributes
        |> update<NameAttribute, _> x.set_Name
        |> update<DescriptionAttribute, _> x.set_Description
        |> update<DefaultValueAttribute, _> x.set_DefaultValue
        |> ignore

        x

    let updateField attributes (x: EventStreamFieldType) =
        attributes
        |> update<NameAttribute, _> x.set_Name
        |> update<DescriptionAttribute, _> x.set_Description
        |> update<DeprecationReasonAttribute, _> x.set_DeprecationReason
        |> update<MetadataAttribute, _> x.set_Metadata
        |> update<TypeAttribute, _> x.set_Type
        |> update<DefaultValueAttribute, _> x.set_DefaultValue
        |> ignore

        x

    let (|Pair|) (pair: KeyValuePair<_, _>) = (pair.Key, pair.Value)

    let updateType attributes (x: #IGraphType)  =
        attributes
        |> update<NameAttribute, _> x.set_Name
        |> update<DescriptionAttribute, _> x.set_Description
        |> update<DeprecationReasonAttribute, _> x.set_DeprecationReason
        |> update<MetadataAttribute, _> (fun metadata ->
            for Pair (key, value) in metadata do
                x.Metadata.[key] <- value)
        |> ignore

        x

[<AutoOpen>]
module internal Field =
    let validProp (prop: PropertyInfo) =
        not prop.IsSpecialName
        && Option.isNone <| getMemberAttribute<CompilerGeneratedAttribute> prop

    let validProps (``type``: Type) =
        ``type``.GetProperties ()
        |> Array.filter validProp

    let properties<'object> =
        [|
            yield! validProps typeof<'object>
            for ``interface`` in typeof<'object>.GetInterfaces () do
                yield! validProps ``interface``
        |]
        |> Array.filter (fun prop -> not <| shouldIgnore prop.PropertyAttributes)

    let inline setInfo (prop: ^t) (x: ^event) =
        let name = (^t: (member Name: string) prop)
        (^event: (member set_Name: string -> unit) (x, name))

        x

    let makePropField infer (prop: PropertyInfo) =
        let field = EventStreamFieldType () |> setInfo prop

        field.Resolver <- (withSource >> resolve) prop.GetValue
        field.ResolvedType <- infer prop.PropertyType

        updateField prop.PropertyAttributes field

    let objectMethod (method: MemberInfo) = method.DeclaringType = typeof<obj>
    // FIXME: this is hacky but works for now. fix this later
    let systemMethod (method: MemberInfo) = method.Module.Name = "System.Private.CoreLib.dll"

    let genericMehod (method: MethodInfo) = method.IsGenericMethod

    let validMethod (method: MethodInfo) =
        not method.IsSpecialName
        && isNull <| method.GetCustomAttribute<CompilerGeneratedAttribute> ()
        && not <| genericMehod method
        && not <| objectMethod method
        && not <| systemMethod method

    let validMethods (``type``: Type)  =
        ``type``.GetMethods ()
        |> Array.filter validMethod
        |> Array.filter (fun method -> not method.IsStatic)

    let methods<'object> =
        [|
            yield! validMethods typeof<'object>
            for ``interface`` in typeof<'object>.GetInterfaces () do
                yield! validMethods ``interface``
        |]
        |> Array.filter (fun method -> not <| shouldIgnore method.MethodAttributes)

    let invalidGraphType =
        {
            new GraphType () with
                override __.Equals _ = false
        }

    let isInvalidGraphType (``type``: #IGraphType) = Object.ReferenceEquals (invalidGraphType, ``type``)

    let setArgumentType infer (parameter: ParameterInfo) (queryArgument: QueryArgument) =
        if isInvalidGraphType queryArgument.ResolvedType
        then queryArgument.ResolvedType <- infer parameter.ParameterType

        queryArgument

    type MethodParameter =
        | Argument of QueryArgument
        | Context

    let (|ContextParameter|ArgumentParameter|) (parameter: ParameterInfo) =
        if parameter.ParameterAttributes |> Seq.exists (fun attribute -> attribute :? ContextAttribute)
        then ContextParameter
        else ArgumentParameter

    let isResolveFieldContext (``type``: Type) =
        if ``type``.IsGenericType
            && ``type``.GetGenericTypeDefinition ()
                <> typedefof<ResolveFieldContext<_>> then true
        else ``type`` = typeof<ResolveFieldContext>

    let rec unwrapGraphType (``type``: IGraphType) =
        match ``type`` with
        | :? NonNullGraphType as ``type`` -> unwrapGraphType ``type``.ResolvedType
        | :? ListGraphType as ``type`` -> unwrapGraphType ``type``.ResolvedType
        | _ -> ``type``

    let rec isList (``type``: IGraphType) =
        match ``type`` with
        | :? NonNullGraphType as ``type`` -> isList ``type``.ResolvedType
        | :? ListGraphType -> true
        | _ -> false

    let allCaps (str: string) =
        str
        |> Seq.forall Char.IsUpper

    let normalizeName name =
        if allCaps name
        then sprintf "%c%s" name.[0] (name.[1..].ToLower())
        else name

    let checkArgumentName (argument: QueryArgument) =
        if isNull argument.Name || argument.Name = "" then
            let unwrapedType = unwrapGraphType argument.ResolvedType
            argument.Name <- normalizeName unwrapedType.Name
            if isList argument.ResolvedType
            then argument.Name <- sprintf "%sList" argument.Name
            argument
        else argument

    let makeArgument infer (parameter: ParameterInfo) =
        match parameter with
        | ContextParameter ->
            if not <| isResolveFieldContext parameter.ParameterType
            then invalidOp "A parameter that has the Context attribute must have the type of ResolveFieldContext<_>."

            Context
        | ArgumentParameter ->
            QueryArgument invalidGraphType
            |> setInfo parameter
            |> updateArgument parameter.ParameterAttributes
            |> setArgumentType infer parameter
            |> checkArgumentName
            |> Argument

    let makeMethodField infer (method: MethodInfo) =
        let field = EventStreamFieldType () |> setInfo method

        let arguments = method.GetParameters ()
        let queryArguemnts = arguments |> Array.map (makeArgument infer)

        let argumentPairs =
            (queryArguemnts, arguments)
            ||> Array.zip

        field.Arguments <-
            queryArguemnts
            |> Array.map (fun parameter ->
                match parameter with
                | Argument arg -> Some arg
                | Context -> None
            )
            |> Array.some
            |> QueryArguments

        field.ResolvedType <- infer method.ReturnType

        let field = updateField method.MethodAttributes field

        // TODO: is this needed?
        // if shouldResolve then
        field.Resolver <-
            resolveInfer (fun (ctx: ResolveFieldContext<_>) ->
                let resolvedArguments =
                    argumentPairs
                    |> Array.map (fun (queryArg, info) ->
                        match queryArg with
                        | Argument queryArg ->
                            ctx.GetArgument (
                                argumentType = info.ParameterType,
                                name = queryArg.Name,
                                defaultValue = queryArg.DefaultValue
                            )
                        | Context -> box ctx
                    )
                method.Invoke (ctx.Source, resolvedArguments))

        field

    let addProperties infer (object: #ComplexGraphType<'object>) =
        properties<'object>
        |> Array.map (makePropField infer)
        |> Array.iter (object.AddField >> ignore)

        object

    let addMethods infer (object: #ComplexGraphType<'object>) =
        methods<'object>
        |> Array.map (makeMethodField infer)
        |> Array.iter (object.AddField >> ignore)

        object
