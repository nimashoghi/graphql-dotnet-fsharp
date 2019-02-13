[<AutoOpen>]
module internal GraphQL.FSharp.AutoBase.Field

open System
open System.Collections.Generic
open System.Reflection
open System.Runtime.CompilerServices
open FSharp.Reflection
open GraphQL
open GraphQL.Types

open GraphQL.FSharp
open GraphQL.FSharp.NameTransformers
open GraphQL.FSharp.Resolvers
open GraphQL.FSharp.Utils
open GraphQL.FSharp.Utils.Attributes

let getMemberAttribute<'attribute when 'attribute :> Attribute> (``member``: MemberInfo) =
    ``member``.GetCustomAttribute<'attribute> ()
    |> Option.ofBox

let (|OptIn|OptOut|) (``type``: Type) =
    let attributes = ``type``.TypeAttributes
    if hasAttribute<AutoAttribute> attributes then OptOut
    else OptIn

let shouldIgnore ``type`` (attributes: seq<Attribute>) =
    match ``type`` with
    | OptOut ->
        attributes
        |> Seq.exists (fun attribute -> attribute :? IgnoreAttribute)
    | OptIn ->
        attributes
        |> Seq.exists (fun attribute -> attribute :? FieldAttribute)
        |> not

let validProp (prop: PropertyInfo) =
    not prop.IsSpecialName
    && Option.isNone <| getMemberAttribute<CompilerGeneratedAttribute> prop

let validProps (``type``: Type) =
    ``type``.GetProperties ()
    |> Array.filter validProp

let properties<'object> =
    [|
        if FSharpType.IsRecord typeof<'object>
        then yield! FSharpType.GetRecordFields typeof<'object>
        else yield! validProps typeof<'object>

        for ``interface`` in typeof<'object>.GetInterfaces () do
            yield! validProps ``interface``
    |]
    |> Array.filter (fun prop -> not <| shouldIgnore typeof<'object> prop.PropertyAttributes)

let makePropField infer (prop: PropertyInfo) =
    let field = EventStreamFieldType (Name = transformPropertyName prop prop.Name)

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
    |> Array.filter (fun method -> not <| shouldIgnore typeof<'object> method.MethodAttributes)

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

let isContextParam (parameter: ParameterInfo) =
    if parameter.ParameterType = typeof<ResolveFieldContext>
        || (parameter.ParameterType.IsGenericType
            && parameter.ParameterType.GetGenericTypeDefinition ()
                = typedefof<ResolveFieldContext<_>>)
    then true
    else false

let (|ContextParameter|ArgumentParameter|) parameter =
    if isContextParam parameter
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

let getTypeName (``type``: #IGraphType) =
    match unwrapGraphType ``type`` with
    | :? GraphQLTypeReference as ``type`` -> ``type``.TypeName
    | ``type`` -> ``type``.Name

let checkArgumentName (argument: QueryArgument) =
    if isNull argument.Name || argument.Name = "" then
        argument.Name <- normalizeName (getTypeName argument.ResolvedType)
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
        QueryArgument (invalidGraphType, Name = parameter.Name)
        |> updateArgument parameter.ParameterAttributes
        |> setArgumentType infer parameter
        |> checkArgumentName
        |> Argument

// TODO: Make into own module and test
let emptyList ``type`` =
    typedefof<_ list>
        .MakeGenericType([|``type``|])
        .GetProperty("Empty")
        .GetValue(null)

let consList ``type`` head tail =
    typedefof<_ list>
        .MakeGenericType([|``type``|])
        .GetMethod("Cons")
        .Invoke(null, [|head; tail|])

let getListValue (x: obj) (``type``: Type) =
    let innerType = ``type``.GenericTypeArguments.[0]
    let mutable list = emptyList innerType
    for element in (x :?> System.Collections.IEnumerable) do
        list <- consList innerType element list
    list

// TODO: Test
let getArgument (``type``: Type) (ctx: ResolveFieldContext<_>) name =
    // if not <| ctx.HasArgument name then None else
    // let argument = ctx.Arguments.[name]
    // if ``type``.IsGenericType && ``type``.GetGenericTypeDefinition () = typedefof<_ list>
    // then Some (getListValue argument ``type``)
    // else
    //     let value =
    //         match argument with
    //         | :? Dictionary<string, obj> as inputObject ->
    //             ``type``.Namespace
    //             |> Option.ofObj
    //             |> Option.bind (fun ``namespace`` ->
    //                 if ``namespace``.StartsWith "System"
    //                 then Some argument
    //                 else None
    //             )
    //             |> Option.defaultWith inputObject.ToObject
    //         | _ -> argument.GetPropertyValue ``type``
    //     value
    //     |> Option.ofObj
    //     |> Option.orElseWith (fun () ->
    //         ctx
    //             .GetArgument (
    //                 argumentType = ``type``,
    //                 name = name,
    //                 defaultValue = null
    //             )
    //         |> Option.ofObj
    //     )
    ctx.GetArgument (
        argumentType = ``type``,
        name = name,
        defaultValue = null
    )
    |> Some

let makeMethodField infer (method: MethodInfo) =
    let field = EventStreamFieldType (Name = transformMethodName method method.Name)

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

    field.Resolver <-
        resolveInfer (fun (ctx: ResolveFieldContext<_>) ->
            let resolvedArguments =
                argumentPairs
                |> Array.map (fun (queryArg, info) ->
                    match queryArg with
                    | Argument queryArg ->
                        getArgument info.ParameterType ctx queryArg.Name
                        |> Option.defaultValue queryArg.DefaultValue
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
