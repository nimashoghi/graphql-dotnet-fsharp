module GraphQL.FSharp.AutoBase

open System
open System.Collections.Generic
open System.Reflection
open System.Runtime.CompilerServices
open FSharp.Reflection
open GraphQL.Types
open GraphQL.Resolvers

open GraphQL.FSharp.Inference
open GraphQL.FSharp.Utils

[<AttributeUsage(
    AttributeTargets.Property
    ||| AttributeTargets.Field
    ||| AttributeTargets.Method,
    AllowMultiple = false,
    Inherited = true)>]
type FieldAttribute() =
    inherit Attribute()

    member val Name: string = null with get, set
    member val Description: string = null with get, set
    member val DeprecationReason: string = null with get, set
    member val Metadata: (string * obj) list = [] with get, set
    member val Type: Type = null with get, set
    member val DefaultValue: obj = null with get, set

[<AttributeUsage(
    AttributeTargets.Interface
    ||| AttributeTargets.Class
    ||| AttributeTargets.Struct,
    AllowMultiple = false,
    Inherited = true)>]
type TypeAttribute() =
    inherit Attribute()

    member val Name: string = null with get, set
    member val Description: string = null with get, set
    member val DeprecationReason: string = null with get, set
    member val Metadata: (string * obj) list = [] with get, set

[<AttributeUsage(
    AttributeTargets.Parameter,
    AllowMultiple = false,
    Inherited = true)>]
type ArgumentAttribute() =
    inherit Attribute()

    member val Name: string = null with get, set
    member val Description: string = null with get, set
    member val Type: Type = null with get, set
    member val DefaultValue: obj = null with get, set

[<AutoOpen>]
module Resolvers =
    let resolve f =
        {
            new IFieldResolver with
                member __.Resolve ctx = f ctx |> box
        }

    let resolveSource f =
        {
            new IFieldResolver with
                member __.Resolve ctx = f ctx.Source |> box
        }

[<AutoOpen>]
module Attribute =
    let getUnionCaseAttribute<'attribute when 'attribute :> Attribute> (case: UnionCaseInfo) =
        case.GetCustomAttributes ()
        |> Array.tryFind (fun attribute -> attribute :? 'attribute)
        |> Option.map (fun attribute -> attribute :?> 'attribute)

    let getMemberAttribute<'attribute when 'attribute :> Attribute> (``member``: MemberInfo) =
        ``member``.GetCustomAttribute<'attribute> ()
        |> Option.ofBox

    let getTypeAttribute<'attribute when 'attribute :> Attribute> (``member``: Type) =
        ``member``.GetCustomAttribute<'attribute> ()
        |> Option.ofBox

    let getArgAttribute<'attribute when 'attribute :> Attribute> (parameter: ParameterInfo) =
        parameter.GetCustomAttribute<'attribute> ()
        |> Option.ofBox

[<AutoOpen>]
module Update =
    let inline withName (attribute: ^attribute) (x: ^t) =
        match Option.ofObj (^attribute: (member Name: string) attribute) with
        | Some name -> (^t: (member set_Name: string -> unit) x, name)
        | _ -> ()
        x

    let inline withDescription (attribute: ^attribute) (x: ^t) =
        match Option.ofObj (^attribute: (member Description: string) attribute) with
        | Some description -> (^t: (member set_Description: string -> unit) x, description)
        | _ -> ()
        x

    let inline withDefaultValue (attribute: ^attribute) (x: ^t) =
        match Option.ofObj (^attribute: (member DefaultValue: obj) attribute) with
        | Some defaultValue -> (^t: (member set_DefaultValue: obj -> unit) x, defaultValue)
        | _ -> ()
        x

    let inline withDeprecationReason (attribute: ^attribute) (x: ^t) =
        match Option.ofObj (^attribute: (member DeprecationReason: string) attribute) with
        | Some deprecationReason -> (^t: (member set_DeprecationReason: string -> unit) x, deprecationReason)
        | _ -> ()
        x

    let inline withType (attribute: ^attribute) (x: ^t) =
        match Option.ofObj (^attribute: (member Type: Type) attribute) with
        | Some ``type`` -> (^t: (member set_Type: Type -> unit) x, ``type``)
        | _ -> ()
        x

    let inline withMetadata (attribute: ^attribute) (x: ^t) =
        let metadata = (^t: (member Metadata: IDictionary<string, obj>) x)
        for key, value in (^attribute: (member Metadata: (string * obj) list) attribute) do
            metadata.[key] <- value
        x

    let inline updateArgument (x: QueryArgument) (attribute: ArgumentAttribute) =
        x
        |> withName attribute
        |> withDescription attribute
        |> withDefaultValue attribute

    let inline updateField (x: FieldType) (attribute: FieldAttribute) =
        x
        |> withName attribute
        |> withDescription attribute
        |> withDeprecationReason attribute
        |> withDefaultValue attribute
        |> withType attribute
        |> withMetadata attribute

    let inline updateType x (attribute: TypeAttribute) =
        x
        |> withName attribute
        |> withDescription attribute
        |> withDeprecationReason attribute
        |> withMetadata attribute

[<AutoOpen>]
module Field =
    let validProp (prop: PropertyInfo) =
        not prop.IsSpecialName
        && Option.isNone <| getMemberAttribute<CompilerGeneratedAttribute> prop

    let validProps (``type``: Type) =
        ``type``.GetProperties ()
        |> Array.filter validProp

    let properties<'object> = [|
        yield! validProps typeof<'object>
        for ``interface`` in typeof<'object>.GetInterfaces () do
            yield! validProps ``interface``
    |]

    let inline setInfo (prop: ^t) (field: ^event) =
        let name = (^t: (member Name: string) prop)
        (^event: (member set_Name: string -> unit) field, name)

    let makePropField infer (prop: PropertyInfo) =
        let field = EventStreamFieldType ()

        setInfo prop field

        field.Resolver <- resolveSource prop.GetValue
        field.ResolvedType <- infer prop.PropertyType

        // if `FieldAttribute` is set, then update our field with the info
        prop.GetCustomAttribute<FieldAttribute> ()
        |> Option.ofBox
        |> Option.iter (updateField field >> ignore)

        field

    let objectMethod (method: MemberInfo) = method.DeclaringType = typeof<obj>
    // FIXME: this is hacky but works for now. fix this later
    let systemMethod (method: MemberInfo) = method.Module.Name = "System.Private.CoreLib.dll"

    let validMethod (method: MethodInfo) =
        not method.IsSpecialName
        && method.GetCustomAttribute<CompilerGeneratedAttribute> () = null
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

    let makeArgument (parameter: ParameterInfo) =
        let attribute =
            getArgAttribute<ArgumentAttribute> parameter
            |> Option.``or`` (ArgumentAttribute ())

        let queryArgument =
            match Option.ofObj attribute.Type with
            | Some ``type`` -> QueryArgument ``type``
            | None ->
                inferObject parameter.ParameterType
                |> QueryArgument

        setInfo parameter queryArgument

        updateArgument queryArgument attribute |> ignore

        queryArgument

    let makeMethodField infer (method: MethodInfo) =
        let field = EventStreamFieldType ()

        setInfo method field

        let arguments = method.GetParameters ()
        let queryArguemnts = Array.map makeArgument arguments

        let argumentPairs =
            (queryArguemnts, arguments)
            ||> Array.zip

        field.Arguments <- QueryArguments queryArguemnts
        field.ResolvedType <- infer method.ReturnType

        getMemberAttribute<FieldAttribute> method
        |> Option.iter (updateField field >> ignore)

        // TODO: is this needed?
        // if shouldResolve then
        field.Resolver <-
            resolve (fun ctx ->
                let resolvedArguments =
                    argumentPairs
                    |> Array.map (fun (queryArg, info) ->
                        ctx.GetArgument (
                            argumentType = info.ParameterType,
                            name = queryArg.Name,
                            defaultValue = queryArg.DefaultValue
                        ))
                method.Invoke (ctx.Source, resolvedArguments))

        field

    let addProperties infer (object: ComplexGraphType<'object>) =
        properties<'object>
        |> Array.map (makePropField infer)
        |> Array.iter (object.AddField >> ignore)

    let addMethods<'object> infer (object: IComplexGraphType) =
        methods<'object>
        |> Array.map (makeMethodField infer)
        |> Array.iter (object.AddField >> ignore)
