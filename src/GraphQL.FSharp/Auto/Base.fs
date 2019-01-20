module internal GraphQL.FSharp.AutoBase

open System
open System.Collections.Generic
open System.Reflection
open System.Runtime.CompilerServices
open FSharp.Reflection
open GraphQL.Types
open GraphQL.Resolvers

open GraphQL.FSharp
open GraphQL.FSharp.Inference
open GraphQL.FSharp.Utils

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

    let tryGetAttribute<'attribute when 'attribute :> Attribute> (attributes: Attribute seq) =
        attributes
        |> Seq.tryFind (fun attribute -> attribute :? 'attribute)
        |> Option.map (fun attribute ->  attribute :?> 'attribute)

    let update<'attribute, 't when 'attribute :> AttributeWithValue<'t>> f (attributes: Attribute seq) =
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

    let updateField attributes (x: FieldType) =
        attributes
        |> update<NameAttribute, _> x.set_Name
        |> update<DescriptionAttribute, _> x.set_Description
        |> update<DeprecationReasonAttribute, _> x.set_DeprecationReason
        |> update<MetadataAttribute, _> x.set_Metadata
        |> update<TypeAttribute, _> x.set_Type
        |> update<DefaultValueAttribute, _> x.set_DefaultValue
        |> ignore

        x

    let (|Pair|) (pair: KeyValuePair<_, _>) = pair.Key, pair.Value

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

    let properties<'object> = [|
        yield! validProps typeof<'object>
        for ``interface`` in typeof<'object>.GetInterfaces () do
            yield! validProps ``interface``
    |]

    let inline setInfo (prop: ^t) (x: ^event) =
        let name = (^t: (member Name: string) prop)
        (^event: (member set_Name: string -> unit) x, name)

        x

    let makePropField infer (prop: PropertyInfo) =
        let field = EventStreamFieldType () |> setInfo prop

        field.Resolver <- resolveSource prop.GetValue
        field.ResolvedType <- infer prop.PropertyType

        updateField (prop.GetCustomAttributes ()) field

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

    let invalidGraphType =
        {
            new GraphType () with
                override __.Equals _ = false
        }

    let setArgumentType (parameter: ParameterInfo) (queryArgument: QueryArgument) =
        if Object.ReferenceEquals (invalidGraphType, queryArgument.ResolvedType)
        then queryArgument.ResolvedType <- inferObject parameter.ParameterType

        queryArgument

    let makeArgument (parameter: ParameterInfo) =
        QueryArgument invalidGraphType
        |> setInfo parameter
        |> updateArgument (parameter.GetCustomAttributes ())
        |> setArgumentType parameter

    let makeMethodField infer (method: MethodInfo) =
        let field = EventStreamFieldType () |> setInfo method

        let arguments = method.GetParameters ()
        let queryArguemnts = Array.map makeArgument arguments

        let argumentPairs =
            (queryArguemnts, arguments)
            ||> Array.zip

        field.Arguments <- QueryArguments queryArguemnts
        field.ResolvedType <- infer method.ReturnType

        let field = updateField (method.GetCustomAttributes ()) field

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
