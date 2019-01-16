namespace GraphQL.FSharp

open System
open System.Collections.Generic
open System.Runtime.CompilerServices
open System.Reflection
open FSharp.Reflection
open GraphQL.Types

open GraphQL.FSharp.Inference
open GraphQL.FSharp.Types
open GraphQL.FSharp.Registry

// TODO: For now we only support tupled functions
// fix this


// TODO: Relocate to its own file
module Option =
    let ofBox (x: 't) =
        x
        |> box
        |> Option.ofObj
        |> Option.map unbox<'t>

[<AutoOpen>]
module AutoUtils2 =

    let getFunctionElements ``type`` =
        let rec run ``type`` args =
            assert FSharpType.IsFunction ``type``

            let arg, retn = FSharpType.GetFunctionElements ``type``
            if FSharpType.IsFunction retn then
                arg :: args
                |> run retn
            else
                arg :: args, retn

        let args, ret = run ``type`` []
        List.rev args, ret

    let (|Data|Function|) (prop: PropertyInfo) =
        if FSharpType.IsFunction prop.PropertyType
        then Data
        else Function

    let invalidProp (prop: PropertyInfo) =
        prop.IsSpecialName
        || prop.GetCustomAttribute<CompilerGeneratedAttribute> () <> null

    let invalidMethod (method: MethodInfo) =
        method.IsSpecialName
        || method.GetCustomAttribute<CompilerGeneratedAttribute> () <> null

    let validProps (``type``: Type) =
        ``type``.GetProperties ()
        |> Array.filter (not << invalidProp)

    let properties<'object> = [|
        yield! validProps typeof<'object>
        for ``interface`` in typeof<'object>.GetInterfaces () do
            yield! validProps ``interface``
    |]

    let inline updateFieldFromAttribute (field: ^t) (attribute: FieldAttribute) =
        if attribute.Name <> null then
            (^t : (member set_Name: string -> unit) field, attribute.Name)
        if attribute.Description <> null then
            (^t : (member set_Description: string -> unit) field, attribute.Description)
        if attribute.DefaultValue <> null then
            (^t : (member set_DefaultValue: obj -> unit) field, attribute.DefaultValue)
        if attribute.Type <> null then
            (^t : (member set_Type: Type -> unit) field, attribute.Type)
        if not <| List.isEmpty attribute.Metadata then
            (^t : (member set_Metadata: IDictionary<string, obj> -> unit) field, dict attribute.Metadata)

    let setInfoFromProp (prop: PropertyInfo) (field: EventStreamFieldType) =
        field.Name <- prop.Name

    let makePropField infer (prop: PropertyInfo) =
        let field = EventStreamFieldType ()

        setInfoFromProp prop field

        match prop with
        | Data ->
            field.Resolver <- resolve (fun ctx -> prop.GetValue ctx.Source)
            field.ResolvedType <- infer prop.PropertyType
        // these are handled in the getmethods calls
        | _ -> ()

        // if `FieldAttribute` is set, then update our field with the info
        prop.GetCustomAttribute<FieldAttribute> ()
        |> Option.ofBox
        |> Option.iter (updateFieldFromAttribute field)

        field

    type ReflectedArgument = {
        Attribute: ArgumentAttribute option
        Name: string
        Type: Type
    }

    type ReflectedMethod = {
        Attribute: FieldAttribute option
        Arguments: ReflectedArgument []
        Invoke: obj -> obj [] -> obj
        ReturnType: Type
    }

    let invokeObj fn args =
        fn
            .GetType()
            .GetMethod("Invoke")
            .Invoke(fn, [|args|])

    let (|ReflectedArgument|) (argument: ParameterInfo) =
        {
            Name = argument.Name
            Type = argument.ParameterType
            Attribute =
                argument.GetCustomAttribute<ArgumentAttribute> ()
                |> Option.ofBox
        }

    let (|ReflectedMethod|) (method: MethodInfo) =
        {
            Arguments = [|
                for ReflectedArgument argument in method.GetParameters () do
                    yield argument
            |]
            Attribute =
                method.GetCustomAttribute<FieldAttribute> ()
                |> Option.ofBox
            Invoke = (fun this args -> method.Invoke (this, args))
            ReturnType = method.ReturnType
        }

    let (|ReflectedPropertyArgument|) (``type``: Type) =
        {
            // TODO: set name
            Name = ""
            Type = ``type``
            Attribute = None
        }

    let (|ReflectedPropertyFunction|) (property: PropertyInfo) =
        let arguments, ret = getFunctionElements property.PropertyType
        {
            Arguments = [|
                for ReflectedPropertyArgument argument in arguments do
                    yield argument
            |]
            Attribute =
                property.GetCustomAttribute<FieldAttribute> ()
                |> Option.ofBox
            Invoke = (fun this args ->
                let tupleType =
                    args
                    |> Array.map (fun x -> x.GetType ())
                    |> FSharpType.MakeTupleType
                let args = FSharpValue.MakeTuple (args, tupleType)
                let fn = property.GetValue this
                invokeObj fn args)
            ReturnType = ret
        }

    let methods (``type``: Type)  =
        ``type``.GetMethods (
            BindingFlags.DeclaredOnly
            ||| BindingFlags.Instance
            ||| BindingFlags.Public
            ||| BindingFlags.InvokeMethod
            ||| BindingFlags.FlattenHierarchy)
        |> Array.filter (not << invalidMethod)
        |> Array.map (|ReflectedMethod|)

    let private allMethods<'object> =
        [|
            if not <| FSharpType.IsRecord typeof<'object> then
                yield! methods typeof<'object>
                for ``interface`` in typeof<'object>.GetInterfaces () do
                    yield! methods ``interface``
            yield!
                properties<'object>
                |> Array.filter (fun prop -> FSharpType.IsFunction prop.PropertyType)
                |> Array.map (|ReflectedPropertyFunction|)
        |]

    let makeMethodField infer (method: ReflectedMethod) =
        let field = EventStreamFieldType()
        field

    type ReflectedField =
    | Property of Property: PropertyInfo
    | Method of Method: ReflectedMethod

    let makeField infer = function
    | Property prop -> makePropField infer prop
    | Method method -> makeMethodField infer method

module UnionCase =
    let getCustomAttribute<'attribute when 'attribute :> Attribute> (case: UnionCaseInfo) =
        case.GetCustomAttributes ()
        |> Array.tryFind (fun x -> x :? 'attribute)
        |> Option.map (fun x -> x :?> 'attribute)

module private Union =
    let isValidUnion<'union> =
        FSharpType.IsUnion typeof<'union>

    let makeUnionCase<'union> (case: UnionCaseInfo) =
        let object = ObjectGraphType ()
        case
        |> UnionCase.getCustomAttribute<TypeAttribute>
        |> Option.iter (fun attribute ->)

        for field in case.GetFields () do
            Property field
            |> makeField inferObject
            |> object.AddField
            |> ignore

        object.IsTypeOf <- (fun x ->
            if not (x :? 'union) then false else
            let case', _ = FSharpValue.GetUnionFields(x, typeof<'union>)
            case.Tag = case'.Tag)
        object

    let addUnionFields<'union> (union: UnionGraphType) =
        FSharpType.GetUnionCases typeof<'union>
        |> Array.map makeUnionCase<'union>
        |> Array.iter union.AddPossibleType

    let inline setTypeInfo (object: ^t) (attribute: TypeAttribute) =
        if attribute.Name <> null then
            (^t : (member set_Name: string -> unit) object, attribute.Name)
        if attribute.Description <> null then
            (^t : (member set_Description: string -> unit) object, attribute.Description)
        if attribute.DeprecationReason <> null then
            (^t : (member set_DeprecationReason: string -> unit) object, attribute.DeprecationReason)
        if not <| List.isEmpty attribute.Metadata then
            (^t : (member set_Metadata: IDictionary<string, obj> -> unit) object, dict attribute.Metadata)

    let Union<'union> =
        assert isValidUnion<'union>

        let union = UnionGraphType ()
        union.Name <- typeof<'union>.Name

        typeof<'union>.GetCustomAttribute<TypeAttribute> ()
        |> Option.ofBox
        |> Option.iter (setTypeInfo union)

        addUnionFields<'union> union
        Object.register (typeof<'union> => union)

        union
