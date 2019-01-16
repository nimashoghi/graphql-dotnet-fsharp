module GraphQL.FSharp.Auto

open System
open System.Collections.Generic
open System.Reflection
open System.Runtime.CompilerServices
open System.Threading.Tasks
open FSharp.Reflection
open GraphQL.Types
open GraphQL.Resolvers

open GraphQL.FSharp
open GraphQL.FSharp.Util

// TODO: Add runtime checking for proper types
// TODO: Add decorators for setting type descriptions

// TODO: clean this up

module Array =
    let some array =
        array
        |> Array.filter Option.isSome
        |> Array.map Option.get

let isValidEnum<'enum> =
    typeof<'enum>.IsEnum ||
    (FSharpType.IsUnion typeof<'enum> &&
     FSharpType.GetUnionCases typeof<'enum>
     |> Array.forall (fun case ->
         case.GetFields ()
         |> Array.isEmpty))

let isValidRecord<'object> = FSharpType.IsRecord typeof<'object>

let Enum<'enum> =
    assert isValidEnum<'enum>

    if typeof<'enum>.IsEnum then
        EnumerationGraphType<'enum> () :> EnumerationGraphType
    else
        let enum = EnumerationGraphType ()
        enum.Name <- typeof<'enum>.Name
        FSharpType.GetUnionCases typeof<'enum>
        |> Array.iter (fun case -> enum.AddValue(case.Name, null, FSharpValue.MakeUnion (case, [||])))

        Object.register (typeof<'enum> => enum)

        enum

let inline setFieldInfo (field: ^t) (attribute: FieldAttribute) =
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

let wrapNull (input: 't) =
    box input
    |> Option.ofObj
    |> Option.map unbox<'t>

let getFieldAttribute (method: MethodInfo) =
    method.GetCustomAttribute<FieldAttribute> ()
    |> wrapNull

let isTask (``type``: Type) =
    ``type``.IsGenericType
    && ``type``.GetGenericTypeDefinition () = typedefof<Task<_>>

// TODO: What about obs?
let (|SyncMethod|TaskMethod|) (method: MethodInfo) =
    if isTask method.ReturnType
    then TaskMethod method.ReturnType.GenericTypeArguments.[0]
    else SyncMethod method.ReturnType

let (||||) lhs rhs =
    if box lhs <> null
    then lhs
    else rhs

// TODO
// pipeline this
// create higher order inline method to decrease repetition
let setParamInfo (param: QueryArgument) (attribute: ArgumentAttribute) =
    if attribute.Name <> null then
        param.Name <- attribute.Name
    if attribute.Description <> null then
        param.Description <- attribute.Description
    if attribute.DefaultValue <> null then
        param.DefaultValue <- attribute.DefaultValue

// TODO: Use Strategy instead of ||||
let makeArgument (info: ParameterInfo) =
    let attribute =
        info.GetCustomAttribute<ArgumentAttribute> ()
        |||| ArgumentAttribute ()

    let argument =
        if attribute.Type <> null
        then QueryArgument attribute.Type
        else
            inferObject info.ParameterType
            |> QueryArgument

    argument.Name <- info.Name
    setParamInfo argument attribute

    argument, info

let makeFunctionField<'source> shouldResolve (method: MethodInfo) =
    let field = EventStreamFieldType ()
    field.Name <- method.Name

    let arguments =
        method.GetParameters ()
        |> Array.map makeArgument

    field.Arguments <-
        arguments
        |> Array.map fst
        |> QueryArguments

    let returnType =
        match method with
        | SyncMethod ``type``
        | TaskMethod ``type`` -> ``type``

    // TODO: Should sync/task be different?
    if shouldResolve then
        field.Resolver <- resolve (fun ctx ->
            let resolvedArguments =
                arguments
                // TODO: Edge case: paramInfo.ParameterType <> the actual type
                |> Array.map (fun (argument, paramInfo) ->
                    ctx.GetArgument (
                        paramInfo.ParameterType,
                        argument.Name,
                        argument.DefaultValue))
            method.Invoke (ctx.Source, resolvedArguments))

    field.ResolvedType <- inferObject returnType

    getFieldAttribute method
    |> Option.iter (setFieldInfo field)

    field

let funcFields (``type``: Type)  =
    ``type``.GetMethods (
        BindingFlags.DeclaredOnly
        ||| BindingFlags.Instance
        ||| BindingFlags.Public
        ||| BindingFlags.InvokeMethod
        ||| BindingFlags.FlattenHierarchy)
    |> Array.filter (fun method ->
        not method.IsSpecialName
        && method.GetCustomAttribute<CompilerGeneratedAttribute> () = null)
    // remove get_ and set_ methods
let private allFuncFields<'object> =
    if FSharpType.IsRecord typeof<'object> then [||] else
    [|
        yield! funcFields typeof<'object>
        for ``interface`` in typeof<'object>.GetInterfaces () do
            yield! funcFields ``interface``
    |]

let addFunctionFields resolve (source: #ComplexGraphType<'source>) =
    allFuncFields<'source>
    |> Array.map (makeFunctionField<'source> resolve)
    |> Array.iter (source.AddField >> ignore)

// TODO: Add func properties in records
// let allFuncProperties<'source> =
//     typeof<'source>.GetProperties ()
//     |> Array.filter (fun prop -> FSharpType.IsFunction prop.PropertyType)

// let addFunctionFieldsFromProperties (source: #ComplexGraphType<'source>) =
//     allFuncProperties<'source>
//     |> Array.map (fun prop ->
//         let field = EventStreamFieldType ()
//         field.Resolver)

let private allProps<'object> () = [|
    yield! typeof<'object>.GetProperties ()
    for ``interface`` in typeof<'object>.GetInterfaces () do
        yield! ``interface``.GetProperties ()
|]

let createField<'object> infer (prop: PropertyInfo, attribute: FieldAttribute) =
    let field = EventStreamFieldType ()
    field.Name <- prop.Name
    setFieldInfo field attribute
    field.Resolver <- resolve (fun ctx -> prop.GetValue ctx.Source)
    // TODO: Use attribute.Type
    field.ResolvedType <- infer prop.PropertyType
    field

// TODO: Should field prop be necessary?
let getPropInfo (prop: PropertyInfo) =
    let attribute =
        prop.GetCustomAttributes ()
        |> Seq.tryFind (fun attribute -> attribute :? FieldAttribute)
        |> Option.map (fun attribute -> attribute :?> FieldAttribute)
        |> Option.orElse (Some (FieldAttribute ()))
        |> Option.get
    prop, attribute

// TODO: Refactor into a pipeline
let private addFields infer (object: #ComplexGraphType<'object>) =
    allProps<'object> ()
    |> Array.map getPropInfo
    |> Array.map (createField<'object> infer)
    |> Array.iter (object.AddField >> ignore)

let abstractClasses<'object> =
    let rec run (``type``: Type) = [|
        let baseType = ``type``.BaseType
        if baseType <> null && baseType <> typeof<obj> && baseType.IsAbstract then
            yield baseType
            yield! run baseType
    |]
    run typeof<'object>

type IA = interface end
[<AbstractClass>]
type Base() = interface IA
[<AbstractClass>]
type BaseTwo() = inherit Base()
[<AbstractClass>]
type BaseThree() = inherit BaseTwo()
type Last() = inherit BaseThree()

assert (abstractClasses<Last> = [|typeof<BaseThree>; typeof<BaseTwo>; typeof<Base>|])

let private interfaces<'object> = [|
    yield! typeof<'object>.GetInterfaces ()
    yield! abstractClasses<'object>
|]

let private addInterfaces (object: ObjectGraphType<'object>) =
    interfaces<'object>
    |> Array.map (fun ``interface`` -> inferObject ``interface`` |> Option.ofObj)
    |> Array.some
    |> Array.filter (fun ``interface`` -> ``interface`` :? IInterfaceGraphType)
    |> Array.map (fun ``interface`` -> ``interface`` :?> IInterfaceGraphType)
    |> Array.iter (fun ``interface`` -> object.AddResolvedInterface ``interface``)

    object.IsTypeOf <- fun x -> x :? 'object

let inline setTypeInfo (object: ^t) (attribute: TypeAttribute) =
    if attribute.Name <> null then
        (^t : (member set_Name: string -> unit) object, attribute.Name)
    if attribute.Description <> null then
        (^t : (member set_Description: string -> unit) object, attribute.Description)
    if attribute.DeprecationReason <> null then
        (^t : (member set_DeprecationReason: string -> unit) object, attribute.DeprecationReason)
    if not <| List.isEmpty attribute.Metadata then
        (^t : (member set_Metadata: IDictionary<string, obj> -> unit) object, dict attribute.Metadata)

let Object<'object> =
    assert (not typeof<'object>.IsInterface)

    let object = ObjectGraphType<'object> ()
    object.Name <- typeof<'object>.Name
    addFields inferObject object
    addFunctionFields true object
    addInterfaces object
    TypeRegistry.Object.register (typeof<'object> => object)
    object

let InputObject<'object> =
    assert (not typeof<'object>.IsInterface)

    let object = InputObjectGraphType<'object> ()
    object.Name <- typeof<'object>.Name
    addFields inferInput object
    TypeRegistry.InputObject.register (typeof<'object> => object)
    object

let Interface<'object> =
    assert (typeof<'object>.IsInterface || typeof<'object>.IsAbstract)

    let ``interface`` = InterfaceGraphType<'object> ()
    ``interface``.Name <- typeof<'object>.Name
    addFields inferObject ``interface``
    addFunctionFields false ``interface``
    TypeRegistry.Object.register (typeof<'object> => ``interface``)
    ``interface``

let List (input: #IGraphType) = ListGraphType input
// TODO: Automatic explicit handling of nullable types using option
let NonNull (input: #IGraphType) = NonNullGraphType input
