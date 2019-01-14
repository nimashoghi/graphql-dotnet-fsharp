module GraphQL.FSharp.Auto

open System
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
    FSharpType.IsUnion typeof<'enum> &&
    FSharpType.GetUnionCases typeof<'enum>
    |> Array.forall (fun case ->
        case.GetFields ()
        |> Array.isEmpty)

let isValidUnion<'union> =
    FSharpType.IsUnion typeof<'union>

let isValidRecord<'object> = FSharpType.IsRecord typeof<'object>

let Enum<'enum> =
    assert isValidEnum<'enum>

    let enum = EnumerationGraphType ()
    enum.Name <- typeof<'enum>.Name
    FSharpType.GetUnionCases typeof<'enum>
    |> Array.iter (fun case -> enum.AddValue(case.Name, null, FSharpValue.MakeUnion (case, [||])))

    Object.register (typeof<'enum> => enum)

    enum

let Union<'union> =
    assert isValidUnion<'union>

    let union = UnionGraphType ()
    union.Name <- typeof<'union>.Name
    FSharpType.GetUnionCases typeof<'union>
    |> Array.map (fun case ->
        let object = ObjectGraphType ()
        object.Name <- case.Name

        case
            .GetFields()
            |> Array.indexed
            |> Array.map (fun (i, prop) ->
                let field = TypedFieldType<'union> ()
                field.Name <- prop.Name
                field.Resolver <- FuncFieldResolver<'union, _> (fun ctx ->
                    let _, fields = FSharpValue.GetUnionFields(ctx.Source, typeof<'union>)
                    fields.[i])
                field.ResolvedType <- inferObject prop.PropertyType
                field)
            |> Array.iter (fun field -> (object.AddField >> ignore) field)

        object.IsTypeOf <- (fun x ->
            if not (x :? 'union) then false else
            let case', _ = FSharpValue.GetUnionFields(x, typeof<'union>)
            case.Tag = case'.Tag)
        object)
    |> Array.iter union.AddPossibleType

    Object.register (typeof<'union> => union)

    union

let private allProps<'object> () = [|
    yield! typeof<'object>.GetProperties ()
    for ``interface`` in typeof<'object>.GetInterfaces () do
        yield! ``interface``.GetProperties ()
|]

// TODO: Refactor into a pipeline
let private addFields infer (object: #ComplexGraphType<'object>) =
    allProps<'object> ()
    |> Array.map (fun prop ->
        let field = FieldType ()
        field.Name <- prop.Name
        field.Resolver <- FuncFieldResolver<'object, _> (fun ctx -> prop.GetValue ctx.Source)
        field.ResolvedType <- infer prop.PropertyType
        field)
    |> Array.iter (object.AddField >> ignore)

let private addInterfaces (object: ObjectGraphType<'object>) =
    typeof<'object>.GetInterfaces ()
    |> Array.map (fun ``interface`` -> inferObject ``interface`` |> Option.ofObj)
    |> Array.some
    |> Array.filter (fun ``interface`` -> ``interface`` :? IInterfaceGraphType)
    |> Array.map (fun ``interface`` -> ``interface`` :?> IInterfaceGraphType)
    |> Array.iter (fun ``interface`` -> object.AddResolvedInterface ``interface``)

    object.IsTypeOf <- fun x -> x :? 'object

let Object<'object> =
    assert (not typeof<'object>.IsInterface)

    let object = ObjectGraphType<'object> ()
    object.Name <- typeof<'object>.Name
    addFields inferObject object
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
    assert typeof<'object>.IsInterface

    let ``interface`` = InterfaceGraphType<'object> ()
    ``interface``.Name <- typeof<'object>.Name
    addFields inferObject ``interface``
    TypeRegistry.Object.register (typeof<'object> => ``interface``)
    ``interface``

let List (input: #IGraphType) = ListGraphType input
// TODO: Automatic explicit handling of nullable types using option
let NonNull (input: #IGraphType) = NonNullGraphType input
