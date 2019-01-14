module GraphQL.FSharp.Auto

open FSharp.Reflection
open GraphQL.Types
open GraphQL.Resolvers

open GraphQL.FSharp
open GraphQL.FSharp.Util

// TODO: clean this up

let isValidEnum<'enum> =
    FSharpType.IsUnion typeof<'enum> &&
    FSharpType.GetUnionCases typeof<'enum>
    |> Array.forall (fun case ->
        case.GetFields ()
        |> Array.isEmpty)

let isValidUnion<'union> =
    FSharpType.IsUnion typeof<'union>

let isValidRecord<'record> = FSharpType.IsRecord typeof<'record>

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

let Object<'record> =
    assert isValidRecord<'record>

    let object = ObjectGraphType<'record> ()
    object.Name <- typeof<'record>.Name
    FSharpType.GetRecordFields typeof<'record>
    |> Array.map (fun prop ->
        let field = FieldType ()
        field.Name <- prop.Name
        field.Resolver <- FuncFieldResolver<'record, _> (fun ctx -> prop.GetValue ctx.Source)
        field.ResolvedType <- inferObject prop.PropertyType
        field)
    |> Array.iter (object.AddField >> ignore)

    Object.register (typeof<'record> => object)

    object

let InputObject<'record> =
    assert isValidRecord<'record>

    let object = InputObjectGraphType<'record> ()
    object.Name <- typeof<'record>.Name
    FSharpType.GetRecordFields typeof<'record>
    |> Array.map (fun prop ->
        let field = FieldType ()
        field.Name <- prop.Name
        field.Resolver <- FuncFieldResolver<'record, _> (fun ctx -> prop.GetValue ctx.Source)
        field.ResolvedType <- inferObject prop.PropertyType
        field)
    |> Array.iter (object.AddField >> ignore)

    InputObject.register (typeof<'record> => object)

    object
