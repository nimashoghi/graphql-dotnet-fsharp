module GraphQL.FSharp.Builder.Field

open System
open FSharp.Control.Reactive
open FSharp.Linq.RuntimeHelpers
open FSharp.Quotations
open GraphQL.Types
open GraphQL.Resolvers
open Iris
open Iris.Option.Builders
open Iris.Option.Operators
open Rx

open GraphQL.FSharp.Model
open GraphQL.FSharp.Builder.Helpers

type FieldWrapper<'source, 'graph, 'value> = {
    arguments: (SchemaInfo -> FieldType -> unit) list
    value: ValueType<'value>
    description: string option
    getter: Expr<'source -> 'value> option
    getterType: Type option
    getterGraphType: (SchemaInfo -> IGraphType) option
    name: string option
    resolver: (ResolveFieldContext<'source> -> 'value obs) option
}

let newField<'source, 'graph, 'value> : FieldWrapper<'source, 'graph, 'value> = {
    arguments = []
    value = Mandatory
    description = None
    getter = None
    getterType = None
    getterGraphType = None
    name = None
    resolver = None
}

let checkFieldGetter field =
    match field.getter, field.resolver with
    | Some _, Some _ -> failwith "Field cannot have both a getter and a resolver"
    | _, _ -> ()

// TODO: Sync up 'source with stuff below
// TODO: Optional set up
type FieldBuilder<'source, 'value>() =
    member __.Yield _ = newField<'source, 'graph, 'value>

    /// Sets the name of this field
    [<CustomOperation "name">]
    member __.Name (field, name) = {field with name = Some name}

    /// Sets the description of this field
    [<CustomOperation "description">]
    member __.Description (field, description) = {field with description = Some description}

    /// Sets the type of this field
    [<CustomOperation "type">]
    member __.Type (field, ``type``) = {field with getterType = Some ``type``}

    /// Sets the description of this field
    /// Fails if we have set optional or mandatory
    [<CustomOperation "defaultValue">]
    member __.DefaultValue (field, defaultValue) = {field with value = DefaultValue defaultValue}

    /// Make the field optional
    [<CustomOperation "optional">]
    member __.Optional field = {field with value = Optional}

    /// Make the field optional
    [<CustomOperation "mandatory">]
    member __.Mandatory field = {field with value = Mandatory}

    /// Sets the arguments of this fields
    [<CustomOperation "arguments">]
    member __.Arguments (field, arguments) = {field with arguments = field.arguments @ arguments}

    /// Gets a specific field
    [<CustomOperation "get">]
    member __.Get (field, [<ReflectedDefinition>] getter) = {field with getter = Some getter}

    /// Sets the type of a field
    [<CustomOperation "setType">]
    member __.SetType (field, ``type``) = {field with getterType = Some ``type``}

    /// Gets a specific field
    member __.SetType (field, ``type``) = {field with getterGraphType = Some ``type``}

    /// Complex resolve function
    [<CustomOperation "resolve">]
    member __.Resolve (field, resolver) = {field with resolver = Some resolver}

    /// Converts the elevated wrapper type into a function that can be called on initialization
    member __.Run (field: FieldWrapper<'source, 'graph, 'value>) =
        fun (schema: SchemaInfo) (graph: ComplexGraphType<'source>) -> maybeOrThrow {
            let fieldType = FieldType()

            let hasCustomType =
                Option.orElse
                    (maybe {
                        let! t = field.getterType
                        fieldType.Type <- t
                        return ()
                    })
                    (maybe {
                        let! t = field.getterGraphType
                        fieldType.ResolvedType <- t schema
                        return ()
                    })
                |> Option.isSome

            if not hasCustomType then fieldType.Type <- typeof<'graph>

            let! name = field.name
            fieldType.Name <- name

            maybeUnit {
                let! description = field.description ||| ""
                fieldType.Description <- description
            }

            maybeUnit {
                let! expression = field.getter
                let expression = LeafExpressionConverter.QuotationToLambdaExpression <@ Func<_, _> %expression @>
                fieldType.Resolver <- ExpressionFieldResolver<'source, 'value> expression
                // TODO: Check this
                if not hasCustomType then
                    setType schema typeof<'value> (isNullable field.value) fieldType
            }

            maybeUnit {
                let! resolver = field.resolver
                let resolver = Func.from (resolver >> Observable.toTask)
                fieldType.Resolver <- AsyncFieldResolver<'source, 'value> resolver
                if not hasCustomType then
                    setType schema typeof<'value> (isNullable field.value) fieldType
            }

            // throw if both getter and resovler are set
            checkFieldGetter field

            fieldType.Arguments <- QueryArguments()
            List.iter (fun argument -> argument schema fieldType) field.arguments

            // check if optional
            do
                match field.value with
                | DefaultValue value ->
                    fieldType.DefaultValue <- box value
                | Optional ->
                    // TODO: look at this
                    fieldType.DefaultValue <- null
                | _ -> ()

            ignore <| graph.AddField fieldType
        }
