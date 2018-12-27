module GraphQL.FSharp.Builder.Field

open System
open Apollo
open FSharp.Linq.RuntimeHelpers
open FSharp.Quotations
open GraphQL.Types
open GraphQL.Resolvers
open Iris
open Iris.Option.Builders
open Iris.Option.Operators

open GraphQL.FSharp.Model
open GraphQL.FSharp.Builder.Helpers

type FieldWrapper<'source, 'graph, 'value> = {
    arguments: (SchemaInfo -> FieldType -> unit) list
    value: ValueType<'value>
    description: string option
    getter: Expr<'source -> 'value> option
    getterType: Type option
    name: string option
    resolver: (ResolveFieldContext<'source> -> 'value obs) option
}

let newField<'source, 'graph, 'value> : FieldWrapper<'source, 'graph, 'value> = {
    arguments = []
    value = Mandatory
    description = None
    getter = None
    getterType = None
    name = None
    resolver = None
}

let checkFieldGetter field =
    if Option.isSome field.getter && Option.isSome field.resolver then
        failwith "Field cannot have both a getter and a resolver"

let private get (field: FieldWrapper<_, _, _>) = field

type FieldBuilder<'source>() =
    member __.Yield _ = newField<'source, 'graph, 'value>

    /// Sets the name of this field
    [<CustomOperation "name">]
    member __.Name (field, name) = {get field with name = Some name}

    /// Sets the description of this field
    [<CustomOperation "description">]
    member __.Description (field, description) = {get field with description = Some description}

    /// Sets the description of this field
    /// Fails if we have set optional or mandatory
    [<CustomOperation "defaultValue">]
    member __.DefaultValue (field, defaultValue) = {get field with value = DefaultValue defaultValue}

    /// Make the field optional
    [<CustomOperation "optional">]
    member __.Optional field = {get field with value = Optional}

    /// Make the field optional
    [<CustomOperation "mandatory">]
    member __.Mandatory field = {get field with value = Mandatory}

    /// Sets the arguments of this fields
    [<CustomOperation "arguments">]
    member __.Arguments (field, arguments) = {get field with arguments = field.arguments @ arguments}

    /// Gets a specific field
    [<CustomOperation "get">]
    member __.Get (field, [<ReflectedDefinition>] getter) = {get field with getter = Some getter}

    /// Gets a specific field
    [<CustomOperation "getWithType">]
    member __.GetWithType (field, [<ReflectedDefinition>] getter, getterType) = {
        field with
            getter = Some getter
            getterType = Some getterType
    }

    /// Complex resolve function
    [<CustomOperation "resolve">]
    member __.Resolve (field, resolver) = {field with resolver = Some resolver}

    /// Converts the elevated wrapper type into a function that can be called on initialization
    member __.Run (field: FieldWrapper<'source, 'graph, 'value>) =
        fun (schema: SchemaInfo) (graph: ComplexGraphType<'source>) -> maybeOrThrow {
            let fieldType = FieldType()

            let! type' = field.getterType ||| typeof<'graph>
            fieldType.Type <- type'

            let! name = field.name
            fieldType.Name <- name

            maybeUnit {
                let! description = field.description ||| ""
                fieldType.Description <- description
            }

            // throw if both getter and resovler are set
            checkFieldGetter field

            maybeUnit {
                let! expression = field.getter
                let expression = LeafExpressionConverter.QuotationToLambdaExpression <@ Func<_, _> %expression @>
                fieldType.Resolver <- ExpressionFieldResolver<'source, 'value> expression
                setType schema typeof<'value> (isNullable field.value) fieldType
            }

            maybeUnit {
                let! resolver = field.resolver
                let resolver = Func.from (resolver >> Observable.toTask)
                fieldType.Resolver <- AsyncFieldResolver<'source, 'value> resolver
                setType schema typeof<'value> (isNullable field.value) fieldType
            }

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
