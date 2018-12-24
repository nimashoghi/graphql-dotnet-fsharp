module GraphQL.FSharp.Builder

type ValueType<'t> =
| Optional
| Mandatory
| DefaultValue of 't

[<AutoOpen>]
module SchemaBuilder =
    open System
    open FSharp.Injection
    open GraphQL
    open GraphQL.Types
    open Iris.Option.Builders

    type IServiceProvider with
        /// **Description**
        ///   * Creates a new `IDependencyResolver` from this `IServiceProvider`.
        member this.ToDependencyResolver () = {
            new IDependencyResolver with
                member __.Resolve<'t> () = this.GetService typeof<'t> :?> 't
                member __.Resolve t = this.GetService t
        }

    /// **Description**
    ///  * Creates a new Schema from the provided IServiceProvider
    let create (provider: IServiceProvider) = new Schema(provider.ToDependencyResolver())

    type SchemaInfo = {
        Query: InjectionFunction<Schema, IObjectGraphType> option
        Mutation: InjectionFunction<Schema, IObjectGraphType> option
        Subscription: InjectionFunction<Schema, IObjectGraphType> option
        Types: InjectionFunction<Schema, IGraphType> list
    }


    /// **Description**
    ///   * Creates an empty SchemaInfo
    let newSchema = {
        Query = None
        Mutation = None
        Subscription = None
        Types = []
    }

    let internal composeObjectGraphType (x: #IObjectGraphType) = x :> IObjectGraphType

    type SchemaBuilder() =
        /// **Description**
        ///   * Creates a new Schema
        member __.Yield _ = newSchema

        /// **Description**
        ///   * Registers the provided `Query` object.
        [<CustomOperation "query">]
        member __.Query (schema, query) = {schema with Query = Some (query >> composeObjectGraphType)}

        /// **Description**
        ///   * Registers the provided `Mutation` object.
        [<CustomOperation "mutation">]
        member __.Mutation (schema, mutation) = {schema with Mutation = Some (mutation >> composeObjectGraphType)}

        /// **Description**
        ///   * Registers the provided `Subscription` object.
        [<CustomOperation "subscription">]
        member __.Subscription (schema, subscription) = {schema with Subscription = Some (subscription >> composeObjectGraphType)}

        /// **Description**
        ///   * Registers the provided types.
        [<CustomOperation "types">]
        member __.Types (schema, types: list<InjectionFunction<Schema, #IGraphType>>) = {schema with Types = schema.Types @ List.map (fun i -> i >> (fun j -> j :> IGraphType)) types}

        /// **Description**
        ///   * Processes the `schema` expressions' `SchemaInfo`.
        member __.Run {Query = query; Mutation = mutation; Subscription = subscription; Types = types} =
            fun (provider: IServiceProvider) ->
                let schema = new Schema(provider.ToDependencyResolver())
                let mutable types = List.map (fun ``type`` -> ``type`` schema) types

                maybeUnit {
                    let! query = query
                    let query = query schema
                    schema.Query <- query
                    types <- query :> IGraphType :: types
                }

                maybeUnit {
                    let! mutation = mutation
                    let mutation = mutation schema
                    schema.Mutation <- mutation
                    types <- mutation :> IGraphType :: types
                }

                maybeUnit {
                    let! subscription = subscription
                    let subscription = subscription schema
                    schema.Subscription <- subscription
                    types <- subscription :> IGraphType :: types
                }

                types
                |> List.toArray
                |> schema.RegisterTypes

                schema

    let schema = SchemaBuilder()

[<AutoOpen>]
module Argument =
    open GraphQL
    open GraphQL.Types
    open Iris.Option.Builders

    [<AutoOpen>]
    module rec Wrapper =
        type Argument<'input, 'output> = {
            name: string option
            value: ValueType<'output>
            description: string option
            validator: Validator<'input, 'output> option
        }

        let newArgument<'input, 'output> : Argument<'input, 'output> = {
            name = None
            value = Mandatory
            description = None
            validator = None
        }

    type ArgumentBuilder<'input>() =
        member __.Yield _ = newArgument<'input, 'input>

        /// Sets the name of the argument
        [<CustomOperation "name">]
        member __.Name (argument, name) = {argument with name = Some name}

        /// Sets the description of the argument
        [<CustomOperation "description">]
        member __.Description (argument, description) = {argument with description = Some description}

        /// Sets the default value of the argument
        /// Fails if we have set optional or mandatory
        [<CustomOperation "defaultValue">]
        member __.DefaultValue (argument, defaultValue) = {argument with value = DefaultValue defaultValue}

        /// Make the argument optional
        [<CustomOperation "optional">]
        member __.Optional argument = {argument with value = Optional}

        /// Make the argument optional
        [<CustomOperation "mandatory">]
        member __.Mandatory argument = {argument with value = Mandatory}

        /// Validate the argument
        [<CustomOperation "validate">]
        member __.Validate (argument, validator) =
            {
                name = argument.name
                value = argument.value
                description = argument.description
                validator = Some validator
            }

        /// Converts the elevated wrapper type into a function that can be called on initialization
        member __.Run (argument: Argument<'input, 'output>) =
            fun (schema: Schema) (field: FieldType) -> maybeOrThrow {
                let isNullable =
                    match argument.value with
                    | Mandatory _ -> false
                    | Optional | DefaultValue _ -> true
                let schemaType = typeof<'input>.GetGraphTypeFromType isNullable

                let queryArgument = ValidatedArgument(schemaType, field)

                let! name = argument.name
                queryArgument.Name <- name

                maybeUnit {
                    let! description = argument.description
                    queryArgument.Description <- description
                }

                maybeUnit {
                    let! defaultValue =
                        match argument.value with
                        | DefaultValue value -> Some value
                        | _ -> None
                    queryArgument.DefaultValue <- box defaultValue
                }

                maybeUnit {
                    let! validator = argument.validator
                    queryArgument.SetValidator validator
                }

                field.Arguments.Add queryArgument
            }

    let argument<'input> = ArgumentBuilder<'input> ()


[<AutoOpen>]
module Field =
    open System
    open Apollo
    open FSharp.Linq.RuntimeHelpers
    open FSharp.Quotations
    open GraphQL
    open GraphQL.Types
    open GraphQL.Resolvers
    open Iris
    open Iris.Option.Builders
    open Iris.Option.Operators

    [<AutoOpen>]
    module rec Wrapper =
        type FieldWrapper<'source, 'graph, 'value> = {
            arguments: (Schema -> FieldType -> unit) list
            value: ValueType<'value>
            description: string option
            getter: Expr<'source -> 'value> option
            getterType: Type option
            name: string option
            resolver: (ResolveFieldContext<'source> -> 'value obs) option
        }

        let defaultFieldWrapper<'source, 'graph, 'value> : FieldWrapper<'source, 'graph, 'value> = {
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

    type FieldBuilder<'graph when 'graph :> IGraphType>() =
        member __.Yield _ = defaultFieldWrapper<'source, 'graph, 'value>

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
            fun (schema: Schema) (graph: ComplexGraphType<'source>) -> maybeOrThrow {
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
                    let expression = LeafExpressionConverter.QuotationToLambdaExpression <@ Func<'source, 'value> %expression  @>
                    fieldType.Resolver <- ExpressionFieldResolver<'source, 'value> expression
                }

                maybeUnit {
                    let! resolver = field.resolver
                    let resolver = Func.from (resolver >> Observable.toTask)
                    fieldType.Resolver <- AsyncFieldResolver<'source, 'value> resolver
                    fieldType.Type <- typeof<'value>.GetGraphTypeFromType()
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

    let field<'graph when 'graph :> IGraphType> = FieldBuilder<'graph>()

[<AutoOpen>]
module Object =
    open GraphQL.Types
    open Iris.Option.Builders

    [<AutoOpen>]
    module rec Wrapper =
        type ObjectWrapper<'source> = {
            name: string option
            description: string option
            fields: (Schema -> ComplexGraphType<'source> -> unit) list
        }

        let defaultObjectWrapper<'source> : ObjectWrapper<'source> = {
            name = None
            description = None
            fields = []
        }

        let merge (lhs: ObjectWrapper<'source>) (rhs: ObjectWrapper<'source>) : ObjectWrapper<'source> = {
            name = Option.orElse lhs.name rhs.name
            description = Option.orElse lhs.description rhs.description
            fields = lhs.fields @ rhs.fields
        }

    let inline private get (x: ObjectWrapper<_>) = x

    type ComplexObjectBuilder<'source>() =
        // TODO: Is there a cleaner way to do this?
        abstract member Yield: _ -> ObjectWrapper<'source>
        default __.Yield _ = defaultObjectWrapper<'source>

        /// Sets the name of this object
        [<CustomOperation "name">]
        member __.Name (object, name) = {get object with name = Some name}

        /// Sets the description of this object
        [<CustomOperation "description">]
        member __.Description (object, description) = {get object with description = Some description}

        /// Adds fields to the object
        [<CustomOperation "fields">]
        member __.Fields (object, fields) = {get object with fields = object.fields @ fields}

        /// Imports another complex object type into the current object type
        [<CustomOperation "import">]
        member __.Import (object, other) = merge object other

    type ObjectBuilder<'source>() =
        inherit ComplexObjectBuilder<'source>()

        member __.Run (object: ObjectWrapper<'source>) =
            fun (schema: Schema) -> maybeOrThrow {
                let graph = ObjectGraphType<'source>()

                let! name = object.name
                graph.Name <- name

                maybeUnit {
                    let! description = object.description
                    graph.Description <- description
                }

                List.iter (fun object ->
                    object schema (graph :> ComplexGraphType<'source>))
                    object.fields

                return graph
            }

    type InputObjectBuilder<'source>() =
        inherit ComplexObjectBuilder<'source>()

        member __.Run (object: ObjectWrapper<'source>) =
            fun (schema: Schema) -> maybeOrThrow {
                let graph = InputObjectGraphType<'source>()

                let! name = object.name
                graph.Name <- name

                let! description = object.description
                graph.Description <- description

                List.iter (fun object -> object schema (graph :> ComplexGraphType<'source>)) object.fields

                return graph
            }

    type QueryBuilder() =
        inherit ObjectBuilder<obj>()

        override __.Yield _ = {defaultObjectWrapper<obj> with name = Some "Query"}

    type MutationBuilder() =
        inherit ObjectBuilder<obj>()

        override __.Yield _ = {defaultObjectWrapper<obj> with name = Some "Mutation"}

    type SubscriptionBuilder() =
        inherit ObjectBuilder<obj>()

        override __.Yield _ = {defaultObjectWrapper<obj> with name = Some "Subscription"}

    let complex<'source> = ComplexObjectBuilder<'source> ()
    let object<'source> = ObjectBuilder<'source> ()
    let input<'source> = InputObjectBuilder<'source> ()

    let query = QueryBuilder()
    let mutation = MutationBuilder()
    let subscription = SubscriptionBuilder()
