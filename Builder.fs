module GraphQL.FSharp.Builder

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
        member __.Types (schema, types) = {schema with Types = schema.Types @ types}

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
module Optional =
    open GraphQL.Types

    type IOptionalTag = interface end
    type IOptionalDefaultTag = inherit IOptionalTag
    type IOptionalDefaultValueTag = inherit IOptionalTag
    type IOptionalMandatoryTag = inherit IOptionalTag
    type IOptionalOptionalTag = inherit IOptionalTag

    let internal (|Mandatory|Optional|DefaultValue|) tag =
        if typeof<IOptionalOptionalTag>.IsAssignableFrom tag then Optional
        elif typeof<IOptionalDefaultValueTag>.IsAssignableFrom tag then DefaultValue
        else Mandatory

    let wrapTag<'tag> t =
        match typeof<'tag> with
        | Mandatory | DefaultValue -> NonNullGraphType t :> IGraphType
        | Optional -> t


[<AutoOpen>]
module Argument =
    open GraphQL.Types
    open Iris.Option.Builders
    open Iris.Option.Operators

    [<AutoOpen>]
    module rec Wrapper =
        [<AutoOpen>]
        module Optional =
            let changeOptionalStatus<'tag, 'input, 'output when 'tag :> IOptionalTag>
                (argument: Argument<'input, 'output, IOptionalDefaultTag>)
                : Argument<'input, 'output, 'tag> = {
                    name = argument.name
                    defaultValue = argument.defaultValue
                    description = argument.description
                    validator = argument.validator
                }

            let makeMandatory = changeOptionalStatus<IOptionalMandatoryTag, _, _>
            let makeOptional = changeOptionalStatus<IOptionalOptionalTag, _, _>

        type Argument<'input, 'output, 'tag when 'tag :> IOptionalTag> = {
            name: string option
            defaultValue: 'output option
            description: string option
            validator: Validator<'input, 'output> option
        }

        let newArgument<'input, 'output> : Argument<'input, 'output, IOptionalDefaultTag> = {
            name = None
            defaultValue = None
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
        member __.DefaultValue (argument: Argument<_, _, IOptionalDefaultTag>, defaultValue) = {argument with defaultValue = Some defaultValue}

        /// Make the argument optional
        [<CustomOperation "optional">]
        member __.Optional argument = makeOptional argument

        /// Make the argument optional
        [<CustomOperation "mandatory">]
        member __.Mandatory argument = makeMandatory argument

        /// Validation operation with chaining capability
        [<CustomOperation "validate">]
        member __.Validate (argument: Argument<_, _, _>, validator) : Argument<_, _, _> =
            {
                name = argument.name
                defaultValue = argument.defaultValue
                description = argument.description
                validator = Some validator
            }

        /// Converts the elevated wrapper type into a function that can be called on initialization
        member __.Run (argument: Argument<'input, 'output, 'tag>) =
            fun (schema: Schema) (field: FieldType) -> maybeUnit {
                let queryArgument = ValidatedArgument(wrapTag<'tag> (schema.FindType typeof<'input>.Name), field)

                let! name = argument.name
                queryArgument.Name <- name

                let! description = argument.description ||| ""
                queryArgument.Description <- description

                do! maybe {
                    let! defaultValue = argument.defaultValue
                    queryArgument.DefaultValue <- box defaultValue
                }

                do! maybe {
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
        [<AutoOpen>]
        module Optional =
            let changeOptionalStatus<'tag, 'source, 'graph, 'value when 'tag :> IOptionalTag>
                (field: FieldWrapper<'source, 'graph, 'value, IOptionalDefaultTag>)
                : FieldWrapper<'source, 'graph, 'value, 'tag> = {
                    arguments = field.arguments
                    defaultValue = field.defaultValue
                    description = field.description
                    getter = field.getter
                    getterType = field.getterType
                    name = field.name
                    resolver = field.resolver
                }

            let makeMandatory = changeOptionalStatus<IOptionalMandatoryTag, _, _, _>
            let makeOptional = changeOptionalStatus<IOptionalOptionalTag, _, _, _>


        type FieldWrapper<'source, 'graph, 'value, 'tag when 'tag :> IOptionalTag> = {
            arguments: (Schema -> FieldType -> unit) list
            defaultValue: 'value option
            description: string option
            getter: Expr<'source -> 'value> option
            getterType: Type option
            name: string option
            resolver: (ResolveFieldContext<'source> -> 'value obs) option
        }

        let defaultFieldWrapper<'source, 'graph, 'value> : FieldWrapper<'source, 'graph, 'value, IOptionalDefaultTag> = {
            arguments = []
            defaultValue = None
            description = None
            getter = None
            getterType = None
            name = None
            resolver = None
        }

        let checkFieldGetter (field: FieldWrapper<_, _, _, _>) =
            if Option.isSome field.getter && Option.isSome field.resolver then
                failwith "Field cannot have both a getter and a resolver"

    type FieldBuilder<'graph when 'graph :> IGraphType>() =
        member __.Yield _ = defaultFieldWrapper<'source, 'graph, 'value>

        /// Sets the name of this field
        [<CustomOperation "name">]
        member __.Name (field: FieldWrapper<_, _, _, _>, name) = {field with name = Some name}

        /// Sets the description of this field
        [<CustomOperation "description">]
        member __.Description (field: FieldWrapper<_, _, _, _>, description) = {field with description = Some description}

        /// Sets the description of this field
        /// Fails if we have set optional or mandatory
        [<CustomOperation "defaultValue">]
        member __.DefaultValue (field: FieldWrapper<_, _, _, IOptionalDefaultTag>, defaultValue) : FieldWrapper<_, _, _, IOptionalDefaultValueTag> =
            {
                arguments = field.arguments
                description = field.description
                getter = field.getter
                getterType = field.getterType
                name = field.name
                resolver = field.resolver
                defaultValue = Some defaultValue
             }

        /// Make the argument optional
        [<CustomOperation "optional">]
        member __.Optional argument = makeOptional argument

        /// Make the argument optional
        [<CustomOperation "mandatory">]
        member __.Mandatory argument = makeMandatory argument

        /// Sets the arguments of this fields
        [<CustomOperation "arguments">]
        member __.Arguments (field, arguments) = {field with arguments = field.arguments @ arguments}

        /// Gets a specific field
        [<CustomOperation "get">]
        member __.Get (field, [<ReflectedDefinition>] getter) = {field with getter = Some getter}

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
        member __.Run (field: FieldWrapper<'source, 'graph, 'value, 'tag>) =
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
                    let expression = LeafExpressionConverter.QuotationToLambdaExpression <@ Func.from %expression @>
                    fieldType.Resolver <- ExpressionFieldResolver<'source, 'value> expression
                }

                maybeUnit {
                    let! resolver = field.resolver
                    let resolver = Func.from (resolver >> Observable.toTask)
                    fieldType.Resolver <- AsyncFieldResolver<'source, 'value> resolver
                    fieldType.Type <- typeof<'value>.GetGraphTypeFromType()
                }

                List.iter (fun argument -> argument schema fieldType) field.arguments

                // check if optional
                do
                    match typeof<'tag> with
                    | DefaultValue ->
                        maybeUnit {
                            let! defaultValue = field.defaultValue
                            fieldType.DefaultValue <- box defaultValue
                        }
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

    type ComplexObjectBuilder<'source>() =
        member __.Yield _ = defaultObjectWrapper<'source>

        /// Sets the name of this object
        [<CustomOperation "name">]
        member __.Name (object: ObjectWrapper<_>, name) = {object with name = Some name}

        /// Sets the description of this object
        [<CustomOperation "description">]
        member __.Description (object: ObjectWrapper<_>, description) = {object with description = Some description}

        /// Adds fields to the object
        [<CustomOperation "fields">]
        member __.Fields (object: ObjectWrapper<'source>, fields) = {object with fields = object.fields @ fields}

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

    type MutationBuilder() =
        inherit ObjectBuilder<obj>()

    type SubscriptionBuilder() =
        inherit ObjectBuilder<obj>()


    let complex<'source> = ComplexObjectBuilder<'source> ()
    let object<'source> = ObjectBuilder<'source> ()
    let input<'source> = InputObjectBuilder<'source> ()

    let query = QueryBuilder()
    let mutation = MutationBuilder()
    let subscription = SubscriptionBuilder()
