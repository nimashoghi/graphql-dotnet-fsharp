module GraphQL.FSharp.Builder

// TODO
type 't res = FSharp.Validation.Result<'t>

let (|||) lhs rhs =
    Some <|
        match lhs with
        | Some value -> value
        | None -> rhs

[<AutoOpen>]
module Arguments =
    open GraphQL.Types
    open Iris

    open Validation

    type ArgumentHelper<'t>() =
        member __.get name (ctx: #ResolveFieldContext<_>) =
            let value = 
                match getValidationValue<'t> ctx.UserContext (metadataValueName ctx.FieldName name) with
                | Some value -> value
                | None -> ctx.GetArgument<'t> name
            Observable.lift value
    let arg<'t> = ArgumentHelper<'t>()

[<AutoOpen>]
module Argument =
    open FSharp.Injection
    open FSharp.Validation
    open GraphQL.Types
    open Iris.Option.Builders

    module rec Wrapper =
        [<AutoOpen>]
        module Optional =
            type ITag = interface end
            type IDefaultTag = inherit ITag
            type IMandatoryTag = inherit ITag
            type IOptionalTag = inherit ITag

            let (|Mandatory|Optional|) tag =
                if typeof<IOptionalTag>.IsAssignableFrom tag then Optional
                else Mandatory

            let changeOptionalStatus<'tag, 'input, 'output when 'tag :> ITag>
                (argument: Argument<'input, 'output, IDefaultTag>)
                : Argument<'input, 'output, 'tag> = {
                    name = argument.name
                    defaultValue = argument.defaultValue
                    description = argument.description
                    validator = argument.validator
                }

            let makeMandatory = changeOptionalStatus<IMandatoryTag, _, _>
            let makeOptional = changeOptionalStatus<IOptionalTag, _, _>

        [<AutoOpen>]
        module Validator =
            let defaultValidator x = box x :?> 'output |> retn

        type Argument<'input, 'output, 'tag when 'tag :> ITag> = {
            name: string option
            defaultValue: 'output option
            description: string option
            validator: ('input -> 'output res) option
        }

        let newArgument<'input, 'output> : Argument<'input, 'output, IDefaultTag> = {
            name = None
            defaultValue = None
            description = None
            validator = None
        }

        let mergeValidators<'input, 'intermediary, 'output>
            (x: 'input -> 'intermediary res)
            (y: 'intermediary -> 'output res) =
            fun input ->
                match x input with
                | Ok value -> y value
                | Error err -> Error err

        let extend
            (argument: Argument<'input, 'intermediary, _>)
            (validator: 'intermediary -> 'output res) : Argument<'input, 'output, _> =
                match argument.validator with
                | Some prev ->
                    {
                        name = argument.name
                        defaultValue = None
                        description = argument.description
                        validator = Some (mergeValidators prev validator)
                    }
                // in this case, 'intermediary = 'input
                | None ->
                    {
                        name = argument.name
                        defaultValue = None
                        description = argument.description
                        validator = Some (fun x -> x |> box :?> 'intermediary |> validator)
                    }

        let wrapTag<'tag> t =
            match typeof<'tag> with
            | Mandatory -> NonNullGraphType t :> IGraphType
            | Optional -> t

    open Wrapper

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
        member __.DefaultValue (argument: Argument<_, _, IDefaultTag>, defaultValue) =
            {argument with defaultValue = Some defaultValue}
        
        /// Make the argument optional
        [<CustomOperation "optional">]
        member __.Optional argument = makeOptional argument

        /// Make the argument optional
        [<CustomOperation "mandatory">]
        member __.Mandatory argument = makeMandatory argument

        /// Validation operation with chaining capability
        [<CustomOperation "validate">]
        member __.Validate (argument, validator) = extend argument validator

        /// Converts the elevated wrapper type into a function that can be called on initialization
        member __.Run (argument: Argument<'input, 'output, 'tag>) =
            fun (Inject (lookup: GraphTypesLookup)) (field: FieldType) -> maybeUnit {
                let queryArgument = QueryArgument (wrapTag<'tag> lookup.[typeof<'input>])

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
                    field.Metadata.[name] <- validator
                }

                field.Arguments.Add queryArgument
            }

    let argument<'input> = ArgumentBuilder<'input> ()


[<AutoOpen>]
module Field =
    open System
    open FSharp.Linq.RuntimeHelpers
    open FSharp.Injection
    open FSharp.Quotations
    open GraphQL.Types
    open GraphQL.Resolvers
    open Iris
    open Iris.Option.Builders
    open Iris.Types

    module rec Wrapper =
        [<AutoOpen>]
        module Optional =
            type ITag = interface end
            type IDefaultTag = inherit ITag
            type IMandatoryTag = inherit ITag
            type IOptionalTag = inherit ITag

            let (|Mandatory|Optional|) tag =
                if typeof<IOptionalTag>.IsAssignableFrom tag then Optional
                else Mandatory

            let changeOptionalStatus<'tag, 'source, 'graph, 'value when 'tag :> ITag>
                (field: FieldWrapper<'source, 'graph, 'value, IDefaultTag>)
                : FieldWrapper<'source, 'graph, 'value, 'tag> = {
                    arguments = field.arguments
                    defaultValue = field.defaultValue
                    description = field.description
                    getter = field.getter
                    getterType = field.getterType
                    name = field.name
                    resolver = field.resolver
                    validators = field.validators
                }

            let makeMandatory = changeOptionalStatus<IMandatoryTag, _, _, _>
            let makeOptional = changeOptionalStatus<IOptionalTag, _, _, _>

            let wrapTag<'tag> t =
                match typeof<'tag> with
                | Mandatory -> NonNullGraphType t :> IGraphType
                | Optional -> t

        type FieldWrapper<'source, 'graph, 'value, 'tag when 'tag :> ITag> = {
            arguments: (Inject<GraphTypesLookup> -> FieldType -> unit) list
            defaultValue: 'value option
            description: string option
            getter: Expr<'source -> 'value> option
            getterType: Type option
            name: string option
            resolver: (ResolveFieldContext<'source> -> 'value obs) option
            validators: ('value -> 'value res) list
        }

        let defaultFieldWrapper<'source, 'graph, 'value> : FieldWrapper<'source, 'graph, 'value, IDefaultTag> = {
            arguments = []
            defaultValue = None
            description = None
            getter = None
            getterType = None
            name = None
            resolver = None
            validators = []
        }

    open Wrapper

    type FieldBuilder<'graph when 'graph :> IGraphType>() =
        member __.Yield _ = defaultFieldWrapper<'source, 'graph, 'value>

        /// Sets the name of this field
        [<CustomOperation "name">]
        member __.Name (field, name) = {field with name = Some name}

        /// Sets the description of this field
        [<CustomOperation "description">]
        member __.Description (field, description) = {field with description = Some description}

        /// Validation operation with chaining capability
        [<CustomOperation "validate">]
        member __.Validate (field, validator) = {field with validators = validator :: field.validators}

        /// Sets the description of this field
        /// Fails if we have set optional or mandatory
        [<CustomOperation "defaultValue">]
        member __.DefaultValue (field: FieldWrapper<_, _, _, IDefaultTag>, defaultValue) = {field with defaultValue = Some defaultValue}

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
            fun (Inject (lookup: GraphTypesLookup)) (graph: ComplexGraphType<'source>) -> maybeUnit {
                let fieldType = FieldType()

                let! type' = field.getterType ||| typeof<'graph>
                fieldType.Type <- type'
                
                let! name = field.name
                fieldType.Name <- name

                let! description = field.description ||| ""
                fieldType.Description <- description

                do! maybe {
                    let! expression = field.getter
                    let expression = LeafExpressionConverter.QuotationToLambdaExpression <@ Func.from %expression @>
                    fieldType.Resolver <- ExpressionFieldResolver<'source, 'value> expression
                }

                do! maybe {
                    let! resolver = field.resolver
                    let resolver = Func.from (resolver >> Observable.toTask)
                    fieldType.Resolver <- AsyncFieldResolver<'source, 'value> resolver
                }

                fieldType.Metadata.["validators"] <- field.validators

                List.iter (fun argument -> argument (Inject lookup) fieldType) field.arguments

                ignore <| graph.AddField fieldType
            }

    let field<'graph when 'graph :> IGraphType> = FieldBuilder<'graph>()

[<AutoOpen>]
module Object =
    open FSharp.Injection
    open GraphQL.Types
    open Iris.Option.Builders

    module rec Wrapper =
        type ObjectWrapper<'source> = {
            name: string option
            description: string option
            fields: (Inject<GraphTypesLookup> -> ComplexGraphType<'source> -> unit) list
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

    open Wrapper

    type ComplexObjectBuilder<'source>() =
        member __.Yield _ = defaultObjectWrapper<'source>

        /// Sets the name of this object
        [<CustomOperation "name">]
        member __.Name (object, name) = {object with name = Some name}

        /// Sets the description of this object
        [<CustomOperation "description">]
        member __.Description (object, description) = {object with description = Some description}

        /// Adds fields to the object
        [<CustomOperation "fields">]
        member __.Fields (object: ObjectWrapper<'source>, fields) = {object with fields = object.fields @ fields}

        /// Imports another complex object type into the current object type
        [<CustomOperation "import">]
        member __.Import (object, other) = merge object other

    type ObjectBuilder<'source>() =
        inherit ComplexObjectBuilder<'source>()

        member __.Run (object: ObjectWrapper<'source>) =
            fun (Inject (lookup: GraphTypesLookup)) -> maybeOrThrow {
                let graph = ObjectGraphType<'source>()
                
                let! name = object.name
                graph.Name <- name

                let! description = object.description
                graph.Description <- description

                List.iter (fun object -> object (Inject lookup) (graph :> ComplexGraphType<'source>)) object.fields
                
                return graph
            }

    type InputObjectBuilder<'source>() =
        inherit ComplexObjectBuilder<'source>()

        member __.Run (object: ObjectWrapper<'source>) =
            fun (Inject (lookup: GraphTypesLookup)) -> maybeOrThrow {
                let graph = InputObjectGraphType<'source>()
                
                let! name = object.name
                graph.Name <- name

                let! description = object.description
                graph.Description <- description

                List.iter (fun object -> object (Inject lookup) (graph :> ComplexGraphType<'source>)) object.fields
                
                return graph
            }

    let complex<'source> = ComplexObjectBuilder<'source> ()
    let object<'source> = ObjectBuilder<'source> ()
    let input<'source> = InputObjectBuilder<'source> ()

