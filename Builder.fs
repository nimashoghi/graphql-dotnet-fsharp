module GraphQL.FSharp.Builder

// TODO
type 't res = FSharp.Validation.Result<'t>

let (|||) lhs rhs =
    Some <|
        match lhs with
        | Some value -> value
        | None -> rhs

[<AutoOpen>]
module Argument =
    open FSharp.Injection
    open FSharp.Validation
    open GraphQL.Types
    open Iris.Option.Builders

    [<AutoOpen>]
    module private rec Wrapper =
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
            name: string
            defaultValue: 'output option
            description: string option
            validator: ('input -> Result<'output>) option
        }

        let newArgument<'input, 'output> name : Argument<'input, 'output, IDefaultTag> = {
            name = name
            defaultValue = None
            description = None
            validator = None
        }

        let mergeValidators<'input, 'intermediary, 'output>
            (x: 'input -> Result<'intermediary>)
            (y: 'intermediary -> Result<'output>) =
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

    type ArgumentBuilder<'input, 'output>(name: string) =
        member __.Yield _ = newArgument<'input, 'input> name

        /// Sets the description of the argument
        [<CustomOperation "description">]
        member __.Description (argument, description) = {argument with description = Some description}

        /// Sets the default value of the argument
        [<CustomOperation "defaultValue">]
        member __.DefaultValue (argument, defaultValue) = {argument with defaultValue = Some defaultValue}

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

                let name = argument.name
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

    let argument<'input, 'output> name = ArgumentBuilder<'input, 'output> name


[<AutoOpen>]
module Field =
    open FSharp.Linq.RuntimeHelpers
    open FSharp.Injection
    open FSharp.Quotations
    open GraphQL.Types
    open GraphQL.Resolvers
    open Iris
    open Iris.Option.Builders
    open Iris.Types

    [<AutoOpen>]
    module private rec Wrapper =
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
                    name = field.name
                    resolver = field.resolver
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
            name: string option
            resolver: (ResolveFieldContext<'source> -> 'value obs) option
        }

        let defaultFieldWrapper<'source, 'graph, 'value, 'tag when 'tag :> ITag> : FieldWrapper<'source, 'graph, 'value, 'tag> = {
            arguments = []
            defaultValue = None
            description = None
            getter = None
            name = None
            resolver = None
        }

    type FieldBuilder<'graph when 'graph :> IGraphType>() =
        member __.Yield _ = defaultFieldWrapper<'source, 'graph, 'value, IDefaultTag>

        /// Sets the name of this field
        [<CustomOperation "name">]
        member __.Name (field, name) = {field with name = Some name}

        /// Sets the description of this field
        [<CustomOperation "description">]
        member __.Description (field, description) = {field with description = Some description}

        // /// Sets the description of this field
        // [<CustomOperation "description">]
        // member __.Description (field, description) = {field with description = Some description}

        /// Sets the description of this field
        [<CustomOperation "defaultValue">]
        member __.DefaultValue (field, defaultValue) = {field with defaultValue = Some defaultValue}

        /// Make the argument optional
        [<CustomOperation "optional">]
        member __.Optional argument = makeOptional argument

        /// Make the argument optional
        [<CustomOperation "mandatory">]
        member __.Mandatory argument = makeMandatory argument

        /// Sets the arguments of this fields
        [<CustomOperation "arguments">]
        member __.Arguments (field, arguments) = {field with arguments = arguments}

        [<CustomOperation "resolve">]
        member __.Resolve (field, resolver) = {field with resolver = resolver}

        [<CustomOperation "get">]
        member __.Get (field, [<ReflectedDefinition>] getter) = {field with getter = Some getter}

        /// Converts the elevated wrapper type into a function that can be called on initialization
        member __.Run (field: FieldWrapper<'source, 'graph, 'value, 'tag>) =
            fun (Inject (lookup: GraphTypesLookup)) (graph: ComplexGraphType<'source>) -> maybeUnit {
                let fieldType = FieldType()

                fieldType.Type <- typeof<'graph>
                
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

                List.iter (fun argument -> argument (Inject lookup) fieldType) field.arguments

                ignore <| graph.AddField fieldType
            }

    let field<'graph when 'graph :> IGraphType> = FieldBuilder<'graph>()

    [<CLIMutable>]
    type MyType = {
        name: string
    }

    let g = field<StringGraphType> {
        name "MyField"
        arguments [
            argument "sup" {
                description "sup"
            }
            argument "xD" {
                description "sup"
            }
        ]
        get (fun i -> i)
        description "sup"
    }

[<AutoOpen>]
module Object =
    open GraphQL.Types

    let private combine f g =
        fun arg ->
            f arg
            g arg
            ()

    type ObjectBuilder<'source, 'graph when 'graph :> ComplexGraphType<'source>>() =
        /// This is necessary for the DSL to work
        member __.Yield _ : 'graph -> unit = ignore

        /// Import another object expression into this object
        [<CustomOperation "import">]
        member __.Import (builder: 'graph -> unit, f: 'graph -> unit) =
            combine builder f

        /// Add a new field
        [<CustomOperation "field">]
        member __.Field (builder: 'graph -> unit, next: FieldBuilderState<'source, 'retn>) =
            fun arg ->
                builder arg
                next.maker (arg :> ComplexGraphType<'source>) |> ignore

        /// Set the name of the object
        [<CustomOperation "name">]
        member __.Name (builder: 'graph -> unit, name) =
            fun arg ->
                builder arg
                arg.Name <- name

        /// Set the description of the object
        [<CustomOperation "description">]
        member __.Description (builder: 'graph -> unit, description) =
            fun arg ->
                builder arg
                arg.Description <- description

[<AutoOpen>]
module Builders =
    open GraphQL.Types

    let a<'graph, 'ret when 'graph :> IGraphType> = ArgumentBuilder<'graph, 'ret>()
    let f<'graph, 'retn, 'source> = FieldMakerBuilder<'graph, 'retn, 'source>()
    let complex<'source, 'graph when 'graph :> ComplexGraphType<'source>> = ObjectBuilder<'source, 'graph>()
    let object<'source, 'graph when 'graph :> ObjectGraphType<'source>> = ObjectBuilder<'source, 'graph>()
    let input<'source, 'graph when 'graph :> InputObjectGraphType<'source>> = ObjectBuilder<'source, 'graph>()

[<AutoOpen>]
module Arguments =
    open FSharp.Control.Reactive
    open GraphQL.Types
    open Iris

    open Validation

    let private get<'t> name (ctx: ResolveFieldContext<_>) =
        match getValidationValue<'t> ctx.UserContext (metadataValueName ctx.FieldName name) with
        | Some value -> value
        | None -> ctx.GetArgument<'t> name

    type Arg<'t>() = member __.Item with get name = get<'t> name >> Observable.lift
    type ArgSync<'t>() = member __.Item with get name = get<'t> name

    let arg<'t> = Arg<'t>()
    let argSync<'t> = ArgSync<'t>()
