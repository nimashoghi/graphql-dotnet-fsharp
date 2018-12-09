module GraphQL.FSharp.Builder

// TODO
type 't res = FSharp.Validation.Result<'t>

let (|||) lhs rhs =
    Some <|
        match lhs with
        | Some value -> value
        | None -> rhs

module ArgumentEx =
    open FSharp.Injection
    open FSharp.Validation
    open GraphQL.Types
    open Iris.Option.Builders

    [<AutoOpen>]
    module internal rec Wrapper =
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
        [<CustomOperation("description")>]
        member __.Description (argument, description) = {argument with description = Some description}

        /// Sets the default value of the argument
        [<CustomOperation("defaultValue")>]
        member __.DefaultValue (argument, defaultValue) = {argument with defaultValue = Some defaultValue}

        /// Make the argument optional
        [<CustomOperation("optional")>]
        member __.Optional argument = makeOptional argument

        /// Make the argument optional
        [<CustomOperation("mandatory")>]
        member __.Mandatory argument = makeMandatory argument

        /// Validation operation with chaining capability
        [<CustomOperation("validate")>]
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

    let arg<'input, 'output> name = ArgumentBuilder<'input, 'output> name

[<AutoOpen>]
module Argument =
    open GraphQL.Types
    open FSharp.Validation

    type ArgumentDefinition<'graph, 't> = {
        name: string
        description: string
        nullable: bool
        defaultValue: 't option
        validator: Validator<obj> option
    }

    let private empty<'graph, 't> : ArgumentDefinition<'graph, 't> = {
        name = "default"
        nullable = false
        description = ""
        validator = None
        defaultValue = None
    }

    type ArgumentBuilder<'graph, 'ret when 'graph :> IGraphType>() =
        member __.Yield _: ArgumentDefinition<'graph, 'ret> = empty<'graph, 'ret>

        /// Sets the name of the argument
        [<CustomOperation("name")>]
        member __.Name (argument: ArgumentDefinition<'graph, 'ret>, name) =
            {argument with name = name}

        /// Sets the description of the argument
        [<CustomOperation("description")>]
        member __.Description (argument: ArgumentDefinition<'graph, 'ret>, description) =
            {argument with description = description}

        /// Sets the default value of the argument
        [<CustomOperation("defaultValue")>]
        member __.DefaultValue (argument: ArgumentDefinition<'graph, 'ret>, defaultValue) =
            {argument with defaultValue = Some defaultValue}

        /// Make the argument nullable
        [<CustomOperation("nullable")>]
        member __.Nullable (argument: ArgumentDefinition<'graph, 'ret>) =
            {argument with nullable = true}

        /// Make the argument non-nullable
        [<CustomOperation("nonNullable")>]
        member __.NonNullable (argument: ArgumentDefinition<'graph, 'ret>) =
            {argument with nullable = false}

        [<CustomOperation("validator")>]
        member __.Validator (argument: ArgumentDefinition<'graph, 'ret>, validator: 'ret -> Result<'input>) =
            if Option.isSome argument.validator then failwith "There can only be one validator"
            let validator: Validator<obj> = (fun i -> i :?> 'ret) >> validator >> map box
            {argument with validator = Some validator}

[<AutoOpen>]
module Field =
    open System
    open System.Collections.Generic
    open GraphQL.Authorization
    open GraphQL.Builders
    open GraphQL.Types

    open Iris
    open Iris.Option
    open Iris.Types

    open Util

    type FieldBuilderState<'source, 'retn> = {
        maker: ComplexGraphType<'source> -> FieldBuilder<'source, 'retn>
        name: string option
    }

    let private wrap f state = {state with maker = f state.maker}

    let private empty<'graph, 'retn, 'source> = {
        maker = (fun (this: ComplexGraphType<'source>) -> this.Field<'graph, 'retn>())
        name = None
    }

    let private nullify t =
        let nonNull = typeof<NonNullGraphType>
        match t with
        // unwrap generic param
        | Assignable nonNull -> t.GenericTypeArguments.[0]
        | _ -> t

    let private unnullify t =
        let nonNull = typeof<NonNullGraphType>
        match t with
        | Assignable nonNull -> t
        | _ -> typeof<NonNullGraphType<_>>.GetGenericTypeDefinition().MakeGenericType t

    let guidIdType<'t> = if typeof<'t> = typeof<Guid> then Some (typeof<IdGraphType>) else None
    let isNullable<'t> = Nullable.GetUnderlyingType typeof<'t> <> null

    let getName (state: FieldBuilderState<'source, 'retn>) (builder: FieldBuilder<'source, 'retn>) =
        match state.name with
        | Some name -> name
        | None -> builder.FieldType.Name

    type FieldMakerBuilder<'graph, 'retn, 'source>() =
        member __.Yield _: FieldBuilderState<'source, 'retn> =
            empty<'graph, 'retn, 'source>

        [<CustomOperation("authorize")>]
        member __.Authorize (state: FieldBuilderState<'source, 'retn>, authorize) =
            wrap (fun builder this -> (builder this).AuthorizeWith authorize) state

        [<CustomOperation("get")>]
        member __.Get (state: FieldBuilderState<'source, 'retn>, expression) =
            // Specialization for Guid
            // TODO: check this
            let t: Type = Option.toObj guidIdType<'retn>
            match state.name with
            | Some value -> wrap (fun _ this -> this.Field<'retn>(value, expression, ``type`` = t, nullable = isNullable<'retn>)) state
            | None -> wrap (fun _ this -> this.Field<'retn>(expression, ``type`` = t, nullable = isNullable<'retn>)) state


        [<CustomOperation("getType")>]
        member __.GetType (state: FieldBuilderState<'source, 'retn>, expression, t) =
            match state.name with
            | Some value -> wrap (fun _ this -> this.Field<'retn>(value, expression, ``type`` = t, nullable = isNullable<'retn>)) state
            | None -> wrap (fun _ this -> this.Field<'retn>(expression, ``type`` = t, nullable = isNullable<'retn>)) state

        [<CustomOperation("name")>]
        member __.Name (state: FieldBuilderState<'source, 'retn>, name) =
            let state = wrap (fun builder this -> (builder this).Name name) state
            {state with name = Some name}

        [<CustomOperation("description")>]
        member __.Description (state: FieldBuilderState<'source, 'retn>, description) =
            wrap (fun builder this -> (builder this).Description description) state

        [<CustomOperation("defaultValue")>]
        member __.DefaultValue (state: FieldBuilderState<'source, 'retn>, value) =
            wrap (fun builder this -> (builder this).DefaultValue value) state

        [<CustomOperation("nullable")>]
        member __.Nullable (state: FieldBuilderState<'source, 'retn>) =
            wrap (fun builder this ->
                let builder = builder this
                builder.FieldType.Type  <- nullify builder.FieldType.Type
                builder) state

        [<CustomOperation("nonNullable")>]
        member __.NonNullable (state: FieldBuilderState<'source, 'retn>) =
            wrap (fun builder this ->
                let builder = builder this
                builder.FieldType.Type  <- unnullify builder.FieldType.Type
                builder) state

        [<CustomOperation("arg")>]
        member __.Argument (state: FieldBuilderState<'source, 'retn>, argument: ArgumentDefinition<'argGraph, 'arg>) =
            let ret =
                match argument.defaultValue with
                | Some defaultValue -> wrap (fun builder this ->
                    (builder this).Argument<'argGraph, 'arg>(argument.name, argument.description, defaultValue)) state
                | None ->
                    match argument.nullable with
                    | true -> wrap (fun builder this ->
                        (builder this).Argument<'argGraph, 'arg>(argument.name, argument.description)) state
                    | false -> wrap (fun builder this ->
                        (builder this).Argument<NonNullGraphType<'argGraph>, 'arg>(argument.name, argument.description)) state
            maybe {
                let! validator = argument.validator
                return
                    wrap (fun builder this ->
                        let builder = builder this
                        this.Metadata.[metadataName (getName state builder) argument.name] <- Some validator
                        builder) ret
            }
            |> Option.orElse (Some ret)
            |> Option.get

        // note: custom operation overloading is a bug
        // https://github.com/Microsoft/visualfsharp/pull/4949#issuecomment-424982354
        [<CustomOperation("resolve")>]
        member __.Resolve (state: FieldBuilderState<'source, 'retn>, f: ResolveFieldContext<'source> -> 'retn) =
            wrap (fun builder this -> Func.from f |> (builder this).Resolve) state
        member __.Resolve (state: FieldBuilderState<'source, 'retn>, f: ResolveFieldContext<'source> -> 'retn task) =
            wrap (fun builder this -> resolveAsync (builder this) f) state
        // TODO: Set up proper error handling
        member __.Resolve (state: FieldBuilderState<'source, 'retn>, f: ResolveFieldContext<'source> -> 'retn obs) =
            wrap (fun builder this -> resolveObservable (builder this) f) state
        member __.Resolve<'a, 'retn when 'retn :> List<'a>> (state: FieldBuilderState<'source, List<'a>>, f: ResolveFieldContext<'source> -> 'a obs) =
            wrap (fun builder this -> resolveObservableList (builder this) f) state

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
        [<CustomOperation("import")>]
        member __.Import (builder: 'graph -> unit, f: 'graph -> unit) =
            combine builder f

        /// Add a new field
        [<CustomOperation("field")>]
        member __.Field (builder: 'graph -> unit, next: FieldBuilderState<'source, 'retn>) =
            fun arg ->
                builder arg
                next.maker (arg :> ComplexGraphType<'source>) |> ignore

        /// Set the name of the object
        [<CustomOperation("name")>]
        member __.Name (builder: 'graph -> unit, name) =
            fun arg ->
                builder arg
                arg.Name <- name

        /// Set the description of the object
        [<CustomOperation("description")>]
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
