module GraphQL.FSharp.Builder

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

    let a<'graph, 'ret when 'graph :> IGraphType> = ArgumentBuilder<'graph, 'ret>()

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

        [<CustomOperation("argument")>]
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

    let f<'graph, 'retn, 'source> = FieldMakerBuilder<'graph, 'retn, 'source>()

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

    let complex<'source, 'graph when 'graph :> ComplexGraphType<'source>> = ObjectBuilder<'source, 'graph>()
    let object<'source, 'graph when 'graph :> ObjectGraphType<'source>> = ObjectBuilder<'source, 'graph>()
    let input<'source, 'graph when 'graph :> InputObjectGraphType<'source>> = ObjectBuilder<'source, 'graph>()
