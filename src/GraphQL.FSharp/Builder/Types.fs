module GraphQL.FSharp.BuilderTypes

open System
open System.Threading.Tasks
open FSharp.Quotations
open GraphQL.Conversion
open GraphQL.Resolvers
open GraphQL.Subscription

open GraphQL.FSharp.BuilderBase
open GraphQL.FSharp.BuilderUtils
open GraphQL.FSharp.Resolvers
open GraphQL.FSharp.Types
open GraphQL.FSharp.Utils

type ArgumentBuilder<'t> (``type``) =
    inherit TypedEntityBuilder<Argument<'t>> ()

    member __.Yield (_: unit): State<Argument<'t>> =
        [
            yield operation 0 <| Argument.trySetType ``type`` typeof<'t>
        ]

type DirectiveBuilder () =
    inherit EntityBuilder<Directive> ()

    member __.Yield (_: unit): State<Directive> = []

    [<CustomOperation "arguments">]
    member __.Arguments (state: State<Directive>, arguments: Argument list) =
        setArguments (Argument.makeArguments arguments) @@ state

    [<CustomOperation "locations">]
    member __.Locations (state: State<Directive>, locations) =
        appendToState state (fun this -> this.PossibleLocations <- locations)

type EnumerationBuilder () =
    inherit BasicGraphTypeBuilder<Enumeration> ()

    member __.Auto<'source> (?Name, ?Description, ?Metadata) =
        let graph: Enumeration<'source> =
            Enumeration<'source> (
                Name = typeof<'source>.Name
            )

        Enum.addEnumValues graph

        Name |> Option.iter (fun name -> graph.Name <- name)
        Description |> Option.iter (fun description -> graph.Description <- description)
        Metadata |> Option.iter (fun metadata -> graph.Metadata <- dict metadata)

        graph

    member __.Yield (_: unit): State<Enumeration> = []

    [<CustomOperation "cases">]
    member __.Cases (state: State<Enumeration>, values) =
        appendToState state (fun this ->
            values
            |> List.iter this.AddValue
        )

type FieldBuilder<'arguments, 'field, 'source> (``type``, ?name) =
    inherit TypedFieldBuilder<Field<'arguments, 'field, 'source>> ()

    member __.Yield (_: unit): State<Field<'arguments, 'field, 'source>> =
        [
            yield operation 0 <| Argument.trySetType ``type`` typeof<'field>

            match name with
            | Some name -> yield operation 0 <| setName name
            | None -> ()
        ]

    [<CustomOperation "validate">]
    member __.Validate (state: State<Field<'arguments, 'field, 'source>>, validator: 'arguments -> Result<'arguments, 'error list> Task) =
        Field.validate validator :: state

    [<CustomOperation "arguments">]
    member __.Arguments (state: State<Field<'arguments, 'field, 'source>>, arguments: Argument list) =
        setArguments (Argument.makeArguments arguments) @@ state

    [<CustomOperation "argumentDescription">]
    member __.ArgumentDescription (state: State<Field<'arguments, 'field, 'source>>, descriptions: (string * string) list) =
        operation 100 (
            fun (this: Field<'arguments, 'field, 'source>) ->
                let arguments =
                    this.Arguments
                    |> Seq.groupBy (fun argument -> argument.Name)
                    |> dict

                for key, value in descriptions do
                    match arguments.TryGetValue key with
                    | true, arguments ->
                        arguments
                        |> Seq.iter (fun argument -> argument.Description <- value)
                    | _ -> failwithf "Was not able to find argument with key '%s' in field '%s'" key this.Name

                this
        ) :: state

    [<CustomOperation "manualResolve">]
    member __.ManualResolve (state: State<Field<'arguments, 'field, 'source>>, resolver: ResolveContext<'source> -> 'field) =
        appendToState state (
            fun this ->
                this.Resolver <- resolve resolver
        )

    [<CustomOperation "prop">]
    member __.PropertyAsync (state: State<Field<'arguments, 'field, 'source>>, [<ReflectedDefinition true>] expr: Expr<'source -> Task<'field>>) =
        Field.setField (|FieldName|_|) (withSource >> resolveAsync) expr state

    [<CustomOperation "method">]
    member __.MethodAsync (state: State<Field<'arguments, 'field, 'source>>, [<ReflectedDefinition true>] expr: Expr<'source -> 'arguments -> Task<'field>>) =
        Field.setField (|MethodName|_|) Field.resolveMethod expr state
        |> Field.addArguments<'arguments, 'field, 'source>

    [<CustomOperation "resolve">]
    member __.ResolveAsync (state: State<Field<'arguments, 'field, 'source>>, resolver: ResolveContext<'source> -> 'arguments -> Task<'field>) =
        appendToState state (
            fun this ->
                this.Resolver <- Field.resolveCtxMethodAsync resolver
        )
        |> Field.addArguments<'arguments, 'field, 'source>

    [<CustomOperation "subscribe">]
    member __.SubscribeAsync (state: State<Field<'arguments, 'field, 'source>>, subscribe: ResolveEventStreamContext<'source> -> Task<IObservable<'field>>) =
        appendToState state (
            fun this ->
                this.AsyncSubscriber <- AsyncEventStreamResolver<_, _> (Func<_, _> subscribe)

                if isNull this.Resolver
                then this.Resolver <- resolve (fun (ctx: ResolveContext<'source>) -> unbox<'field> ctx.Source)
        )

    member inline __.Run (state: State<Field<'arguments, 'field, 'source>>): Field<'source> =
        handleNonNullTypes @@ state
        |> runState
        :> Field<'source>

type InputObjectBuilder<'source> () =
    inherit ComplexGraphTypeBuilder<InputObject<'source>, 'source> ()

    member __.Yield (_: unit): State<InputObject<'source>> =
        [
            yield operation 0 <| setName typeof<'source>.Name
        ]

type InterfaceBuilder<'source> () =
    inherit ComplexGraphTypeBuilder<Interface<'source>, 'source> ()

    member __.Yield (_: unit): State<Interface<'source>> =
        [
            yield operation 0 <| setName typeof<'source>.Name
        ]

type ObjectBuilder<'source> () =
    inherit ComplexGraphTypeBuilder<Object<'source>, 'source> ()

    member __.Yield (_: unit): State<Object<'source>> =
        [
            yield operation 0 <| setName typeof<'source>.Name
        ]

    [<CustomOperation "interfaces">]
    member __.Interfaces (state: State<Object<'source>>, interfaces) =
        appendToState state (fun this ->
            interfaces
            |> List.iter this.AddResolvedInterface
        )

type SchemaBuilder () =
    inherit ConfigureBuilder<Schema> ()

    let setFieldNameConverter (schema: Schema) =
        schema.FieldNameConverter <-
            {
                new IFieldNameConverter with
                    member __.NameFor (name, _) = name
            }
        schema

    member __.Yield (_: unit): State<Schema> =
        [
            yield operation 0 <| setFieldNameConverter
        ]

    [<CustomOperation "query">]
    member __.Query (state: State<Schema>, endpoints: Field<obj> list) =
        appendToState state (
            fun this ->
                let object =
                    Object<obj> (
                        Name = "Query",
                        Description = "The queriesss accepted in this GraphQL API."
                    )
                endpoints
                |> List.iter (object.AddField >> ignore)
                this.Query <- object
        )

    [<CustomOperation "mutation">]
    member __.Mutation (state: State<Schema>, endpoints: Field<obj> list) =
        appendToState state (
            fun this ->
                let object =
                    Object<obj> (
                        Name = "Mutation",
                        Description = "The mutations accepted in this GraphQL API."
                    )
                endpoints
                |> List.iter (object.AddField >> ignore)
                this.Mutation <- object
        )

    [<CustomOperation "subscription">]
    member __.Subscription (state: State<Schema>, endpoints: Field<obj> list) =
        appendToState state (
            fun this ->
                let object =
                    Object<obj> (
                        Name = "Subscription",
                        Description = "The subscriptions accepted in this GraphQL API."
                    )
                endpoints
                |> List.iter (object.AddField >> ignore)
                this.Subscription <- object
        )

    [<CustomOperation "types">]
    member __.Types (state: State<Schema>, types) =
        appendToState state (fun this ->
            types
            |> Schema.handleInterfaces
            |> List.toArray
            |> this.RegisterTypes
        )

type UnionBuilder () =
    inherit BasicGraphTypeBuilder<Union> ()

    member __.Auto<'source> (?Name, ?Description, ?Metadata) =
        let graph: Union<'source> =
            Union<'source> (
                Name = typeof<'source>.Name
            )

        Union.addPossibleTypes graph

        Name |> Option.iter (fun name -> graph.Name <- name)
        Description |> Option.iter (fun description -> graph.Description <- description)
        Metadata |> Option.iter (fun metadata -> graph.Metadata <- dict metadata)

        graph

    member __.Yield (_: unit): State<Union> = []

    [<CustomOperation "cases">]
    member __.Cases (state: State<Union>, cases) =
        appendToState state (fun this -> this.PossibleTypes <- List.toSeq cases)
