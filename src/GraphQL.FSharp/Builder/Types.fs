namespace GraphQL.FSharp

open System
open System.Threading.Tasks
open FSharp.Quotations
open GraphQL.Conversion
open GraphQL.Resolvers
open GraphQL.Subscription
open GraphQL.Types

open GraphQL.FSharp.BuilderBase
open GraphQL.FSharp.BuilderUtils
open GraphQL.FSharp.Resolvers
open GraphQL.FSharp.Types
open GraphQL.FSharp.Utils

[<AutoOpen>]
module ArgumentHelpers =
    let inline trySetType graphType systemType (x: ^t) =
        if graphType <> __
        then setGraphType graphType x
        else setType systemType x

    let makeArguments arguments =
        arguments
        |> List.map (fun arg -> arg :> QueryArgument)
        |> List.toArray
        |> QueryArguments

type ArgumentBuilder<'t> (``type``) =
    inherit TypedEntityBuilder<Argument<'t>> ()

    member __.Yield (_: unit): State<Argument<'t>> =
        [
            yield operation 0 <| trySetType ``type`` typeof<'t>
        ]

type DirectiveBuilder () =
    inherit EntityBuilder<Directive> ()

    member __.Yield (_: unit): State<Directive> = []

    [<CustomOperation "arguments">]
    member __.Arguments (state: State<Directive>, arguments: Argument list) =
        setArguments (makeArguments arguments) @@ state

    [<CustomOperation "locations">]
    member __.Locations (state: State<Directive>, locations) =
        appendToState state (fun this -> this.PossibleLocations <- locations)

type EnumerationBuilder () =
    inherit BasicGraphTypeBuilder<Enumeration> ()

    member __.auto<'source> (?Name, ?Description, ?Metadata) =
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

type FieldBuilder<'field, 'source> (``type``, ?name) =
    inherit TypedFieldBuilder<Field<'field, 'source>> ()

    member __.Yield (_: unit): State<Field<'field, 'source>> =
        [
            yield operation 0 <| trySetType ``type`` typeof<'field>

            match name with
            | Some name -> yield operation 0 <| setName name
            | None -> ()
        ]

    [<CustomOperation "arguments">]
    member __.Arguments (state: State<Field<'field, 'source>>, arguments: Argument list) =
        setArguments (makeArguments arguments) @@ state

    [<CustomOperation "prop">]
    member __.Property (state: State<Field<'field, 'source>>, [<ReflectedDefinition true>] expr: Expr<'source -> 'field>) =
        Field.setField (|FieldName|_|) (withSource >> resolve) expr state

    [<CustomOperation "propAsync">]
    member __.PropertyAsync (state: State<Field<'field, 'source>>, [<ReflectedDefinition true>] expr: Expr<'source -> Task<'field>>) =
        Field.setField (|FieldName|_|) (withSource >> resolveAsync) expr state

    [<CustomOperation "method">]
    member __.Method (state: State<Field<'field, 'source>>, [<ReflectedDefinition true>] expr: Expr<'source -> 'arguments -> 'field>) =
        Field.setField (|MethodName|_|) Field.resolveMethod expr state
        |> Field.addArguments<'arguments, 'field, 'source>

    [<CustomOperation "methodAsync">]
    member __.MethodAsync (state: State<Field<'field, 'source>>, [<ReflectedDefinition true>] expr: Expr<'source -> 'arguments -> Task<'field>>) =
        Field.setField (|MethodName|_|) Field.resolveMethod expr state
        |> Field.addArguments<'arguments, 'field, 'source>

    [<CustomOperation "resolve">]
    member __.Resolve (state: State<Field<'field, 'source>>, resolver: ResolveContext<'source> -> 'arguments -> 'field) =
        appendToState state (fun this ->
            this.Resolver <- Field.resolveCtxMethod resolver
        )
        |> Field.addArguments<'arguments, 'field, 'source>

    [<CustomOperation "resolveAsync">]
    member __.ResolveAsync (state: State<Field<'field, 'source>>, resolver: ResolveContext<'source> -> 'arguments -> Task<'field>) =
        appendToState state (fun this ->
            this.Resolver <- Field.resolveCtxMethodAsync resolver
        )
        |> Field.addArguments<'arguments, 'field, 'source>

    [<CustomOperation "subscribe">]
    member __.Subscribe (state: State<Field<'field, 'source>>, subscribe: ResolveEventStreamContext<'source> -> IObservable<'field>) =
        appendToState state (fun this ->
            this.Subscriber <- EventStreamResolver<_, _> (Func<_, _> subscribe)
            this.Resolver <- resolve (fun (ctx: ResolveContext<'source>) -> unbox<'field> ctx.Source)
        )

    [<CustomOperation "subscribeAsync">]
    member __.SubscribeAsync (state: State<Field<'field, 'source>>, subscribe: ResolveEventStreamContext<'source> -> Task<IObservable<'field>>) =
        appendToState state (fun this ->
            this.AsyncSubscriber <- AsyncEventStreamResolver<_, _> (Func<_, _> subscribe)
            this.Resolver <- resolve (fun (ctx: ResolveContext<'source>) -> unbox<'field> ctx.Source)
        )

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
    member __.Query (state: State<Schema>, Query query) =
        appendToState state (fun this -> this.Query <- query)

    [<CustomOperation "mutation">]
    member __.Mutation (state: State<Schema>, Mutation mutation) =
        appendToState state (fun this -> this.Mutation <- mutation)

    [<CustomOperation "subscription">]
    member __.Subscription (state: State<Schema>, Subscription subscription) =
        appendToState state (fun this -> this.Subscription <- subscription)

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

    member __.auto<'source> (?Name, ?Description, ?Metadata) =
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
