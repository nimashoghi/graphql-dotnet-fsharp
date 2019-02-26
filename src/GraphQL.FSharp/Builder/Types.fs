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

type ArgumentBuilder<'t> (``type``) =
    inherit TypedEntityBuilder<Argument<'t>> ()

    member __.Yield (_: unit) =
        Argument<'t> (?``type`` = Option.ofObj ``type``)

type DirectiveBuilder () =
    inherit EntityBuilder<Directive> ()

    member __.Yield (_: unit) = Directive ()

    [<CustomOperation "arguments">]
    member __.Arguments (state: Directive, arguments) =
        setArguments arguments state

    [<CustomOperation "locations">]
    member __.Locations (state: Directive, locations) =
        state.PossibleLocations <- locations
        state

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

    member __.Yield (_: unit) = Enumeration ()

    [<CustomOperation "cases">]
    member __.Cases (state: Enumeration, values) =
        values
        |> List.iter state.AddValue

        state

type FieldBuilder<'field, 'source> (``type``, ?name) =
    inherit TypedFieldBuilder<Field<'field, 'source>> ()

    member __.Yield (_: unit) =
        Field<'field, 'source> (
            ?``type`` = Option.ofObj ``type``,
            Name = Option.toObj name
        )

    [<CustomOperation "arguments">]
    member __.Arguments (state: Field<'field, 'source>, arguments) =
        setArguments (QueryArguments (List.toSeq arguments)) state

    [<CustomOperation "prop">]
    member __.Property (state: Field<'field, 'source>, [<ReflectedDefinition true>] expr: Expr<'source -> 'field>) =
        Field.setField (|FieldName|_|) (withSource >> resolve) state expr
        |> Field.setType<'field, 'source>

    [<CustomOperation "propAsync">]
    member __.PropertyAsync (state: Field<'field, 'source>, [<ReflectedDefinition true>] expr: Expr<'source -> Task<'field>>) =
        Field.setField (|FieldName|_|) (withSource >> resolveAsync) state expr
        |> Field.setType<'field, 'source>

    [<CustomOperation "method">]
    member __.Method (state: Field<'field, 'source>, [<ReflectedDefinition true>] expr: Expr<'source -> 'arguments -> 'field>) =
        Field.setField (|MethodName|_|) Field.resolveMethod state expr
        |> Field.addArguments<'arguments, 'field, 'source>
        |> Field.setType<'field, 'source>

    [<CustomOperation "methodAsync">]
    member __.MethodAsync (state: Field<'field, 'source>, [<ReflectedDefinition true>] expr: Expr<'source -> 'arguments -> Task<'field>>) =
        Field.setField (|MethodName|_|) Field.resolveMethod state expr
        |> Field.addArguments<'arguments, 'field, 'source>
        |> Field.setType<'field, 'source>

    [<CustomOperation "resolve">]
    member __.Resolve (state: Field<'field, 'source>, resolver: ResolveContext<'source> -> 'arguments -> 'field) =
        state.Resolver <- Field.resolveCtxMethod resolver

        state
        |> Field.addArguments<'arguments, 'field, 'source>
        |> Field.setType<'field, 'source>

    [<CustomOperation "resolveAsync">]
    member __.ResolveAsync (state: Field<'field, 'source>, resolver: ResolveContext<'source> -> 'arguments -> Task<'field>) =
        state.Resolver <- Field.resolveCtxMethodAsync resolver

        state
        |> Field.addArguments<'arguments, 'field, 'source>
        |> Field.setType<'field, 'source>

    [<CustomOperation "subscribe">]
    member __.Subscribe (state: Field<'field, 'source>, subscribe: ResolveEventStreamContext<'source> -> IObservable<'field>) =
        state.Subscriber <- EventStreamResolver<_, _> (Func<_, _> subscribe)
        state.Resolver <- resolve (fun (ctx: ResolveContext<'source>) -> unbox<'field> ctx.Source)

        state
        |> Field.setType<'field, 'source>

    [<CustomOperation "subscribeAsync">]
    member __.SubscribeAsync (state: Field<'field, 'source>, subscribe: ResolveEventStreamContext<'source> -> Task<IObservable<'field>>) =
        state.AsyncSubscriber <- AsyncEventStreamResolver<_, _> (Func<_, _> subscribe)
        state.Resolver <- resolve (fun (ctx: ResolveContext<'source>) -> unbox<'field> ctx.Source)

        state
        |> Field.setType<'field, 'source>

type InputObjectBuilder<'source> () =
    inherit ComplexGraphTypeBuilder<InputObject<'source>, 'source> ()

    member __.Yield (_: unit) = InputObject<'source> (Name = typeof<'source>.Name)

type InterfaceBuilder<'source> () =
    inherit ComplexGraphTypeBuilder<Interface<'source>, 'source> ()

    member __.Yield (_: unit) = Interface<'source> (Name = typeof<'source>.Name)

type ObjectBuilder<'source> () =
    inherit ComplexGraphTypeBuilder<Object<'source>, 'source> ()

    member __.Yield (_: unit) = Object<'source> (Name = typeof<'source>.Name)

    [<CustomOperation "interfaces">]
    member __.Interfaces (state: Object<'source>, interfaces) =
        interfaces
        |> List.iter state.AddResolvedInterface

        state

type SchemaBuilder () =
    inherit ConfigureBuilder<Schema> ()

    member __.Yield (_: unit) =
        new Schema (
            FieldNameConverter =
                {
                    new IFieldNameConverter with
                        member __.NameFor (name, _) = name
                }
        )

    [<CustomOperation "query">]
    member __.Query (state: Schema, Query query) =
        state.Query <- query
        state

    [<CustomOperation "mutation">]
    member __.Mutation (state: Schema, Mutation mutation) =
        state.Mutation <- mutation
        state

    [<CustomOperation "subscription">]
    member __.Subscription (state: Schema, Subscription subscription) =
        state.Subscription <- subscription
        state

    [<CustomOperation "types">]
    member __.Types (state: Schema, types) =
        types
        |> Schema.handleInterfaces
        |> List.toArray
        |> state.RegisterTypes

        state

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

    member __.Yield (_: unit) = Union ()

    [<CustomOperation "cases">]
    member __.Cases (state: Union, cases) =
        state.PossibleTypes <- List.toSeq cases

        state
