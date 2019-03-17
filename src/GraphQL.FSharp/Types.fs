[<AutoOpen>]
module GraphQL.FSharp.Types

open System
open System.Collections.Generic
open System.Runtime.InteropServices
open System.Threading.Tasks
open GraphQL.Resolvers
open GraphQL.Types

type NullGraphType (?``type``) =
    inherit GraphType ()

    member val ResolvedType: IGraphType = Option.toObj ``type`` with get, set

[<AutoOpen>]
module TypePatterns =
    let internal getGraphType<'t when 't :> IGraphType> (``type``: IGraphType) =
        match ``type`` with
        | :? 't as ``type`` -> Some ``type``
        | _ -> None

    let (|List|_|) ``type`` = getGraphType<ListGraphType> ``type``
    let (|NonNull|_|) ``type`` = getGraphType<NonNullGraphType> ``type``
    let (|Null|_|) ``type`` = getGraphType<NullGraphType> ``type``

    let processNonNullity ``type`` =
        let rec run isNull (``type``: IGraphType) =
            match ``type`` with
            | null -> null
            | Null ``type`` -> run true ``type``.ResolvedType
            | List ``type`` -> ListGraphType (run false ``type``.ResolvedType) :> IGraphType
            | NonNull ``type`` -> ``type`` :> IGraphType
            | ``type`` when isNull -> ``type``
            | ``type`` -> NonNullGraphType ``type`` :> IGraphType
        run false ``type``

[<AutoOpen>]
module Instances =
    let internal invalidGraphType =
        {
            new GraphType () with
                override __.Equals _ = false
        }
        :> IGraphType

    let BooleanGraph = BooleanGraphType ()
    let DateGraph = DateGraphType ()
    let DateTimeGraph = DateTimeGraphType ()
    let DateTimeOffsetGraph = DateTimeOffsetGraphType ()
    let DecimalGraph = DecimalGraphType ()
    let FloatGraph = FloatGraphType ()
    let DoubleGraph = FloatGraphType ()
    let IdGraph = IdGraphType ()
    let GuidGraph = IdGraphType ()
    let IntGraph = IntGraphType ()
    let StringGraph = StringGraphType ()
    let TimeSpanMillisecondsGraph = TimeSpanMillisecondsGraphType ()
    let TimeSpanSecondsGraph = TimeSpanSecondsGraphType ()
    let UriGraph = UriGraphType ()
    let NullGraph (``type``: #IGraphType) = NullGraphType (``type`` :> IGraphType)
    let ListGraph (``type``: #IGraphType) = ListGraphType (processNonNullity ``type``)
    let __: IGraphType = null

type ResolveContext<'source> (?context: ResolveFieldContext<'source>) as this =
    inherit ResolveFieldContext<'source> ()

    do
        context |> Option.iter(
            fun context ->
                this.Source <- context.Source
                this.FieldName <- context.FieldName
                this.FieldAst <- context.FieldAst
                this.FieldDefinition <- context.FieldDefinition
                this.ReturnType <- context.ReturnType
                this.ParentType <- context.ParentType
                this.Arguments <- context.Arguments
                this.Schema <- context.Schema
                this.Document <- context.Document
                this.Fragments <- context.Fragments
                this.RootValue <- context.RootValue
                this.UserContext <- context.UserContext
                this.Operation <- context.Operation
                this.Variables <- context.Variables
                this.CancellationToken <- context.CancellationToken
                this.Metrics <- context.Metrics
                this.Errors <- context.Errors
                this.SubFields <- context.SubFields
        )

    member this.AsObjectContext =
        ResolveFieldContext (
            Source = this.Source,
            FieldName = this.FieldName,
            FieldAst = this.FieldAst,
            FieldDefinition = this.FieldDefinition,
            ReturnType = this.ReturnType,
            ParentType = this.ParentType,
            Arguments = this.Arguments,
            Schema = this.Schema,
            Document = this.Document,
            Fragments = this.Fragments,
            RootValue = this.RootValue,
            UserContext = this.UserContext,
            Operation = this.Operation,
            Variables = this.Variables,
            CancellationToken = this.CancellationToken,
            Metrics = this.Metrics,
            Errors = this.Errors,
            SubFields = this.SubFields
        )

    member this.GetArgument<'TType> (name, ?defaultValue: 'TType) =
        let defaultValue = defaultArg defaultValue Unchecked.defaultof<'TType>
        this.GetArgument (typeof<'TType>, name, box defaultValue)

    member __.GetArgument (argumentType: Type, name: string, [<Optional; DefaultParameterValue (null: obj)>] defaultValue: obj) =
        try base.GetArgument (argumentType, name, defaultValue)
        with :? InvalidOperationException -> null

let makeContext<'source> (ctx: ResolveFieldContext) =
    ResolveContext<'source>(ResolveFieldContext<'source> ctx)

type Resolver<'source, 'field> (f: ResolveContext<'source> -> obj) =
    member val Resolver = f

    interface IFieldResolver with
        member __.Resolve ctx = box (f (makeContext<'source> ctx))

type AsyncResolver<'source, 'field> (f: ResolveContext<'source> -> obj Task) =
    member val Resolver = f

    interface IFieldResolver with
        member __.Resolve ctx = box (f (makeContext<'source> ctx))

type Argument () =
    inherit QueryArgument (Instances.invalidGraphType)

    member val Metadata: IDictionary<string, obj> = upcast Dictionary () with get, set

    member __.GraphType
        with get () = base.ResolvedType
        and set value = base.ResolvedType <- processNonNullity value

type Argument<'t> () =
    inherit Argument ()

type Field () =
    inherit EventStreamFieldType ()

    member __.GraphType
        with get () = base.ResolvedType
        and set value = base.ResolvedType <- processNonNullity value

type Field<'source> () =
    inherit Field ()

type Field<'field, 'source> () =
    inherit Field<'source> ()

type Field<'arguments, 'field, 'source> () =
    inherit Field<'field, 'source> ()

type EnumerationValue () =
    inherit EnumValueDefinition ()

type EnumerationValue<'t> () =
    inherit EnumerationValue ()

type Enumeration () =
    inherit EnumerationGraphType ()

type Enumeration<'t> () =
    inherit Enumeration ()

type InputObject<'t> () =
    inherit InputObjectGraphType<'t> ()

type Interface<'t> () =
    inherit InterfaceGraphType<'t> ()

type Object<'t> () =
    inherit ObjectGraphType<'t> ()

type Directive () =
    inherit DirectiveGraphType ("", Seq.empty)

    member val Metadata: IDictionary<string, obj> = upcast Dictionary () with get, set

    member this.PossibleLocations
        with get () = this.Locations
        and set value =
            let list = this.Locations :?> List<DirectiveLocation>
            list.Clear ()
            list.AddRange value

type Union () =
    inherit UnionGraphType ()

type Union<'t> () =
    inherit Union ()

type Schema = GraphQL.Types.Schema
