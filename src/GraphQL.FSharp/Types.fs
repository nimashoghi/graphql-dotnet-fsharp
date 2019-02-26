module GraphQL.FSharp.Types

open System
open System.Collections.Generic
open System.Runtime.InteropServices
open GraphQL.Types

open GraphQL.FSharp.Inference

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

type ResolveContext<'source> (context: ResolveFieldContext<'source>) =
    inherit ResolveFieldContext<'source> ()

    do
        base.Source <- context.Source
        base.FieldName <- context.FieldName
        base.FieldAst <- context.FieldAst
        base.FieldDefinition <- context.FieldDefinition
        base.ReturnType <- context.ReturnType
        base.ParentType <- context.ParentType
        base.Arguments <- context.Arguments
        base.Schema <- context.Schema
        base.Document <- context.Document
        base.Fragments <- context.Fragments
        base.RootValue <- context.RootValue
        base.UserContext <- context.UserContext
        base.Operation <- context.Operation
        base.Variables <- context.Variables
        base.CancellationToken <- context.CancellationToken
        base.Metrics <- context.Metrics
        base.Errors <- context.Errors
        base.SubFields <- context.SubFields

    member this.GetArgument<'TType> (name, ?defaultValue: 'TType) =
        let defaultValue = defaultArg defaultValue Unchecked.defaultof<'TType>
        this.GetArgument (typeof<'TType>, name, box defaultValue)

    member __.GetArgument (argumentType: Type, name: string, [<Optional; DefaultParameterValue (null: obj)>] defaultValue: obj) =
        try base.GetArgument (argumentType, name, defaultValue)
        with :? InvalidOperationException -> null

module Option =
    let mapToObj f x =
        x
        |> Option.map f
        |> Option.toObj


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

type Query = Object<obj>
type Mutation = Object<obj>
type Subscription = Object<obj>

type Schema = GraphQL.Types.Schema
