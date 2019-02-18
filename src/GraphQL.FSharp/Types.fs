module GraphQL.FSharp.Types

open System
open System.Collections.Generic
open System.Runtime.InteropServices
open GraphQL.Types

open GraphQL.FSharp.Inference

let invalidGraphType =
    {
        new GraphType () with
            override __.Equals _ = false
    }
    :> IGraphType

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

type Argument (?``type``) =
    inherit QueryArgument (Option.defaultValue invalidGraphType ``type``)

    member val Metadata: IDictionary<string, obj> = upcast Dictionary () with get, set

type Argument<'t> (?``type``) =
    inherit Argument (``type`` = (Option.defaultValue (createReference typeof<'t>) ``type``))

type Field (?``type``) =
    inherit EventStreamFieldType (ResolvedType = Option.toObj ``type``)

type Field<'source> (?``type``) =
    inherit Field (?``type`` = ``type``)

type Field<'field, 'source> (?``type``) =
    inherit Field<'source> (?``type`` = ``type``)

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
