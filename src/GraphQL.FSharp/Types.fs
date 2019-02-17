module GraphQL.FSharp.Types

open System
open System.Collections.Generic
open GraphQL.Types

open GraphQL.FSharp.Inference

let invalidGraphType =
    {
        new GraphType () with
            override __.Equals _ = false
    }
    :> IGraphType

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
