module GraphQL.FSharp.Types

open System
open System.Collections.Generic
open System.Reflection
open GraphQL.Types

type DirectiveLocationUnion =
| Query
| Mutation
| Subscription
| Field
| FragmentDefinition
| FragmentSpread
| InlineFragment
| Schema
| Scalar
| Object
| FieldDefinition
| ArgumentDefinition
| Interface
| Union
| Enum
| EnumValue
| InputObject
| InputFieldDefinition
    member this.GraphQLDirectiveLocation =
        match this with
        | Query -> DirectiveLocation.Query
        | Mutation -> DirectiveLocation.Mutation
        | Subscription -> DirectiveLocation.Subscription
        | Field -> DirectiveLocation.Field
        | FragmentDefinition -> DirectiveLocation.FragmentDefinition
        | FragmentSpread -> DirectiveLocation.FragmentSpread
        | InlineFragment -> DirectiveLocation.InlineFragment
        | Schema -> DirectiveLocation.Schema
        | Scalar -> DirectiveLocation.Scalar
        | Object -> DirectiveLocation.Object
        | FieldDefinition -> DirectiveLocation.FieldDefinition
        | ArgumentDefinition -> DirectiveLocation.ArgumentDefinition
        | Interface -> DirectiveLocation.Interface
        | Union -> DirectiveLocation.Union
        | Enum -> DirectiveLocation.Enum
        | EnumValue -> DirectiveLocation.EnumValue
        | InputObject -> DirectiveLocation.InputObject
        | InputFieldDefinition -> DirectiveLocation.InputFieldDefinition

    static member OfGraphQLDirectiveLocation (location: DirectiveLocation) =
        match location with
        | DirectiveLocation.Query -> Query
        | DirectiveLocation.Mutation -> Mutation
        | DirectiveLocation.Subscription -> Subscription
        | DirectiveLocation.Field -> Field
        | DirectiveLocation.FragmentDefinition -> FragmentDefinition
        | DirectiveLocation.FragmentSpread -> FragmentSpread
        | DirectiveLocation.InlineFragment -> InlineFragment
        | DirectiveLocation.Schema -> Schema
        | DirectiveLocation.Scalar -> Scalar
        | DirectiveLocation.Object -> Object
        | DirectiveLocation.FieldDefinition -> FieldDefinition
        | DirectiveLocation.ArgumentDefinition -> ArgumentDefinition
        | DirectiveLocation.Interface -> Interface
        | DirectiveLocation.Union -> Union
        | DirectiveLocation.Enum -> Enum
        | DirectiveLocation.EnumValue -> EnumValue
        | DirectiveLocation.InputObject -> InputObject
        | DirectiveLocation.InputFieldDefinition -> InputFieldDefinition
        | _ -> failwith "Invalid location"

let invalidGraphType =
    {
        new GraphType () with
            override __.Equals _ = false
    }
    :> IGraphType

type IProcessable<'t> =
    abstract member Process: ('t -> 't) -> 't

type IHasAttributes =
    abstract member Attributes: Attribute list with get, set

type TypedQueryArgument<'t> (?``type``) =
    inherit QueryArgument (
        ``type``
        |> Option.defaultValue invalidGraphType
    )

    member val Metadata: IDictionary<string, obj> = upcast Dictionary () with get, set

    interface IHasAttributes with
        member val Attributes = [] with get, set

type TypedFieldType<'source> () =
    inherit EventStreamFieldType ()

    interface IHasAttributes with
        member val Attributes = [] with get, set

    interface IProcessable<TypedFieldType<'source>> with
        member this.Process f = f this

type EnumerationGraphTypeEx<'t> () =
    inherit EnumerationGraphType<'t> ()

    override __.ChangeEnumCase x = x

[<Literal>]
let DirectiveLocationsFieldName = "_directiveLocations"

let internal getDirectiveLocations (x: DirectiveGraphType) =
    box x
    |> typeof<DirectiveGraphType>
        .GetField(DirectiveLocationsFieldName, BindingFlags.NonPublic ||| BindingFlags.Instance)
        .GetValue
    :?> List<DirectiveLocation>

let internal locationToSeq x =
    x
    |> List.map (fun (location: DirectiveLocationUnion) -> location.GraphQLDirectiveLocation)
    |> List.toSeq

type DirectiveGraphTypeEx (?name, ?locations) =
    inherit DirectiveGraphType (
        name |> Option.defaultValue "",
        locations |> Option.defaultValue [] |> locationToSeq
    )

    member val Metadata: IDictionary<string, obj> = upcast Dictionary () with get, set

    member this.PossibleLocations
        with get () =
            this.Locations
            |> Seq.toList
            |> List.map DirectiveLocationUnion.OfGraphQLDirectiveLocation
        and set value =
            let list = getDirectiveLocations this
            list.Clear ()

            value
            |> locationToSeq
            |> list.AddRange


type UnionGraphType<'t> () =
    inherit UnionGraphType ()

type UnitGraphType () as this =
    inherit ScalarGraphType ()

    do this.Name <- "Unit"

    override __.Serialize _ = box "{}"
    override __.ParseValue _ = box ()
    override __.ParseLiteral _ = box ()

type Query = ObjectGraphType<obj>
type Mutation = ObjectGraphType<obj>
type Subscription = ObjectGraphType<obj>

module Instances =
    let BooleanGraph = BooleanGraphType ()
    let DateGraph = DateGraphType ()
    let DateTimeGraph = DateTimeGraphType ()
    let DateTimeOffsetGraph = DateTimeOffsetGraphType ()
    let DecimalGraph = DecimalGraphType ()
    let FloatGraph = FloatGraphType ()
    let IdGraph = IdGraphType ()
    let IntGraph = IntGraphType ()
    let StringGraph = StringGraphType ()
    let TimeSpanMillisecondsGraph = TimeSpanMillisecondsGraphType ()
    let TimeSpanSecondsGraph = TimeSpanSecondsGraphType ()
    let UriGraph = UriGraphType ()
    let NonNullGraph (x: #IGraphType) = NonNullGraphType (x :> IGraphType)
    let ListGraph (x: #IGraphType) = ListGraphType (x :> IGraphType)
