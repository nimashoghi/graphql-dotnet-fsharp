module GraphQL.FSharp.Types

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

let invalidGraphType =
    {
        new GraphType () with
            override __.Equals _ = false
    }
    :> IGraphType

type TypedQueryArgument<'t> (?``type``) =
    inherit QueryArgument (
        ``type``
        |> Option.defaultValue invalidGraphType
    )

type TypedFieldType<'source> () =
    inherit EventStreamFieldType ()

type EnumerationGraphTypeEx<'t> () =
    inherit EnumerationGraphType<'t> ()

    override __.ChangeEnumCase x = x

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
