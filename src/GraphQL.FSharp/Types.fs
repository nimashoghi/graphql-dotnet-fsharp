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
        | Query -> GraphQL.Types.DirectiveLocation.Query
        | Mutation -> GraphQL.Types.DirectiveLocation.Mutation
        | Subscription -> GraphQL.Types.DirectiveLocation.Subscription
        | Field -> GraphQL.Types.DirectiveLocation.Field
        | FragmentDefinition -> GraphQL.Types.DirectiveLocation.FragmentDefinition
        | FragmentSpread -> GraphQL.Types.DirectiveLocation.FragmentSpread
        | InlineFragment -> GraphQL.Types.DirectiveLocation.InlineFragment
        | Schema -> GraphQL.Types.DirectiveLocation.Schema
        | Scalar -> GraphQL.Types.DirectiveLocation.Scalar
        | Object -> GraphQL.Types.DirectiveLocation.Object
        | FieldDefinition -> GraphQL.Types.DirectiveLocation.FieldDefinition
        | ArgumentDefinition -> GraphQL.Types.DirectiveLocation.ArgumentDefinition
        | Interface -> GraphQL.Types.DirectiveLocation.Interface
        | Union -> GraphQL.Types.DirectiveLocation.Union
        | Enum -> GraphQL.Types.DirectiveLocation.Enum
        | EnumValue -> GraphQL.Types.DirectiveLocation.EnumValue
        | InputObject -> GraphQL.Types.DirectiveLocation.InputObject
        | InputFieldDefinition -> GraphQL.Types.DirectiveLocation.InputFieldDefinition

let invalidGraphType =
    {
        new GraphType () with
            override __.Equals _ = false
    }
    :> IGraphType

type TypedQueryArgument<'source> (?``type``) =
    inherit QueryArgument (
        ``type``
        |> Option.defaultValue invalidGraphType
    )

type TypedQueryArgument (?``type``) =
    inherit TypedQueryArgument<obj> (?``type`` = ``type``)

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
