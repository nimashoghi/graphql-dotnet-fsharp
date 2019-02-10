[<AutoOpen>]
module internal GraphQL.FSharp.AutoBase.Update

open System
open GraphQL.Types

open GraphQL.FSharp
open GraphQL.FSharp.Utils
open GraphQL.FSharp.Utils.Attributes

let update<'attribute, 't when 'attribute :> AttributeWithValue<'t>> f (attributes: seq<Attribute>) =
    attributes
    |> tryGetAttribute<'attribute>
    |> Option.map (fun attribute -> attribute.Value)
    |> Option.iter f

    attributes

let updateEnumValue attributes (x: EnumValueDefinition) =
    attributes
    |> update<NameAttribute, _> x.set_Name
    |> update<DescriptionAttribute, _> x.set_Description
    |> update<DeprecationReasonAttribute, _> x.set_DeprecationReason
    |> update<ValueAttribute, _> x.set_Value
    |> ignore

    x

let updateArgument attributes (x: QueryArgument) =
    attributes
    |> update<NameAttribute, _> x.set_Name
    |> update<DescriptionAttribute, _> x.set_Description
    |> update<DefaultValueAttribute, _> x.set_DefaultValue
    |> ignore

    x

let updateField attributes (x: EventStreamFieldType) =
    attributes
    |> update<NameAttribute, _> x.set_Name
    |> update<DescriptionAttribute, _> x.set_Description
    |> update<DeprecationReasonAttribute, _> x.set_DeprecationReason
    |> update<MetadataAttribute, _> x.set_Metadata
    |> update<TypeAttribute, _> x.set_Type
    |> update<DefaultValueAttribute, _> x.set_DefaultValue
    |> ignore

    x

let updateType attributes (x: #IGraphType)  =
    attributes
    |> update<NameAttribute, _> x.set_Name
    |> update<DescriptionAttribute, _> x.set_Description
    |> update<DeprecationReasonAttribute, _> x.set_DeprecationReason
    |> update<MetadataAttribute, _> (fun metadata ->
        for pair in metadata do
            x.Metadata.[pair.Key] <- pair.Value)
    |> ignore

    x
