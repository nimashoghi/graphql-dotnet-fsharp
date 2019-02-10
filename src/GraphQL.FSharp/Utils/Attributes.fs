module GraphQL.FSharp.Utils.Attributes

open System
open System.Reflection
open FSharp.Reflection

type Type with
    member this.TypeAttributes = this.GetCustomAttributes ()

type MethodInfo with
    member this.MethodAttributes = this.GetCustomAttributes ()

type PropertyInfo with
    member this.PropertyAttributes = this.GetCustomAttributes ()

type ParameterInfo with
    member this.ParameterAttributes = this.GetCustomAttributes ()

type UnionCaseInfo with
    member this.CaseAttributes =
        this.GetCustomAttributes ()
        |> Seq.filter (fun x -> x :? Attribute)
        |> Seq.map (fun x -> x :?> Attribute)

let tryGetAttribute<'attribute when 'attribute :> Attribute> (attributes: seq<Attribute>) =
    attributes
    |> Seq.tryFind (fun attribute -> attribute :? 'attribute)
    |> Option.map (fun attribute ->  attribute :?> 'attribute)

let hasAttribute<'attribute when 'attribute :> Attribute> attributes =
    tryGetAttribute<'attribute> attributes
    |> Option.isSome
