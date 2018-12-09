[<AutoOpen>]
module GraphQL.FSharp.Main

open System
open FSharp.Reflection
open GraphQL
open GraphQL.Types

let internal resolveObj (provider: IServiceProvider) x = provider.GetService x
let internal resolve<'t> provider = resolveObj provider typeof<'t> :?> 't

let internal make<'graph when 'graph :> IGraphType> (f: 'graph -> unit) =
    let graph =
        f.GetType ()
        |> FSharpType.GetFunctionElements
        |> fst
        |> Activator.CreateInstance
        :?> 'graph
    f graph
    graph

let schema (definitions: #seq<IServiceProvider -> #IGraphType -> unit>) provider =
    let schema = resolve<Schema> provider
    for definition in definitions do
        definition provider
        |> make
        |> schema.RegisterType

module Schema =
    let create (provider: IServiceProvider) =
        let dependencyResolver = {
            new IDependencyResolver with
                member __.Resolve<'t> () = provider.GetService typeof<'t> :?> 't
                member __.Resolve t = provider.GetService t
        }

        new Schema(dependencyResolver)

    let withQuery query (schema: ISchema) =
        schema.Query <- query
        schema

    let withMutation mutation (schema: ISchema) =
        schema.Mutation <- mutation
        schema

    let withSubscription subscription (schema: ISchema) =
        schema.Subscription <- subscription
        schema

    let register<'t when 't :> IGraphType> (schema: ISchema) =
        schema.RegisterType<'t> ()
        schema
