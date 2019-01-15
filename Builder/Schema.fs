[<AutoOpen>]
module GraphQL.FSharp.Builder.Schema

open GraphQL.Instrumentation
open GraphQL.Types
open GraphQL.FSharp.Util

let inline private set f (x: Schema) = f x; x

(*
            FieldMiddlewareBuilder middware = new FieldMiddlewareBuilder();
            middware.Use(next => ctx =>
            {
                // any code here
                return next(ctx);
            });
            middware.ApplyTo(this);
*)

// open System
// open System.Threading.Tasks
// open FSharp.Control.Tasks.V2

// let createMiddleware (f: (ResolveFieldContext -> obj Task) -> ResolveFieldContext -> obj Task) =
//     Func<FieldMiddlewareDelegate, FieldMiddlewareDelegate> (
//         fun next -> FieldMiddlewareDelegate (fun ctx -> f next.Invoke ctx))

// let addMiddleware (schema: Schema) =
//     let middleware = FieldMiddlewareBuilder ()
//     middleware
//         .Use (createMiddleware (fun next ctx ->
//             printfn "got here %s" ctx.ReturnType.Name
//             next ctx))
//         |> ignore
//     middleware.ApplyTo schema
//     schema

type SchemaBuilder() =
    member __.Yield _ = new Schema()

    [<CustomOperation "query">]
    member __.Query (schema, query: ObjectGraphType<obj>) =
        set (fun schema -> schema.Query <- query) schema

    [<CustomOperation "mutation">]
    member __.Mutation (schema, mutation: ObjectGraphType<obj>) =
        set (fun schema -> schema.Mutation <- mutation) schema

    [<CustomOperation "subscription">]
    member __.Subscription (schema, subscription: ObjectGraphType<obj>) =
        set (fun schema -> schema.Subscription <- subscription) schema

    [<CustomOperation "types">]
    member __.Types (schema, types: IGraphType list) =
        set (fun schema ->
            types
            |> List.toArray
            |> schema.RegisterTypes) schema

    member __.Run (schema: Schema) =
        // TODO: Does this need to be cleaned up?
        Object.typesToRegister
        |> List.toArray
        |> schema.RegisterTypes

        // addMiddleware schema
        schema

let schema = SchemaBuilder ()
