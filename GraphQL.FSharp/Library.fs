module GraphQL.FSharp

open System
open FSharp.Injection
open FSharp.Interop.Dynamic
open FSharp.Reflection
open GraphQL.Types
open Iris.Option

let (|FunctionType|_|) (t: Type) =
    if not (FSharpType.IsFunction t) then
        None
    else
        let (arg, ret) = FSharpType.GetFunctionElements t
        Some (FunctionType (arg, ret))

let (|Array|) lst = Array (List.toArray lst)

let generateTypes t =
    let rec execute t =
        match t with
        | FunctionType (arg, ret) -> arg :: execute ret
        | t -> [t]
    let lst = execute t
    if List.length lst < 2 then
        None
    else
        // TODO: Look here
        match List.splitAt (List.length lst - 2) lst with
        | (args, [graphType; _]) -> Some (args, graphType)
        | _ -> None

let resolveObj (provider: IServiceProvider) x = provider.GetService x
let resolve<'t> provider = resolveObj provider typeof<'t> :?> 't

// let inject resolver fn = maybe {
//     let t = fn.GetType()
//     let! (Array args, ret) = generateTypes t
//     let resolved = Array.map (resolveObj resolver) args
//     if Array.forall (Option.ofObj >> Option.isSome) resolved then
//         return! None
//     else
//         return!
//             fn.GetType().GetMethods()
//             |> Array.tryFind (fun method -> method.Name = "Invoke" && method.GetParameters().Length = args.Length)
//             |> Option.map (fun method -> // TODO: look here
//                 let definitionFn = method.Invoke(fn, resolved)
//                 let graphType = Activator.CreateInstance ret
//                 definitionFn?Invoke graphType
//                 graphType :?> IGraphType)
// }
let inject f provider = inject provider f :> IGraphType

let schema provider (definitions: #seq<IServiceProvider -> #IGraphType>) =
    let schema = resolve<Schema> provider
    for definition in definitions do schema.RegisterType <| definition provider

[<AutoOpen>]
module Configure =
    open Microsoft.AspNetCore.Builder
    open Microsoft.Extensions.DependencyInjection

    let useGraphQL (app: IApplicationBuilder) = ()
    let addGraphQL (services: IServiceCollection) = ()

module Test =
    open System
    open System.Linq
    open GraphQL.Types
    open GraphQL.FSharp.Builder

    [<CLIMutable>]
    type ItemInput = {
        Id: Guid
        Name: string
        Count: int
    }

    [<AttributeUsage(AttributeTargets.Parameter)>]
    type InjectAttribute() = inherit Attribute()

    module Z =
        type internal Marker = interface end
        let x () = ()
    typeof<Z.Marker>.DeclaringType.GetMembers()

    open FSharp.Reflection

    let add (provider: IServiceProvider) =


    FSharpType.IsModule

(* object {
    name "myObject"
    description "My Description"
    field (f {
        get (fun input -> input.Id)
    })
    field (f {
        get (fun input -> input.Name)
    })
    field (f {
        get (fun input -> input.Count)
    })
} *)
