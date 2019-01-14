[<AutoOpen>]
module GraphQL.FSharp.Register

open System
open System.Collections.Generic
open System.Reflection
open GraphQL.Server
open GraphQL.Types
open Microsoft.Extensions.DependencyInjection

open GraphQL.FSharp.Util

let getAdditionalInstances (schema: Schema) =
    schema
        .GetType()
        .GetProperty("_additionalInstances",
            BindingFlags.NonPublic ||| BindingFlags.Instance ||| BindingFlags.GetProperty)
        .GetValue schema
        :?> List<IGraphType>

let getAdditionalTypes (schema: Schema) = seq {
    yield!
        schema.AdditionalTypes
        |> Seq.map (fun ``type`` -> Activator.CreateInstance ``type`` :?> IGraphType)
    yield! getAdditionalInstances schema
}

let private checkGraphType<'t> ``type`` =
    let ``type`` = ``type``.GetType ()
    if not ``type``.IsGenericType then None else
    let genericTypeDef = ``type``.GetGenericTypeDefinition ()
    if genericTypeDef <> typedefof<'t> then None else
    Some ``type``.GenericTypeArguments.[0]

let (|ObjectGraphType|_|) = checkGraphType<ObjectGraphType<_>>
let (|InputObjectGraphType|_|) = checkGraphType<InputObjectGraphType<_>>

let (|RegistrableType|) (``type``: IGraphType) =
    match ``type`` with
    | ObjectGraphType source
    | InputObjectGraphType source -> Some (source, ``type``)
    | _ -> None

type IGraphQLBuilder with
    member this.AddFSharp<'schema when 'schema :> Schema> (schema: unit -> 'schema) =
        getAdditionalTypes schema
        |> Seq.map (|RegistrableType|)
        |> Seq.filter Option.isSome
        |> Seq.map Option.get
        |> Seq.iter (fun (sys, gql) -> register (sys => gql))

        this.Services.AddSingleton<Schema> schema |> ignore
        this
