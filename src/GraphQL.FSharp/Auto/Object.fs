module internal GraphQL.FSharp.AutoObject

open System
open GraphQL.Types

open GraphQL.FSharp.AutoBase
open GraphQL.FSharp.Inference
open GraphQL.FSharp.Registry
open GraphQL.FSharp.Utils

// TODO: Subscriptions

let abstractClasses<'object> =
    let rec run (``type``: Type) = [|
        let baseType = ``type``.BaseType
        if baseType <> null && baseType <> typeof<obj> && baseType.IsAbstract then
            yield baseType
            yield! run baseType
    |]
    run typeof<'object>

let interfaces<'object> = [|
    yield! typeof<'object>.GetInterfaces ()
    yield! abstractClasses<'object>
|]

let addInterfaces (object: ObjectGraphType<'object>) =
    interfaces<'object>
    |> Array.map (fun ``interface`` -> inferObject ``interface`` |> Option.ofObj)
    |> Array.some
    |> Array.filter (fun ``interface`` -> ``interface`` :? IInterfaceGraphType)
    |> Array.map (fun ``interface`` -> ``interface`` :?> IInterfaceGraphType)
    |> Array.iter (fun ``interface`` -> object.AddResolvedInterface ``interface``)

    object.IsTypeOf <- fun x -> x :? 'object

    object

let Object<'object> =
    if typeof<'object>.IsInterface || typeof<'object>.IsAbstract
    then invalidArg "'object" "Type parameter cannot be abstract"

    ObjectGraphType<'object> ()
    |> setInfo typeof<'object>
    |> addProperties inferObjectNull
    |> addMethods inferObjectNull
    |> updateType typeof<'object>.TypeAttributes
    |> addInterfaces
    |> Object.register typeof<'object>
