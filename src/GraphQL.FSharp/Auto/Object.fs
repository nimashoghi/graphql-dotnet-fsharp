module GraphQL.FSharp.AutoImplementation.Object

open System
open GraphQL.Types

open GraphQL.FSharp.Inference
open GraphQL.FSharp.Registry
open GraphQL.FSharp.Utils

let private abstractClasses<'object> =
    let rec run (``type``: Type) = [|
        let baseType = ``type``.BaseType
        if baseType <> null && baseType <> typeof<obj> && baseType.IsAbstract then
            yield baseType
            yield! run baseType
    |]
    run typeof<'object>

let private interfaces<'object> = [|
    yield! typeof<'object>.GetInterfaces ()
    yield! abstractClasses<'object>
|]

let private addInterfaces (object: ObjectGraphType<'object>) =
    interfaces<'object>
    |> Array.map (fun ``interface`` -> inferObject ``interface`` |> Option.ofObj)
    |> Array.some
    |> Array.filter (fun ``interface`` -> ``interface`` :? IInterfaceGraphType)
    |> Array.map (fun ``interface`` -> ``interface`` :?> IInterfaceGraphType)
    |> Array.iter (fun ``interface`` -> object.AddResolvedInterface ``interface``)

    object.IsTypeOf <- fun x -> x :? 'object

let Object<'object> =
    if typeof<'object>.IsInterface || typeof<'object>.IsAbstract
    then invalidArg "object" "type parameter cannot be abstract"

    let object = ObjectGraphType<'object> ()
    setInfo typeof<'object> object

    addProperties inferObject object
    addMethods<'object> inferObject object

    getTypeAttribute<TypeAttribute> typeof<'object>
    |> Option.iter (updateType object >> ignore)

    addInterfaces object

    Object.register (typeof<'object>, object)

    object
