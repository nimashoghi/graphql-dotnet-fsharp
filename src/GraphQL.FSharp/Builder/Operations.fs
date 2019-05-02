[<AutoOpen>]
module GraphQL.FSharp.Builder.Operations

open System.Runtime.CompilerServices
open FSharp.Utils

type IOperation<'t> =
    abstract member Id: string
    abstract member Invoke: 't -> 't
    abstract member Priority: Priority

let (|Operation|) (operation: IOperation<_>) = operation.Invoke
let (|Priority|) (operation: IOperation<_>) =
    let (Priority priority) = operation.Priority
    priority
let (|Id|) (operation: IOperation<_>) = operation.Id

type FlattenOperation<'t> (parameters: IOperation<'t> list) =
    member val Parameters = parameters

    interface IOperation<'t> with
        member __.Id = "Flatten"
        member __.Invoke target =
            parameters
            |> List.sortBy (|Priority|)
            |> List.fold (fun target (Operation f) -> f target) target
        member __.Priority = Priority.Flatten

let flatten parameters = FlattenOperation parameters

let internal flattenOperations (parameters: IOperation<_> list) =
    parameters
    |> List.collect (
        fun operation -> [
            match operation with
            | :? FlattenOperation<_> as flattenOp -> yield! flattenOp.Parameters
            | operation -> yield operation
        ]
    )

let reduce initial parameters =
    parameters
    |> List.sortBy (|Priority|)
    |> List.fold (fun object (Operation operation) -> operation object) initial

let reduceWith initial parameters = reduce (initial ()) parameters

let operation id priority f =
    {
        new IOperation<_> with
            member __.Id = id
            member __.Invoke x = f x
            member __.Priority = priority
    }
let operationUnit id priority f = operation id priority (fun x -> f x; x)

let configure id f = operation id Priority.Default f
let configureUnit id f = operationUnit id Priority.Default f

type Operation private () =
    static member Create (priority, [<CallerMemberName>] ?name) = fun f -> operation (Option.get name) priority f
    static member CreateUnit (priority, [<CallerMemberName>] ?name) = fun f -> operationUnit (Option.get name) priority f

    static member Configure (f, [<CallerMemberName>] ?name) = configure (Option.get name) f
    static member ConfigureUnit (f, [<CallerMemberName>] ?name) = configureUnit (Option.get name) f
