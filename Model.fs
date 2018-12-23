[<AutoOpen>]
module GraphQL.FSharp.Model

open System.Collections.Generic
open Apollo
open FSharp.Control.Reactive
open GraphQL.Types

type Validator<'x, 'y> = 'x -> 'y obs

type ValidatedArgument(t: IGraphType, field: FieldType) =
    inherit QueryArgument(t)

    member val Field = field
    member val Validator: Validator<obj, obj> option = None with get, set

    /// **Description**
    ///   * Sets the validator for this argument.
    member this.SetValidator (f: Validator<'x, 'y>) =
        this.Validator <- Some (fun x ->
            f (x :?> 'x)
            |> Observable.map box)

let internal (|ValidatedArg|_|) (queryArgument: QueryArgument) =
    match queryArgument with
    | :? ValidatedArgument as arg -> Some (ValidatedArg arg)
    | _ -> None

type ArgumentInfo() =
    member val Value: obj obs option = None with get, set

let internal (|Argument|) name (x: Dictionary<string, ArgumentInfo>) =
    match x.TryGetValue name with
    | true, value -> Argument value
    | false, _ ->
        let argInfo = ArgumentInfo()
        x.Add (name, argInfo)
        Argument argInfo

type FieldInfo() =
    member val Arguments = Dictionary<string, ArgumentInfo>()

let internal (|Field|) name (x: Dictionary<string, FieldInfo>) =
    match x.TryGetValue name with
    | true, value -> Field value
    | false, _ ->
        let fieldInfo = FieldInfo()
        x.Add(name, fieldInfo)
        Field fieldInfo

type UserContext() =
    member this.GetArgumentValue name =
        let (Field name field) = this.Fields
        let (Argument name arg) = field.Arguments
        arg.Value

    member this.SetArgumentValue fieldName argName value =
        let (Field fieldName field) = this.Fields
        let (Argument argName arg) = field.Arguments
        arg.Value <- Some value

    member val Fields = Dictionary<string, FieldInfo>()

let internal (|UserContext|_|) (ctx: obj) =
    match ctx with
    | :? UserContext as ctx -> Some (UserContext ctx)
    | _ -> None
