[<AutoOpen>]
module GraphQL.FSharp.Model

open System
open System.Collections.Generic
open Apollo
open FSharp.Control.Reactive
open GraphQL.Types

type Validator<'x, 'y> = 'x -> 'y obs

type ValidatedArgument(t: Type, field: FieldType) =
    inherit QueryArgument(t)

    /// **Description**
    ///   * The field that the current argument is in.
    member val Field = field


    /// **Description**
    ///   * The validator for the current argument.
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
    /// **Description**
    ///   * Gets the overridden value of the current argument.
    member val Value: obj obs option = None with get, set

type FieldInfo() =
    /// **Description**
    ///   * The arguments of this field.
    member val Arguments = Dictionary<string, ArgumentInfo>()

/// **Description**
///   * Gets a specific argument from the argument dictionary.
let internal (|Argument|) name (x: Dictionary<string, ArgumentInfo>) =
    match x.TryGetValue name with
    | true, value -> Argument value
    | false, _ ->
        let argInfo = ArgumentInfo()
        x.Add (name, argInfo)
        Argument argInfo

/// **Description**
///   * Gets a specific field from the field dictionary.
let internal (|Field|) name (x: Dictionary<string, FieldInfo>) =
    match x.TryGetValue name with
    | true, value -> Field value
    | false, _ ->
        let fieldInfo = FieldInfo()
        x.Add(name, fieldInfo)
        Field fieldInfo

type UserContext() =
    /// **Description**
    ///   * Gets the value of the field with name `fieldName` and argument with name `argName`.
    member this.GetArgumentValue fieldName argName =
        let (Field fieldName field) = this.Fields
        let (Argument argName arg) = field.Arguments
        arg.Value

    /// **Description**
    ///   * Sets the value of the field with name `fieldName` and argument with name `argName`.
    member this.SetArgumentValue fieldName argName value =
        let (Field fieldName field) = this.Fields
        let (Argument argName arg) = field.Arguments
        arg.Value <- Some value


    /// **Description**
    ///   * Gets all the overridden fields.
    member val Fields = Dictionary<string, FieldInfo>()

let internal (|UserContext|_|) (ctx: obj) =
    match ctx with
    | :? UserContext as ctx -> Some (UserContext ctx)
    | _ -> None
