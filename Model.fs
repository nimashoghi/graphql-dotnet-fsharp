[<AutoOpen>]
module GraphQL.FSharp.Model

open System
open System.Collections.Generic
open Apollo
open FSharp.Control.Reactive
open GraphQL.Types

type ValueType<'t> =
| Optional
| Mandatory
| DefaultValue of 't

type Validator<'x, 'y> = 'x -> 'y obs

type ValidatedArgument(t: IGraphType, field: FieldType) =
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

// TODO: children of ObjectGraphType will not be caught by this
let internal getTypeParam (typeDef: Type) (x: IGraphType) =
    if not typeDef.IsGenericTypeDefinition then failwith "typeDef must be a generic type definition!"
    let xType = x.GetType()
    let xTypeDef = xType.GetGenericTypeDefinition()
    if typeDef = xTypeDef
    then Some (xType.GetGenericArguments().[0])
    else None

let internal (|Input|_|) (x: IGraphType) = getTypeParam typedefof<InputObjectGraphType<_>> x
let internal (|Object|_|) (x: IGraphType) = getTypeParam typedefof<ObjectGraphType<_>> x

type SchemaInfo(resolver) =
    inherit Schema(dependencyResolver = resolver)

    let objects = Dictionary<Type, IGraphType> ()

    member this.WithTypes (types: #seq<IGraphType>) =
        types
        |> Seq.toArray
        |> this.RegisterTypes

        for ``type`` in types do
            match ``type`` with
            | Object object | Input object -> objects.[object] <- ``type``
            | _ -> () // TODO: Warn/error?

    member __.GetObject ``type`` nullable =
        let handleNullable x = if not nullable then NonNullGraphType x :> IGraphType else x

        match objects.TryGetValue ``type`` with
        | true, value -> Some (handleNullable value)
        | _ -> None

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
