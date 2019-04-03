module GraphQL.FSharp.UnitTests.Builder.Operations

open System
open System.Threading.Tasks
open System.Collections.Generic
open NUnit.Framework
open FsCheck
open FsCheck.NUnit
open Swensen.Unquote
open FSharp.Utils
open FSharp.Utils.Tasks
open GraphQL
open GraphQL.FSharp.Builder
open GraphQL.FSharp.Types
open GraphQL.Types

open GraphQL.FSharp.UnitTests.Generators

module ``active patterns`` =
    let f (i: int) = i
    let operation =
        {
            new IOperation<int> with
                member __.Id = ""
                member __.Invoke i = f i
                member __.Priority = Priority 0
        }

    [<Test>]
    let ``operation active pattern`` () =
        let (Operation operation) = operation
        for i in 0 .. 100 do
            operation i =! f i

    [<Test>]
    let ``priority active pattern`` () =
        let (Priority priority) = operation
        priority =! 0

type MyType = {
    mutable Name: string
}

[<Property>]
let ``name`` (newName: string) =
    let (Operation f) = name newName
    f {Name = "hello"} =! {Name = newName}

type MyTypeEx = {
    mutable DeprecationReason: string
}

[<Property>]
let ``deprecationReason`` (newDeprecationReason: string) =
    let (Operation f) = deprecate newDeprecationReason
    f {DeprecationReason = "hello"} =! {DeprecationReason = newDeprecationReason}

module ``defaultValue`` =
    [<Property>]
    let ``field`` (value: obj) (``type``: NonNullGraphType) =
        let (Operation f) = defaultValue value
        let field =
            Field<_, _, _> (
                ResolvedType = ``type``
            )
            |> f
        field.DefaultValue =! box value
        field.ResolvedType =! ``type``.ResolvedType

    [<Property>]
    let ``argument`` (value: obj) (``type``: NonNullGraphType) =
        let (Operation f) = defaultValue value
        let argument =
            Argument<_> (
                ResolvedType = ``type``
            )
            |> f
        argument.DefaultValue =! box value
        argument.ResolvedType =! ``type``.ResolvedType

type MetadataTester () =
    member val Metadata: IDictionary<string, obj> = null with get, set

[<Property>]
let ``metadata without existing`` (value: (NonEmptyString * obj) list) =
    let value =
        value
        |> List.map (fun (NonEmptyString key, value) -> key, value)
    let graph =
        MetadataTester ()
        |> (|Operation|) (metadata value)
    let value = Dictionary.ofList value
    graph.Metadata.Count =! Seq.length value
    value
    |> Seq.iter (fun (KeyValue (key, value)) -> graph.Metadata.[key] =! value)

[<Property>]
let ``metadata with existing`` (oldValue: (NonEmptyString * obj) list) (value: (NonEmptyString * obj) list) =
    let oldValue =
        oldValue
        |> List.map (fun (NonEmptyString key, value) -> key, value)
    let value =
        value
        |> List.map (fun (NonEmptyString key, value) -> key, value)
    let graph =
        MetadataTester (
            Metadata = Dictionary.ofList oldValue
        )
        |> (|Operation|) (metadata value)
    let oldValue = Dictionary.ofList oldValue
    let value = Dictionary.ofList value
    let dictionary = Dictionary.merge oldValue value
    graph.Metadata.Count =! Seq.length dictionary
    dictionary
    |> Seq.iter (fun (KeyValue (key, value)) -> graph.Metadata.[key] =! value)

module ``Documentation`` =
    type DescriptionType () =
        member val Description: string = null with get, set

    [<Property>]
    let ``description`` (value: string) =
        let target =
            DescriptionType ()
            |> (|Operation|) (description value)
        target.Description =! value

    [<Property>]
    let ``Documentation.arguments`` (value: IDictionary<ValidNameString, ValidNameString>) =
        let value =
            value
            |> Seq.map (fun (KeyValue (ValidNameString name, ValidNameString desc)) -> name, desc)
            |> Seq.toList

        let field = Field<_, _, _> ()
        value
        |> List.map (
            fun (name, _) ->
                Argument (
                    Name = name
                )
                :> QueryArgument
        )
        |> List.toArray
        |> QueryArguments
        |> field.set_Arguments

        field
        |> (|Operation|) (Documentation.arguments value)
        |> ignore

        for argument in field.Arguments do
            argument.Description =! (value |> List.find (fun (name, _) -> argument.Name = name) |> snd)

module ``Field`` =
    [<Property>]
    let ``fieldArguments`` (field: Field<obj, obj, obj>) (argumentList: Argument<obj> list) =
        let argumentList =
            argumentList
            |> List.groupBy (fun arg -> arg.Name)
            |> List.map (fun (_, arguments) -> List.head arguments :> Argument)

        let field =
            [arguments argumentList]
            |> reduce field

        for argument in argumentList do
            field.Arguments.Find argument.Name =! upcast argument

    [<Property>]
    let ``validate`` (input: string) =
        let validator (args: {|Name: string|}) =
            if String.IsNullOrWhiteSpace args.Name
            then Error ["Invalid Input"]
            else Ok {|args with Name = sprintf "%s-Modified" args.Name|}
        // let field =
        //     Field<obj, {|Name: string|}, obj> (
        //         ResolvedType = NonNullGraphType StringGraph,
        //         Resolver = AsyncResolver<obj, obj> (fun ctx -> vtask { return ctx.Arguments.["Name"] })
        //     )
        let field =
            field (NonNullGraphType StringGraph) [
                validate (fun args -> vtask { return validator args })
                resolve.endpoint (fun args -> vtask { return args.Name })
            ]

        field.ResolvedType =! upcast StringGraph

        // FIXME: Currently, our FieldContext wrapper does not forward changes back, so we have to send an empty error list.
        let ctx =
            ResolveFieldContext (
                Errors = ExecutionErrors ()
            )
        ctx.Arguments <- Dictionary.ofList ["Name", box input] :?> Dictionary<_, _>
        let result = (field.Resolver.Resolve ctx :?> obj Task).Result :?> string
        match validator {|Name = input|} with
        | Ok value ->
            result =! value.Name
        | Error errors ->
            test <@ isNull result @>
            test <@ not (isNull ctx.Errors) @>
            for error in errors do
                test
                    <@
                        ctx.Errors
                        |> Seq.exists (fun e -> e.Message = error)
                    @>

    // [<Property>]
    // let ``manual resolve`` (NonEmptyString expected) (errors: NonEmptyString list) =
    //     let errors = errors |> List.map (fun (NonEmptyString str) -> str)
    //     let field =
    //         [
    //             resolve.manual (
    //                 fun ctx ->
    //                     errors |> List.iter (fun error -> ctx.Errors.Add (ExecutionError error))
    //                     Task.FromResult expected
    //             )
    //         ]
    //         |> reduceWith Field<string, obj, obj>
    //     let ctx =
    //         ResolveFieldContext (
    //             Errors = ExecutionErrors ()
    //         )
    //     let result = (field.Resolver.Resolve ctx :?> obj Task).Result :?> string
    //     for error in errors do
    //         test
    //             <@
    //                 ctx.Errors
    //                 |> Seq.exists (fun e -> e.Message = error)
    //             @>

    //     result =! expected

    type PropertyType = {
        mutable Property: string
    }
    [<Property>]
    let ``property resolve`` (ValidNameString expected) =
        let field =
            [
                resolve.property (fun this -> vtask { return this.Property })
            ]
            |> reduceWith Field<string, obj, PropertyType>
        let ctx =
            ResolveContext<PropertyType> (
                Source = {Property = expected}
            )
        let result = (field.Resolver.Resolve ctx.AsObjectContext :?> obj Task).Result :?> string
        result =! expected
