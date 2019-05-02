module GraphQL.FSharp.UnitTests.Generators

open System
open System.Text.RegularExpressions
open NUnit.Framework
open FsCheck
open FsCheck.Arb
open GraphQL.FSharp.Types
open GraphQL.Types

type ValidNameString =
    | ValidNameString of string

    member this.Get = match this with ValidNameString str -> str
    override this.ToString () = this.Get

let basicTypes: IGraphType list = [
    BooleanGraph
    DateGraph
    DateTimeGraph
    DateTimeOffsetGraph
    DecimalGraph
    FloatGraph
    DoubleGraph
    GuidGraph
    IntGraph
    StringGraph
    TimeSpanMillisecondsGraph
    TimeSpanSecondsGraph
    UriGraph
]

let nonNullGraphTypeGenerator =
    gen {
        return! Gen.elements [
            yield!
                basicTypes
                |> List.collect (
                    fun ``type`` ->
                        [
                            NonNullGraphType ``type``
                            NonNullGraphType (ListGraph ``type``)
                            NonNullGraphType (ListGraph (NonNullGraphType ``type``))
                        ]: NonNullGraphType list
                )
        ]
    }

let graphTypeGenerator =
    gen {
        return! Gen.elements [
            yield! basicTypes
            yield!
                basicTypes
                |> List.collect (
                    fun ``type`` ->
                        [
                            NonNullGraphType ``type``
                            ListGraph ``type``
                            NonNullGraphType (ListGraph ``type``)
                            ListGraph (NonNullGraphType ``type``)
                            NonNullGraphType (ListGraph (NonNullGraphType ``type``))
                        ]: IGraphType list
                )
        ]
    }

let fieldGenerator =
    gen {
        let! ``type`` = graphTypeGenerator
        let! (ValidNameString name) = Arb.generate<ValidNameString>
        return
            Field<obj, obj, obj> (
                Name = name,
                ResolvedType = ``type``
            )
    }

let argumentGenerator =
    gen {
        let! ``type`` = graphTypeGenerator
        let! (ValidNameString name) = Arb.generate<ValidNameString>
        return
            Argument<obj> (
                Name = name,
                ResolvedType = ``type``
            )
    }

type Generators =
    static member GraphType () = Arb.fromGen graphTypeGenerator
    static member NonNullGraphType () = Arb.fromGen nonNullGraphTypeGenerator
    static member Argument () = Arb.fromGen argumentGenerator
    static member Field () = Arb.fromGen fieldGenerator
    static member ValidNameString () =
        Default.String()
        |> filter (
            fun s ->
                not (String.IsNullOrEmpty s)
                && not (String.exists ((=) '\000') s)
                && not (s.StartsWith "__")
                && Regex.IsMatch (s, @"^[_a-zA-Z][_a-zA-Z0-9]*$")
        )
        |> convert ValidNameString string

[<SetUpFixture>]
type TestSetup () =
    [<OneTimeSetUp>]
    member __.Setup () =
        Arb.register<Generators> ()
        |> ignore
