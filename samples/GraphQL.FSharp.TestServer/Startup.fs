namespace GraphQL.FSharp.TestServer

open System.Reactive.Linq
open System.Threading.Tasks
open FSharp.Utils.Tasks
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.DependencyInjection
open GraphQL.Server
open GraphQL.Server.Ui.Playground
open GraphQL.FSharp.Builder
open GraphQL.FSharp.Server
open GraphQL.FSharp.Types
open Validation.Builder

module Model =
    type MySubscriptionWrapper = {
        Name: string
    }

    type MyType() =
        member __.GetSomethingSync () = "hello world"
        member __.GetSomethingAsync () = Task.FromResult "hello world async"
        member __.MethodWithParam i j k l = sprintf "%i + %A + %A + %A" i j k l

    [<CLIMutable>]
    type MyUnionFirst = {
        Name: string
    }

    [<CLIMutable>]
    type MyUnionSecond = {
        Age: int
    }

    type MyUnion =
    | FirstUnion of MyUnionFirst
    | SecondUnion of MyUnionSecond

    type NormalUnion =
    | First of First: int * Second: int
    | HelloWorld of Hello: float * World: System.Guid

    type MyEnum =
    | First
    | Second
    | Third

    type MyEnumUnion =
    | MyEnumUnionFirst
    | MyEnumUnionSecond
    | MyEnumUnionThird

    type MyEnumEnum =
    | MyEnumEnumFirst = 0
    | MyEnumEnumSecond = 1
    | MyEnumEnumThird = 2

    type MyAutoUnion =
    | MyAutoUnionFirstCase of {|Name: string|}
    | MyAutoUnionSecondCase of {|Age: int|}

    type AutoRecord = {
        Name: string
        Age: int
        Id: System.Guid
        MyAutoUnion: MyAutoUnion
        AnonymousRecordType:
            {|
                First: int
                Second: float
                Third: int list
            |}
    }

[<AutoOpen>]
module Validation =
    let validateAge (age: int) =
        if age = 21
        then Ok 21
        else Error ["Invalid Age"]

    let validateHeight (height: float) =
        if height = 180.
        then Ok 180.
        else Error ["Invalid Height"]

    let validateName (name: string) =
        if name = "Alice"
        then Ok "Alice"
        else Error ["Invalid Name"]

    let validateAsyncName (name: string) =
        vtask {
            if name = "AsyncAlice"
            then return Ok "AsyncAlice"
            else return Error ["Invalid Name"]
        }

module Schema =
    open Model

    let MySubscriptionWrapperGraph =
        object<MySubscriptionWrapper> [
            name "MySubscriptionWrapper"
            fields [
                field __ [
                    name "Name"
                    resolve.property (fun this -> vtask { return this.Name })
                ]
            ]
        ]

    let MyTypeGraph =
        object<MyType> [
            fields [
                field __ [
                    Documentation.description "Hello world"
                    Documentation.arguments [
                        "IntegerParam" => "Some integer parameter!"
                    ]
                    resolve.method (
                        fun
                            this
                            (args:
                            {|
                                IntegerParam: int
                                ListParam: int []
                                OptionalParam: int option
                                ResultParam: Result<int, string>
                            |})
                            -> vtask {
                                return
                                    this.MethodWithParam
                                        args.IntegerParam
                                        args.ListParam
                                        args.OptionalParam
                                        args.ResultParam
                            }
                    )
                ]
                field __ [
                    Documentation.description "Hello world"
                    resolve.method (fun this _ -> vtask { return this.GetSomethingSync () })
                ]
                field __ [
                    resolve.method (fun this _ -> vtask { return! this.GetSomethingAsync () })
                ]
            ]
        ]

    let MyUnionFirstGraph =
        object<MyUnionFirst> [
            fields [
                field __ [
                    resolve.property (fun this -> vtask { return this.Name })
                ]
            ]
        ]

    let MyUnionSecondGraph =
        object<MyUnionSecond> [
            fields [
                field __ [
                    resolve.property (fun this -> vtask { return this.Age })
                ]
            ]
        ]

    let MyUnionGraph =
        union<MyUnion> [
            name "MyUnion"
            Union.cases [
                Union.case FirstUnion MyUnionFirstGraph
                Union.case SecondUnion MyUnionSecondGraph
            ]
        ]

    let MyEnumGraph =
        enum<MyEnum> [
            name "MyEnum"
            Enum.cases [
                Enum.case First []
                Enum.case Second []
                Enum.case Third []
            ]
        ]

    let MyEnumUnionGraph =
        enum<MyEnumUnion> [
            Enum.auto []
        ]

    let MyEnumEnumGraph =
        enum<MyEnumEnum> [
            Enum.auto []
        ]

    let MyAutoUnionGraph =
        union<MyAutoUnion> [
            Union.auto []
        ]

    let NormalUnionGraph =
        union<NormalUnion> [
            Union.auto []
        ]

    let AutoRecordGraph =
        object<AutoRecord> [
            Object.auto []
        ]

    let Query =
        query [
            field __ [
                name "GetNormalUnion"
                resolve.method (fun _ _ -> vtask { return NormalUnion.HelloWorld (Hello = 1.5, World = System.Guid.NewGuid ()) })
            ]
            field __ [
                name "GetMyEnum"
                resolve.method (fun _ _ -> vtask { return MyEnum.Third })
            ]
            field __ [
                name "GetMyUnion"
                resolve.method (fun _ _ -> vtask { return (FirstUnion {Name = "hello"}) })
            ]
            field MyTypeGraph [
                name "GetMyType"
                resolve.method (fun _ _ -> vtask { return (Ok (MyType ())) })
            ]
            field __ [
                name "GetAutoRecordGraph"
                resolve.endpoint (
                    fun _ ->
                        vtask {
                            return {
                                Name = ""
                                Age = 12
                                Id = System.Guid.Empty
                                MyAutoUnion = MyAutoUnionFirstCase {|Name = "test"|}
                                AnonymousRecordType =
                                    {|
                                        First = 1
                                        Second = 1.
                                        Third = [1]
                                    |}
                            }
                        }
                )
            ]
            field __ [
                name "MyEnumUnion"
                resolve.endpoint (fun _ -> vtask { return MyEnumUnion.MyEnumUnionFirst })
            ]
            field __ [
                name "MyEnumEnum"
                resolve.endpoint (fun _ -> vtask { return MyEnumEnum.MyEnumEnumFirst })
            ]
            field __ [
                name "AutoUnionFirst"
                resolve.endpoint (fun _ -> vtask { return MyAutoUnion.MyAutoUnionFirstCase {|Name = "hello"|} })
            ]
            field __ [
                name "AutoUnionSecond"
                resolve.endpoint (fun _ -> vtask { return MyAutoUnion.MyAutoUnionSecondCase {|Age = 1|} })
            ]
            field __ [
                name "Validate"
                validate (
                    fun (args: {|Age: int; Height: float; Name: string; AsyncName: string; SomeOtherThing: {|Name: {|Name: string|}|}|}) -> validation {
                        validate age in validateAge args.Age
                        validate height in validateHeight args.Height
                        validate name in validateName args.Name
                        validate asyncName in validateAsyncName args.AsyncName
                        return
                            {|
                                args with
                                    Age = age
                                    Height = height
                                    Name = name
                                    AsyncName = asyncName
                            |}
                    }
                )
                resolve.method (fun _ args -> vtask { return sprintf "%A %s_%s_%i_%.0f" args.SomeOtherThing args.Name args.AsyncName args.Age args.Height })
            ]
        ]

    let validateName name =
        match name with
        | "Invalid" -> Error ["Invalid name!"]
        | _ -> Ok name

    let Subscription =
        subscription [
            field __ [
                name "Test"
                validate (
                    fun (args: {|Name: string|}) -> validation {
                        validate name in validateName args.Name
                        return
                            {|
                                args with
                                    Name = name
                            |}
                    }
                )
                subscribe (
                    fun args ->
                        vtask {
                            return Observable
                                .Return({Name = args.Name}: MySubscriptionWrapper)
                                .CombineLatest(Observable.Interval(System.TimeSpan.FromSeconds 1.), fun x y -> {Name = sprintf "%s %i" x.Name y}: MySubscriptionWrapper)
                        }
                )
            ]
            field __ [
                name "TestAnon"
                validate (
                    fun (args: {|Name: string|}) -> validation {
                        validate name in validateName args.Name
                        return
                            {|
                                args with
                                    Name = name
                            |}
                    }
                )
                subscribe (
                    fun args ->
                        vtask {
                            return Observable
                                .Return({|Name = args.Name|})
                                .CombineLatest(Observable.Interval(System.TimeSpan.FromSeconds 1.), fun x y -> {|Name = sprintf "%s %i" x.Name y|})
                        }
                )
            ]
            field __ [
                name "TestAnonResult"
                validate (
                    fun (args: {|Name: string|}) -> validation {
                        validate name in validateName args.Name
                        return
                            {|
                                args with
                                    Name = name
                            |}
                    }
                )
                subscribe (
                    fun args ->
                        vtask {
                            return Observable
                                .Return({|Name = args.Name|})
                                .CombineLatest(
                                    Observable.Interval(System.TimeSpan.FromSeconds 1.),
                                    fun x y ->
                                        if y = 1L
                                        then Error ["Some error"]
                                        else Ok {|Name = sprintf "%s %i" x.Name y|}
                                )
                        }
                )
            ]
            field __ [
                name "TestAnonResultWithFailure"
                validate (
                    fun (args: {|Name: string|}) -> validation {
                        validate name in validateName args.Name
                        return
                            {|
                                args with
                                    Name = name
                            |}
                    }
                )
                subscribe (
                    fun args ->
                        vtask {
                            if args.Name = "invalidName" then return Observable.Return (Error ["myError"]) else
                            return Observable
                                .Return({|Name = args.Name|})
                                .CombineLatest(
                                    Observable.Interval(System.TimeSpan.FromSeconds 1.),
                                    fun x y ->
                                        if y = 1L
                                        then Error ["Some error"]
                                        else Ok {|Name = sprintf "%s %i" x.Name y|}
                                )
                        }
                )
            ]
        ]

    let Schema =
        schema [
            Query
            Subscription
            types [
                AutoRecordGraph
                MyTypeGraph
                MySubscriptionWrapperGraph
                MyUnionGraph
                MyEnumGraph
                MyEnumUnionGraph
                MyEnumEnumGraph
                MyAutoUnionGraph
                NormalUnionGraph
            ]
        ]

type Startup() =
    member __.ConfigureServices(services: IServiceCollection) =
        services
            .AddHttpContextAccessor()
            .AddSingleton(Schema.Schema)
            .AddGraphQL(
                fun options ->
                    options.ExposeExceptions <- false
                    options.EnableMetrics <- true
            )
            .AddWebSockets()
            .AddDefaultFieldNameConverter()
            .AddDocumentExecutor()
            |> ignore
        ()

    member __.Configure(app: IApplicationBuilder, env: IHostingEnvironment) =
        if env.IsDevelopment() then
            app.UseDeveloperExceptionPage() |> ignore

        app.UseWebSockets () |> ignore

        app.UseGraphQL<Schema> "/graphql" |> ignore
        app.UseGraphQLWebSockets<Schema> "/graphql" |> ignore
        GraphQLPlaygroundOptions () |> app.UseGraphQLPlayground |> ignore
