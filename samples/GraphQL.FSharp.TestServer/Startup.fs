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
        task {
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
                    resolve.property (fun this -> task { return this.Name })
                ]
            ]
        ]

    let MyTypeGraph =
        object<MyType> [
            fields [
                field __ [
                    documentation [
                        description "Hello world"
                        arguments [
                            "IntegerParam" => "Some integer parameter!"
                        ]
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
                            -> Task.FromResult (
                                this.MethodWithParam
                                    args.IntegerParam
                                    args.ListParam
                                    args.OptionalParam
                                    args.ResultParam
                            )
                    )
                ]
                field __ [
                    documentation [
                        description "Hello world"
                    ]
                    resolve.method (fun this _ -> Task.FromResult (this.GetSomethingSync ()))
                ]
                field __ [
                    resolve.method (fun this _ -> this.GetSomethingAsync ())
                ]
            ]
        ]

    let MyUnionFirstGraph =
        object<MyUnionFirst> [
            fields [
                field __ [
                    resolve.property (fun this -> Task.FromResult this.Name)
                ]
            ]
        ]

    let MyUnionSecondGraph =
        object<MyUnionSecond> [
            fields [
                field __ [
                    resolve.property (fun this -> Task.FromResult this.Age)
                ]
            ]
        ]

    let MyUnionGraph =
        union<MyUnion> [
            name "MyUnion"
            cases [
                case FirstUnion MyUnionFirstGraph
                case SecondUnion MyUnionSecondGraph
            ]
        ]

    let MyEnumGraph =
        enum<MyEnum> [
            name "MyEnum"
            cases [
                case First []
                case Second []
                case Third []
            ]
        ]

    let MyEnumUnionGraph =
        enum<MyEnumUnion> [
            enumAuto
        ]

    let MyEnumEnumGraph =
        enum<MyEnumEnum> [
            enumAuto
        ]

    let MyAutoUnionGraph =
        union<MyAutoUnion> [
            unionAuto
        ]

    let Query =
        query [
            field __ [
                name "GetMyEnum"
                resolve.method (fun _ _ -> Task.FromResult MyEnum.Third)
            ]
            field __ [
                name "GetMyUnion"
                resolve.method (fun _ _ -> Task.FromResult (FirstUnion {Name = "hello"}))
            ]
            field MyTypeGraph [
                name "GetMyType"
                resolve.method (fun _ _ -> Task.FromResult (Ok (MyType ())))
            ]

            field __ [
                name "MyEnumUnion"
                resolve.endpoint (fun _ -> task { return MyEnumUnion.MyEnumUnionFirst })
            ]
            field __ [
                name "MyEnumEnum"
                resolve.endpoint (fun _ -> task { return MyEnumEnum.MyEnumEnumFirst })
            ]
            field __ [
                name "AutoUnionFirst"
                resolve.endpoint (fun _ -> task { return MyAutoUnion.MyAutoUnionFirstCase {|Name = "hello"|} })
            ]
            field __ [
                name "AutoUnionSecond"
                resolve.endpoint (fun _ -> task { return MyAutoUnion.MyAutoUnionSecondCase {|Age = 1|} })
            ]
            field __ [
                name "Validate"
                validate (
                    fun (args: {|Age: int; Height: float; Name: string; AsyncName: string|}) -> validation {
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
                resolve.method (fun _ args -> Task.FromResult (sprintf "%s_%s_%i_%.0f" args.Name args.AsyncName args.Age args.Height))
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
                subscribe.endpoint (
                    fun args ->
                        task {
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
                subscribe.endpoint (
                    fun args ->
                        task {
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
                subscribe.endpoint (
                    fun args ->
                        task {
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
                subscribe.endpointResult (
                    fun args ->
                        task {
                            if args.Name = "invalidName" then return Error ["myError"] else
                            return Ok <| Observable
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
                MyTypeGraph
                MySubscriptionWrapperGraph
                MyUnionGraph
                MyEnumGraph
                MyEnumUnionGraph
                MyEnumEnumGraph
                MyAutoUnionGraph
            ]
        ]

type Startup() =
    member __.ConfigureServices(services: IServiceCollection) =
        services
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
