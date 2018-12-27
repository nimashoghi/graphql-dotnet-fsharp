[<AutoOpen>]
module GraphQL.FSharp.Main

open System
open Apollo
open GraphQL
open GraphQL.Language.AST
open GraphQL.Server
open GraphQL.Server.Ui.GraphiQL
open GraphQL.Server.Ui.Playground
open GraphQL.Server.Ui.Voyager
open GraphQL.Types
open GraphQL.Validation
open Iris
open Iris.Option.Builders
open Microsoft.Extensions.DependencyInjection
open Microsoft.AspNetCore.Builder

type ArgBuilder<'t>() =
    /// **Description**
    ///   * Gets the value of the argument with the name `name`.
    member __.Item
        with get name =
            fun (fieldCtx: ResolveFieldContext<_>) ->
                match fieldCtx.UserContext with
                | :? UserContext as userCtx ->
                    match userCtx.GetArgumentValue fieldCtx.FieldName name with
                    | Some x ->
                        x
                        |> Observable.catch (fun exn ->
                            fieldCtx.Errors.Add (ExecutionError("Validation error", ``exception`` = exn))
                            Observable.empty)
                        |> Observable.flatMap (function
                            | :? 't as x -> Observable.unit x
                            // TODO: Failwithf should be properly reported
                            | _ -> failwithf "Could not cast arg to type %s" typeof<'t>.Name)
                    | None -> Observable.unit (fieldCtx.GetArgument<'t> name)
                | _ -> Observable.unit (fieldCtx.GetArgument<'t> name)

let arg<'t> = ArgBuilder<'t>()

let private makeValidator (f: ValidationContext -> EnterLeaveListener -> unit) =
    {
        new IValidationRule with
            member __.Validate ctx =
                let visitor = EnterLeaveListener(Action<EnterLeaveListener> (f ctx)) :> INodeVisitor
                {
                    new INodeVisitor with
                        member __.Enter node = visitor.Enter node
                        member __.Leave node = visitor.Leave node
                }
    }


let validator (_: IServiceProvider) = makeValidator (fun ctx listener ->
    match ctx.UserContext with
    | UserContext userCtx ->
        listener.Match<Argument>(fun argument ->
            match ctx.TypeInfo.GetArgument () with
            | ValidatedArg argDef ->
                maybeUnit {
                    let! validator = argDef.Validator
                    argument.Value.Value
                    |> validator
                    |> userCtx.SetArgumentValue argDef.Field.Name argDef.Name
                }
            | _ -> ()
        )
    | _ -> ()
)

type IServiceCollection with
    // TODO: Better naming
    // TODO: More configurability
    member this.AddGraphQLFS<'injection, 'schema when 'schema : not struct and 'schema :> Schema> (f: 'injection -> 'schema) =
        this
            .AddTransient<IValidationRule>(Func.from validator)
            .AddSingleton<Schema>(Func.from (fun (provider: IServiceProvider) -> f (provider.GetService typeof<'injection> :?> 'injection) :> Schema))
            .AddGraphQL(fun options ->
                options.EnableMetrics <- true
                options.ExposeExceptions <- true)
            .AddWebSockets()
            .AddDataLoader()
            .AddUserContextBuilder(fun _ -> UserContext())

type IApplicationBuilder with
    // TODO: Better naming
    // TODO: More configurability
    member this.UseGraphQLFS<'injection, 'schema when 'schema :> Schema> (_: 'injection -> 'schema) =
        this
            .UseWebSockets()
            .UseGraphQL<Schema>()
            .UseGraphQLWebSockets<Schema>()
            .UseGraphiQLServer(GraphiQLOptions())
            .UseGraphQLPlayground(GraphQLPlaygroundOptions())
            .UseGraphQLVoyager(GraphQLVoyagerOptions())
