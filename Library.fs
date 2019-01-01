[<AutoOpen>]
module GraphQL.FSharp.Main

open System
open FSharp.Control.Reactive
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
open Rx
open Rx.Builders

/// **Description**
///   * Details:
///     * Try to parse the user context as our custom `UserContext` type.
///     * Try to get custom argument value override from our new context.
///     * If this doesn't work, use `ctx.GetArgument` instead.
let getArg<'arg, 'source> name (ctx: ResolveFieldContext<'source>) =
    Option.orElse
        (ctx.GetArgument<'arg> name |> Observable.single |> Some)
        (maybe {
            let! userCtx = (|UserContext|_|) ctx.UserContext
            let! argument = userCtx.GetArgumentValue ctx.FieldName name
            return rx {
                for value in argument do
                    map (value :?> 'arg)
            }
        })
    |> Option.get

type ArgBuilder<'arg>() =
    member __.Get<'source> name = getArg<'arg, 'source> name

    /// **Description**
    ///   * Gets the value of the argument with the name `name`.
    member this.Item with get name = this.Get name

let arg<'arg> = ArgBuilder<'arg>()

let private makeValidator f =
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
