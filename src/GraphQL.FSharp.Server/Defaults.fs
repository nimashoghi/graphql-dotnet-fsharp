[<AutoOpen>]
module GraphQL.FSharp.Server.Defaults

open System.Collections.Generic
open System.Reactive.Linq
open FSharp.Utils.Tasks
open GraphQL
open GraphQL.Conversion
open GraphQL.Execution
open GraphQL.Language.AST
open GraphQL.Server
open GraphQL.Server.Internal
open GraphQL.Types
open GraphQL.Validation
open GraphQL.Validation.Complexity
open Microsoft.Extensions.DependencyInjection

type GraphQLExecuter<'t when 't :> ISchema> (schema, documentExecutor, options, listeners, validationRules) =
    inherit DefaultGraphQLExecuter<'t> (schema, documentExecutor, options, listeners, validationRules)

    override __.GetOptions (operationName, query, variables, context, token) =
        let options = base.GetOptions (operationName, query, variables, context, token)
        options.FieldNameConverter <-
            {
                new IFieldNameConverter with
                    member __.NameFor (name, _) = name
            }
        options

type GraphQLSubscriptionExecutionStrategy () =
    inherit SubscriptionExecutionStrategy ()

    override __.ResolveEventStreamAsync (ctx, node) =
        let obs = base.ResolveEventStreamAsync (ctx, node)
        task {
            let! obs = obs
            let set = HashSet []

            return obs
                .Select(
                    fun result ->
                        if isNull result.Errors
                        then result.Errors <- ExecutionErrors ()

                        ctx.Errors
                        |> Seq.filter set.Add
                        |> Seq.iter result.Errors.Add

                        result
                )
        }

type GraphQLDocumentExecuter (documentBuilder: IDocumentBuilder, documentValidator: IDocumentValidator, complexityAnalyzer: IComplexityAnalyzer) =
    inherit DocumentExecuter (documentBuilder, documentValidator, complexityAnalyzer)

    new () =
        GraphQLDocumentExecuter (
            GraphQLDocumentBuilder (),
            DocumentValidator (),
            ComplexityAnalyzer ()
        )

    override __.SelectExecutionStrategy ctx =
        match ctx.Operation.OperationType with
        | OperationType.Subscription -> upcast GraphQLSubscriptionExecutionStrategy ()
        | _ -> base.SelectExecutionStrategy ctx

type IGraphQLBuilder with
    member this.AddDocumentExecutor () =
        this.Services
            .AddSingleton<IDocumentExecuter, GraphQLDocumentExecuter> ()
        |> ignore

        this

    member this.AddDefaultFieldNameConverter () =
        this.Services.AddTransient (
            serviceType = typedefof<IGraphQLExecuter<_>>,
            implementationType = typedefof<GraphQLExecuter<_>>
        )
        |> ignore

        this

    member this.AddFSharp () =
        this
            .AddDocumentExecutor()
            .AddDefaultFieldNameConverter()
