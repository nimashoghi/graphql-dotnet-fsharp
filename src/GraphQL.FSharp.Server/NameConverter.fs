[<AutoOpen>]
module GraphQL.FSharp.Server.NameConverter

open GraphQL.Conversion
open GraphQL.Server
open GraphQL.Server.Internal
open GraphQL.Types
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

type IGraphQLBuilder with
    member this.AddDefaultFieldNameConverter () =
        this.Services.AddTransient (
            serviceType = typedefof<IGraphQLExecuter<_>>,
            implementationType = typedefof<GraphQLExecuter<_>>
        )
        |> ignore

        this
