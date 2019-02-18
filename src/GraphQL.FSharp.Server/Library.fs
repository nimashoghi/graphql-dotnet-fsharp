module GraphQL.FSharp.Server

open GraphQL.Conversion
open GraphQL.FSharp
open GraphQL.FSharp.Types
open GraphQL.Types
open GraphQL.Server
open GraphQL.Server.Authorization.AspNetCore
open GraphQL.Server.Internal
open Microsoft.Extensions.DependencyInjection

type FieldBuilder<'field, 'source> with
    [<CustomOperation "authorize">]
    member __.Authorize (field: Field<'field, 'source>, policy) =
        field.AuthorizeWith policy
        field

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
