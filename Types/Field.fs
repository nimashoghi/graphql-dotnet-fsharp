[<AutoOpen>]
module GraphQL.FSharp.Types.Field

open Iris.Option.Builders
open GraphQL.Types

type Define with
    static member Input
        (name: string,
         ``type``: IGraphType,
         ?``default``: 'value,
         ?description: string) =

        let argument = QueryArgument ``type``
        argument.Name <- name
        maybeUnit {
            let! description = description
            argument.Description <- description
        }
        maybeUnit {
            let! ``default`` = ``default``
            argument.DefaultValue <- box ``default``
        }
        argument

    static member Field
        (name: string,
         ?``type``: IGraphType,
         ?args: QueryArgument list,
         ?description: string,
         ?deprecationReason: string) =

        let field = FieldType () |> setBasicProps name description deprecationReason
        maybeUnit {
            let! ``type`` = ``type``
            field.ResolvedType <- ``type``
        }
        maybeUnit {
            let! args = args
            field.Arguments <- QueryArguments args
        }
        field
