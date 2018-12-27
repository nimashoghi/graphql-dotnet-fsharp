module GraphQL.FSharp.Builder.Argument

open GraphQL
open GraphQL.Types
open Iris.Option.Builders

open GraphQL.FSharp.Model
open GraphQL.FSharp.Builder.Helpers

[<AutoOpen>]
module rec Wrapper =
    type Argument<'input, 'output> = {
        name: string option
        value: ValueType<'output>
        description: string option
        validator: Validator<'input, 'output> option
    }

    let newArgument<'input, 'output> : Argument<'input, 'output> = {
        name = None
        value = Mandatory
        description = None
        validator = None
    }

type ArgumentBuilder<'input>() =
    member __.Yield _ = newArgument<'input, 'input>

    /// Sets the name of the argument
    [<CustomOperation "name">]
    member __.Name (argument, name) = {argument with name = Some name}

    /// Sets the description of the argument
    [<CustomOperation "description">]
    member __.Description (argument, description) = {argument with description = Some description}

    /// Sets the default value of the argument
    /// Fails if we have set optional or mandatory
    [<CustomOperation "defaultValue">]
    member __.DefaultValue (argument, defaultValue) = {argument with value = DefaultValue defaultValue}

    /// Make the argument optional
    [<CustomOperation "optional">]
    member __.Optional argument = {argument with value = Optional}

    /// Make the argument optional
    [<CustomOperation "mandatory">]
    member __.Mandatory argument = {argument with value = Mandatory}

    /// Validate the argument
    [<CustomOperation "validate">]
    member __.Validate (argument, validator) =
        {
            name = argument.name
            value = argument.value
            description = argument.description
            validator = Some validator
        }

    /// Converts the elevated wrapper type into a function that can be called on initialization
    member __.Run (argument: Argument<'input, 'output>) =
        fun (schema: SchemaImplementation) (field: FieldType) -> maybeOrThrow {
            let schemaType = getType schema typeof<'input> (isNullable argument.value)
            let queryArgument = ValidatedArgument(schemaType, field)

            let! name = argument.name
            queryArgument.Name <- name

            maybeUnit {
                let! description = argument.description
                queryArgument.Description <- description
            }

            maybeUnit {
                let! defaultValue =
                    match argument.value with
                    | DefaultValue value -> Some value
                    | _ -> None
                queryArgument.DefaultValue <- box defaultValue
            }

            maybeUnit {
                let! validator = argument.validator
                queryArgument.SetValidator validator
            }

            field.Arguments.Add queryArgument
        }

let argument<'input> = ArgumentBuilder<'input> ()
