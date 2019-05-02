[<AutoOpen>]
module GraphQL.FSharp.Builder.Documentation

open GraphQL.Types

open GraphQL.FSharp.Types

let inline description value = Operation.ConfigureUnit <| fun target ->
    (^t: (member set_Description: string -> unit) target, value)

let arguments (arguments: (string * string) list) = Operation.CreateUnit Priority.ArgumentDocumentation <| fun (field: Field<'field, 'arguments, 'source>) ->
    let queryArguments =
        field.Arguments
        |> Option.ofObj
        |> Option.defaultValue (QueryArguments ())

    arguments
    |> List.map (
        fun (key, value) ->
            let arg =
                queryArguments
                |> Seq.find (fun argument -> argument.Name = key)
            arg, value
    )
    |> List.iter (fun (arg, value) -> arg.Description <- value)
