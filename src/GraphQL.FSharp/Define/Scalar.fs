[<AutoOpen>]
module GraphQL.FSharp.DefineScalar

open GraphQL.Language.AST

// TODO: Single case unions (e.g. type Email = Email of string)

let internal (|SingleField|_|) (case: UnionCaseInfo) =
    match case.GetFields () with
    | [|field|] -> Some field
    | _ -> None

let internal (|SingleCase|_|) ``type`` =
    match FSharpType.GetUnionCases ``type`` with
    | [|SingleField field|] -> Some field
    | _ -> None

let internal scalarGraphType
    (serialize: 't -> obj option)
    (parseValue: obj -> 't option)
    (parseLiteral: IValue -> 't option) =
    {
        new ScalarGraphType () with
            override __.Serialize value =
                match value with
                | :? 't as value ->
                    serialize value
                    |> Option.toObj
                | _ -> null

            override __.ParseValue value =
                parseValue value
                |> Option.map box
                |> Option.toObj

            override __.ParseLiteral literal =
                parseLiteral literal
                |> Option.map box
                |> Option.toObj
    }

let Scalar<'t> =
    match typeof<'t> with
    | SingleCase field -> ()
    | _ -> invalidArg "'t" "Type argument must be a single case discriminated union."
