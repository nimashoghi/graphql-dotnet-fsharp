module GraphQL.FSharp.Builder.Analyzers

// TODO: Run analyzers only once
// TODO: AspNetCore analyzer for WebSockets

open System

open GraphQL.FSharp.Builder.Operations
open GraphQL.FSharp.Types

type Analyzer<'t> = IOperation<'t> list -> IOperation<'t> list

let analyzer (f: IOperation<'t> list -> string option): Analyzer<'t> =
    fun (parameters: IOperation<'t> list) ->
        f parameters
        |> Option.iter (eprintfn "Analyzer failed: %s\n%s" Environment.StackTrace)

        parameters

let applyAnalyzer (parameters: IOperation<'t> list) (analyzer: Analyzer<'t>) =
    analyzer parameters

let applyAnalyzers parameters analyzers =
    analyzers
    |> List.fold applyAnalyzer parameters

module Field =
    let noResolver parameters =
        let resolver =
            parameters
            |> List.tryFind (
                fun (Id id) ->
                    id = "manual"
                    || id = "property"
                    || id = "method"
                    || id = "contextMethod"
            )
        match resolver with
        | Some _ -> None
        | None -> Some "No resolver found!"

    let analyzers<'field, 'arguments, 'source> : Analyzer<Field<'field, 'arguments, 'source>> list = [
        analyzer noResolver
    ]

let field parameters = applyAnalyzers parameters Field.analyzers
