module GraphQL.FSharp.IntegrationTests.Utils

open Swensen.Unquote
open GraphQL
open GraphQL.Types
open Newtonsoft.Json.Linq

let queryEqual query expected (schema: Schema) =
    let expected = JObject.Parse expected
    let result =
        schema.Execute (fun options -> options.Query <- query)
        |> JObject.Parse
    test <@ JToken.DeepEquals (result, expected) @>
