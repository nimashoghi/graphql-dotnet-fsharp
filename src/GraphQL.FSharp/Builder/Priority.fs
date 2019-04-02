namespace GraphQL.FSharp.Builder

open System

[<Struct>]
type Priority = Priority of Priority: int

[<RequireQualifiedAccess>]
module Priority =
    let Min = Priority Int32.MinValue
    let Max = Priority Int32.MaxValue
    let Default = Priority 0

    let EnsureCorrectType = Min
    let Flatten = Min
    let InferredGraphType = Priority -200
    let Validation = Priority -100
    let InferredGraphTypeField = Priority 50
    let DefaultValue = Priority 100
    let ArgumentDocumentation = Priority 200
