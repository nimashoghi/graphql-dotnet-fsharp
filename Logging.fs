module GraphQL.FSharp.Logging

open System
open System.Runtime.CompilerServices
open FSharp.Reflection

type ILogger = interface end

[<Extension>]
type LoggerExtensions() =
    [<Extension>]
    static member Log (this: #ILogger, format, [<CallerMemberName>] ?memberName: string) =
        let ``type`` = this.GetType()
        assert (FSharpType.IsUnion ``type``)

        let case, fields = FSharpValue.GetUnionFields (this, ``type``)
        assert (Array.isEmpty fields)

        let memberName = Option.get memberName
        let memberName =
            if String.exists Char.IsWhiteSpace memberName
            then sprintf "``%s``" memberName
            else memberName

        Printf.kprintf (printfn "[%s::%s] %s" case.Name memberName) format

type Logging =
| Trace
| Debug
| Normal
| Warning
| Error
    interface ILogger
