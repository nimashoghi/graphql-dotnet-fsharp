#r "paket:
nuget FSharp.Core
nuget Fake.Core.Target
nuget Fake.DotNet.Cli //"
#load "./.fake/build.fsx/intellisense.fsx"

open System.IO
open Fake.Core
open Fake.DotNet
open Fake.IO

module Shell =
    let command command =
        CreateProcess.fromRawCommand "cmd" ["/C"; command]

    let commandf format = Printf.kprintf command format

module DotNet =
    let clean settings =
        DotNet.exec settings "clean" "" |> ignore

    let run settings args project =
        let args =
            ["--project"; project] @ args
            |> List.toSeq
            |> String.concat " "
        DotNet.exec settings "run" args |> ignore

let getFsproj directory =
    DirectoryInfo.getFiles directory
    |> Array.tryFind (fun file -> file.Extension = ".fsproj")

let internal getProjects directory =
    DirectoryInfo.ofPath directory
    |> DirectoryInfo.getSubDirectories
    |> Array.choose getFsproj

let sampleProjects = getProjects "samples"
let srcProjects = getProjects "src"
let testProjects = getProjects "tests"

let projects =
    [|
        yield! sampleProjects
        yield! srcProjects
        yield! testProjects
    |]

let runOnAllProjects f (projects: FileInfo []) =
    projects
    |> Array.map (fun project -> project.FullName)
    |> Array.iter f

Target.create "Restore" (fun _ ->
    projects
    |> runOnAllProjects (DotNet.restore id)
)

Target.create "Clean" (fun _ ->
    DotNet.clean id

    projects
    |> Array.map (fun fsproj -> fsproj.Directory)
    |> Array.collect (fun directory ->
        [|
            Path.combine directory.FullName "bin"
            Path.combine directory.FullName "obj"
        |]
    )
    |> Shell.deleteDirs
)

Target.create "Reload" ignore

Target.create "Build" (fun _ ->
    projects
    |> Array.map (fun fsproj -> fsproj.FullName)
    |> Array.iter (DotNet.build id)
)

Target.create "Sample" (fun _ ->
    DotNet.run id [] "./samples/GraphQL.FSharp.TestServer/GraphQL.FSharp.TestServer.fsproj"
)

Target.create "Test" (fun _ ->
    testProjects
    |> Array.map (fun fsproj -> fsproj.FullName)
    |> Array.iter (DotNet.test id)
)

Target.create "Projects" (fun _ ->
    projects
    |> Array.map (fun project -> project.FullName)
    |> Array.iter Trace.log
)

Target.create "NuGet" (fun _ ->
    srcProjects
    |> Array.map (fun project -> project.FullName)
    |> Array.iter (
        fun project ->
            DotNet.pack (
                fun options ->
                    {options with Configuration = DotNet.BuildConfiguration.Release}
            ) project
    )
)

let getProjectFiles (project: FileInfo) =
    project.Directory
    |> DirectoryInfo.getMatchingFilesRecursive "*.fs"

let isEmptyLine line = String.trim line = ""

let countLOC (file: FileInfo) =
    File.read file.FullName
    |> Seq.filter (not << isEmptyLine)
    |> Seq.length

Target.create "CountLOC" (fun _ ->
    srcProjects
    |> Array.collect getProjectFiles
    |> Array.sumBy countLOC
    |> Trace.logf "LOC: %i\n"
)

module TodoChecker =
    let run (file: FileInfo) =
        Shell.commandf "todo-checker.cmd --single %s" file.FullName
        |> Proc.run
        |> ignore

Target.create "TODOS" (fun _ ->
    projects
    |> Array.iter TodoChecker.run
)

open Fake.Core.TargetOperators

"Build" ==> "Nuget"
"Build" ==> "Nuget"

"Clean" ==> "Reload"
"Restore" ==> "Reload"

Target.runOrDefault "Build"
