properties {
    $IgnoreTodo
}

task Default -depends Restore

task Test -depends Check-Todos-Test, Restore-Test {
    begin {
        Push-Location "$PSScriptRoot\GraphQL.FSharp.Test"
    } process {
        dotnet run
    } end {
        Pop-Location
    }
}

task Check-Todos-Test -depends Check-Todos {
    if ($IgnoreTodo) { return }
    begin {
        Push-Location "$PSScriptRoot\GraphQL.FSharp.Test"
    } process {
        exec {
            todo-checker.cmd "$PSScriptRoot\GraphQL.FSharp.Test.fsproj"
        }
    } end {
        Pop-Location
    }
}

task Check-Todos {
    if ($IgnoreTodo) { return }
    exec {
        todo-checker.cmd "$PSScriptRoot\GraphQL.FSharp.fsproj"
    }
}

task Restore-Test -depends Restore, Clean-Test {
    begin {
        Push-Location "$PSScriptRoot\GraphQL.FSharp.Test"
    } process {
        dotnet restore
    } end {
        Pop-Location
    }
}

task Clean-Test {
    begin {
        Push-Location "$PSScriptRoot\GraphQL.FSharp.Test"
    } process {
        Remove-Item -ErrorAction Ignore -Recurse -Force bin
        Remove-Item -ErrorAction Ignore -Recurse -Force obj
        dotnet clean
    } end {
        Pop-Location
    }
}

task Run -depends Restore {
    dotnet run
}

task Build -depends Restore {
    dotnet build
}

task Restore -depends Clean {
    dotnet restore
}

task Clean {
    Remove-Item -Recurse -Force -ErrorAction Ignore .\bin
    Remove-Item -Recurse -Force -ErrorAction Ignore .\obj
    dotnet clean
}
