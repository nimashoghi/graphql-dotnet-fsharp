task watch {
    begin { Push-Location .\tests\GraphQL.FSharp.Tests }
    process { dotnet watch test }
    end { Pop-Location }
}

task test {
    dotnet test
}
