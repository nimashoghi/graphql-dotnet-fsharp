task run-sample {
    begin { Push-Location .\samples\GraphQL.FSharp.TestServer }
    process { dotnet run }
    end { Pop-Location }
}

task watch {
    begin { Push-Location .\tests\GraphQL.FSharp.Tests }
    process { dotnet watch test }
    end { Pop-Location }
}

task test {
    dotnet test
}
