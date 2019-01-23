$totalLoc = 0

foreach ($item in Get-ChildItem *.fs, *.fsproj -Recurse -Force) {
    $totalLoc += (Get-Content -Path $item.FullName | Measure-Object -Line).Lines
}

Write-Output "Total LOC is $totalLoc"
