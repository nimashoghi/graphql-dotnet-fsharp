param (
    [switch]
    $SourceOnly = $false
)

$totalLoc = 0

if ($SourceOnly) {
    $items = Get-ChildItem .\src\**\*.fs -Recurse -Force
}
else {
    $items = Get-ChildItem *.fs, *.fsproj -Recurse -Force
}

foreach ($item in $items) {
    $totalLoc += (Get-Content -Path $item.FullName | Measure-Object -Line).Lines
}

Write-Output "Total LOC is $totalLoc"
