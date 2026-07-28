param(
    [switch]$IncludeSelfArtifact
)

$ErrorActionPreference = "Stop"
$repoRoot = [System.IO.Path]::GetFullPath((Join-Path $PSScriptRoot ".."))
$generatedRoot = Join-Path $repoRoot ".generated"
$evidenceRoot = Join-Path $generatedRoot "pre-gate-evidence"
$utf8NoBom = New-Object System.Text.UTF8Encoding($false)

function Assert-GeneratedChild {
    param([string]$Path)

    $fullPath = [System.IO.Path]::GetFullPath($Path)
    $parentPath = [System.IO.Path]::GetFullPath(
        [System.IO.Path]::GetDirectoryName($fullPath)
    )
    $expectedParent = [System.IO.Path]::GetFullPath($generatedRoot)
    if ($parentPath -ne $expectedParent) {
        throw "refusing generated cleanup outside $expectedParent`: $fullPath"
    }
}

function Reset-GeneratedChild {
    param([string]$Path)

    Assert-GeneratedChild $Path
    if (Test-Path -LiteralPath $Path) {
        Remove-Item -LiteralPath $Path -Recurse -Force
    }
    New-Item -ItemType Directory -Path $Path | Out-Null
}

function Invoke-Checked {
    param(
        [string]$Label,
        [scriptblock]$Command
    )

    Write-Host "[pre-gate] $Label"
    & $Command
    if ($LASTEXITCODE -ne 0) {
        throw "$Label failed with exit code $LASTEXITCODE"
    }
}

function Invoke-Captured {
    param(
        [string]$Label,
        [scriptblock]$Command,
        [string]$OutputPath
    )

    Write-Host "[pre-gate] $Label"
    $currentOutput = @(& $Command)
    $currentExit = $LASTEXITCODE
    [System.IO.File]::WriteAllText(
        $OutputPath,
        (($currentOutput -join "`n") + "`n"),
        $utf8NoBom
    )
    if ($currentExit -ne 0) {
        throw "$Label failed with exit code $currentExit"
    }
}

Push-Location $repoRoot
try {
    Reset-GeneratedChild $evidenceRoot

    Invoke-Checked "build" {
        stack --work-dir .stack-work-codex build
    }
    Invoke-Captured "semantic witness" {
        stack --work-dir .stack-work-codex exec curde-semantics-witness
    } (Join-Path $evidenceRoot "stage0-semantics.json")
    Invoke-Captured "runtime witness" {
        stack --work-dir .stack-work-codex exec curde-runtime-witness
    } (Join-Path $evidenceRoot "stage0-runtime.json")
    Invoke-Captured "TrustBase binding witness" {
        stack --work-dir .stack-work-codex exec trustbase-binding-witness
    } (Join-Path $evidenceRoot "stage0-trustbase-binding.json")
    Invoke-Captured "semantic self-interpret witness" {
        stack --work-dir .stack-work-codex exec core-self-interpret-witness
    } (Join-Path $evidenceRoot "stage0-self-interpret.json")
    Invoke-Captured "promotion policy witness" {
        stack --work-dir .stack-work-codex exec core-promotion-witness
    } (Join-Path $evidenceRoot "stage0-promotion.json")
    Invoke-Captured "approved SDK package witness" {
        stack --work-dir .stack-work-codex exec sdk-package-witness
    } (Join-Path $evidenceRoot "stage0-sdk-package.json")
    Invoke-Captured "self model report" {
        stack --work-dir .stack-work-codex exec self-artifact-tool -- report
    } (Join-Path $evidenceRoot "stage0-self-model.json")
    Invoke-Captured "TrustBase manifest" {
        stack --work-dir .stack-work-codex exec self-artifact-tool -- trust-base $repoRoot
    } (Join-Path $evidenceRoot "stage0-trust-base.json")
    Invoke-Checked "negative boundaries" {
        & (Join-Path $PSScriptRoot "check-negative.ps1")
    }
    Invoke-Checked "diff check" {
        git diff --check
    }

    Write-Host '{"schema":"myframework-release-pre-gate.v1","result":"passed"}'

    if ($IncludeSelfArtifact) {
        & (Join-Path $PSScriptRoot "self-artifact.ps1") `
            -EvidenceRoot $evidenceRoot
        if ($LASTEXITCODE -ne 0) {
            throw "self-artifact gate failed with exit code $LASTEXITCODE"
        }
    }
}
finally {
    Pop-Location
}
