param()

$ErrorActionPreference = "Stop"
$repoRoot = [System.IO.Path]::GetFullPath((Join-Path $PSScriptRoot ".."))
$generatedRoot = [System.IO.Path]::GetFullPath(
    (Join-Path $repoRoot ".generated")
)
$artifactRoot = Join-Path $generatedRoot "negative-artifact"
$evidenceRoot = Join-Path $generatedRoot "negative-evidence"
$outsideRoot = Join-Path $repoRoot ".negative-artifact-outside-generated"
$utf8NoBom = New-Object System.Text.UTF8Encoding($false)

function Assert-DirectGeneratedChild {
    param([string]$Path)

    $fullPath = [System.IO.Path]::GetFullPath($Path)
    $actualParent = [System.IO.Path]::GetFullPath(
        [System.IO.Path]::GetDirectoryName($fullPath)
    )
    if ($actualParent -ne $generatedRoot) {
        throw "refusing negative-test cleanup outside $generatedRoot`: $fullPath"
    }
}

function Remove-GeneratedChild {
    param([string]$Path)

    Assert-DirectGeneratedChild $Path
    if (Test-Path -LiteralPath $Path) {
        Remove-Item -LiteralPath $Path -Recurse -Force
    }
}

function Invoke-ExpectedFailure {
    param(
        [string]$Label,
        [string]$Expected,
        [scriptblock]$Command
    )

    $currentOutput = @(& $Command 2>&1)
    $currentExit = $LASTEXITCODE
    $currentText = $currentOutput -join "`n"
    if ($currentExit -eq 0) {
        throw "$Label unexpectedly succeeded"
    }
    if (-not $currentText.Contains($Expected)) {
        throw "$Label did not report $Expected`: $currentText"
    }
}

if (Test-Path -LiteralPath $outsideRoot) {
    throw "refusing to use pre-existing outside-root negative target: $outsideRoot"
}

Remove-GeneratedChild $artifactRoot
Remove-GeneratedChild $evidenceRoot
New-Item -ItemType Directory -Path $evidenceRoot | Out-Null

Push-Location $repoRoot
try {
    $materializeOutput = @(
        stack --work-dir .stack-work-codex exec self-artifact-tool -- `
            materialize negative $artifactRoot
    )
    if ($LASTEXITCODE -ne 0) {
        throw "negative fixture materialization failed"
    }

    Invoke-ExpectedFailure `
        "outside generated root" `
        "ArtifactOutputOutsideGeneratedRoot" {
            stack --work-dir .stack-work-codex exec self-artifact-tool -- `
                materialize negative-outside $outsideRoot
        }

    Invoke-ExpectedFailure `
        "duplicate artifact root" `
        "ArtifactOutputAlreadyExists" {
            stack --work-dir .stack-work-codex exec self-artifact-tool -- `
                materialize negative-duplicate $artifactRoot
        }

    $fakeSemantic = Join-Path $evidenceRoot "forged-semantics.json"
    $fakeRuntime = Join-Path $evidenceRoot "forged-runtime.json"
    [System.IO.File]::WriteAllText(
        $fakeSemantic,
        '{"schema":"curde-semantics-evidence.v1","result":"passed"}',
        $utf8NoBom
    )
    [System.IO.File]::WriteAllText(
        $fakeRuntime,
        '{"schema":"curde-runtime-witness.v1","result":"passed"}',
        $utf8NoBom
    )
    Invoke-ExpectedFailure `
        "forged stage evidence" `
        "StageEvidenceSemanticWitnessInvalid" {
            stack --work-dir .stack-work-codex exec self-artifact-tool -- `
                evidence forged $fakeSemantic $fakeRuntime $artifactRoot
        }

    $generatedModel = Join-Path `
        $artifactRoot `
        "src\MyFramework\Generated\SelfModel.hs"
    [System.IO.File]::AppendAllText(
        $generatedModel,
        "`n-- deliberate negative-test tamper`n",
        $utf8NoBom
    )
    Invoke-ExpectedFailure `
        "tampered artifact" `
        "ArtifactManifestDigestMismatch" {
            stack --work-dir .stack-work-codex exec self-artifact-tool -- `
                verify $artifactRoot
        }

    Write-Host (
        '{"schema":"myframework-negative-gate.v1",' +
        '"result":"passed",' +
        '"checks":["outside-root","duplicate-root",' +
        '"forged-evidence","tampered-artifact"]}'
    )
}
finally {
    Pop-Location
    Remove-GeneratedChild $artifactRoot
    Remove-GeneratedChild $evidenceRoot
}

# Expected native failures above must not leak into the calling pre-gate.
$global:LASTEXITCODE = 0
