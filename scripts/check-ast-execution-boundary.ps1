$ErrorActionPreference = 'Stop'

$tempRoot = Join-Path ([System.IO.Path]::GetTempPath()) (
  'myframework-ast-boundary-' + [System.Guid]::NewGuid().ToString('N')
)
[System.IO.Directory]::CreateDirectory($tempRoot) | Out-Null

function Assert-NegativeCompile {
  param(
    [string]$Name,
    [string]$Source,
    [string]$ExpectedPattern
  )
  $path = Join-Path $tempRoot ($Name + '.hs')
  [System.IO.File]::WriteAllText(
    $path,
    $Source,
    [System.Text.UTF8Encoding]::new($false)
  )
  $previousPreference = $ErrorActionPreference
  $ErrorActionPreference = 'Continue'
  $output = @(
    & stack exec -- ghc `
      -fno-code `
      -hide-all-packages `
      -package base `
      -package myframework `
      $path 2>&1
  )
  $exitCode = $LASTEXITCODE
  $ErrorActionPreference = $previousPreference
  $rendered = $output -join "`n"
  if ($exitCode -eq 0) {
    throw "$Name unexpectedly compiled"
  }
  if ($rendered -notmatch $ExpectedPattern) {
    throw "$Name failed for the wrong reason:`n$rendered"
  }
  return @{
    name = $Name
    passed = $true
  }
}

try {
  $checks = @(
    Assert-NegativeCompile `
      'direct-handler-invocation-is-not-public' `
      @'
module DirectInvocationBoundary where
import MyFramework
forbidden = invokeCude
'@ `
      'invokeCude.*not in scope|Variable not in scope.*invokeCude'

    Assert-NegativeCompile `
      'operator-ref-is-rejected' `
      @'
module OperatorBoundary where
import MyFramework.CURDE
forbidden = OperatorRef "increment"
'@ `
      'OperatorRef.*not in scope|Data constructor not in scope.*OperatorRef'

    Assert-NegativeCompile `
      'missing-execution-permit-cannot-invoke-handler' `
      @'
module InternalHandlerBoundary where
import MyFramework.Handler.Internal
forbidden = ()
'@ `
      'hidden module|Could not load module.*MyFramework.Handler.Internal'
  )

  $renderedChecks =
    $checks |
      ForEach-Object {
        '{"name":"' + $_.name + '","passed":true}'
      }
  Write-Output (
    '{"schema":"ast-execution-boundary.v1","result":"passed",' +
      '"claims":[' +
        '{"name":"curde-public-facade-boundary","status":"established"},' +
        '{"name":"curde-runtime-ast-execution-provenance","status":"established"}' +
      '],"checks":[' +
      ($renderedChecks -join ',') +
      ']}'
  )
}
finally {
  if ([System.IO.Directory]::Exists($tempRoot)) {
    Remove-Item -LiteralPath $tempRoot -Recurse -Force
  }
}

exit 0
