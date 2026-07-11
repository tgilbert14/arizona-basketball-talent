# ---------------------------------------------------------------------------
# runNightly.ps1 -- Task Scheduler entry point for the nightly data refresh.
#
# Sets the working directory to the repo, wraps scripts/nightlyRefresh.R in a
# transcript log under logs/, and passes through any arguments unchanged
# (e.g. no-deploy no-alert). Exit code = the R script's exit code:
#   0 = success or no-op, 1 = failure, 2 = degraded (see manifest).
#
# PowerShell 5.1-compatible (the Task Scheduler default host).
# Registered by scripts/setupSchedule.ps1; can also be run by hand:
#   powershell -NoProfile -ExecutionPolicy Bypass -File scripts\runNightly.ps1
# ---------------------------------------------------------------------------

$ErrorActionPreference = 'Continue'

$RepoRoot = 'C:\Users\tsgil\OneDrive\Documents\VGS - R\arizona-basketball-talent'
Set-Location -LiteralPath $RepoRoot

$LogDir = Join-Path $RepoRoot 'logs'
if (-not (Test-Path -LiteralPath $LogDir)) {
    New-Item -ItemType Directory -Path $LogDir | Out-Null
}

$Stamp   = Get-Date -Format 'yyyy-MM-dd_HHmm'
$LogPath = Join-Path $LogDir ('refresh_' + $Stamp + '.log')

$Rscript = 'C:\Program Files\R\R-4.5.2\bin\Rscript.exe'
if (-not (Test-Path -LiteralPath $Rscript)) {
    $Found = Get-Command 'Rscript.exe' -ErrorAction SilentlyContinue
    if ($Found) { $Rscript = $Found.Source }
}

$ExitCode = 1
Start-Transcript -Path $LogPath | Out-Null
try {
    Write-Output ('Nightly refresh starting: ' + (Get-Date -Format 'yyyy-MM-dd HH:mm:ss'))
    Write-Output ('Repo    : ' + $RepoRoot)
    Write-Output ('Rscript : ' + $Rscript)
    if ($args.Count -gt 0) {
        Write-Output ('Args    : ' + ($args -join ' '))
    }

    if (-not (Test-Path -LiteralPath $Rscript)) {
        Write-Output 'FATAL: Rscript.exe not found (pinned path missing and not on PATH).'
        $ExitCode = 1
    }
    else {
        # Pipe the native exe through ForEach-Object so Start-Transcript
        # records its output (PS 5.1 transcripts miss native stdout that
        # goes straight to the console). $LASTEXITCODE survives the pipeline.
        & $Rscript 'scripts/nightlyRefresh.R' @args 2>&1 | ForEach-Object { "$_" }
        $ExitCode = $LASTEXITCODE
        if ($null -eq $ExitCode) { $ExitCode = 1 }
    }

    Write-Output ('Nightly refresh finished with exit code ' + $ExitCode)
}
finally {
    Stop-Transcript | Out-Null
}

exit $ExitCode
