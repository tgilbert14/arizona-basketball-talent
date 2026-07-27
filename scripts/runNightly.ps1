# ---------------------------------------------------------------------------
# runNightly.ps1 -- Task Scheduler entry point for the nightly data refresh.
#
# Sets the working directory to the repo, wraps scripts/nightlyRefresh.R in a
# transcript log under logs/, and passes through any arguments unchanged
# (e.g. no-deploy no-alert). Exit code = the R script's exit code:
#   0 = success or no-op, 1 = failure, 2 = degraded (see manifest).
#
# Rscript resolution: newest installed "C:\Program Files\R\R-*" wins, the
# historical pin is the fallback -- an R upgrade deletes the old R-x.y.z
# folder, and a hard pin would strand the schedule (found by ship's license
# exam, 2026-07-11). If R still cannot launch, or it dies before writing this
# run's logs/refresh_manifest_*.json, the wrapper itself raises the gh issue
# (all in-pipeline alerting lives in R's S10 stage, which never runs in that
# failure mode). Set NIGHTLY_ALERT_DRYRUN=1 to print the would-be alert
# instead of calling gh.
#
# PowerShell 5.1-compatible (the Task Scheduler default host).
# Registered by scripts/setupSchedule.ps1; can also be run by hand:
#   powershell -NoProfile -ExecutionPolicy Bypass -File scripts\runNightly.ps1
# ---------------------------------------------------------------------------

$ErrorActionPreference = 'Continue'

$RepoRoot = Split-Path -Parent $PSScriptRoot
$PublishBranch = 'main'
$NightlyBranch = 'automation/nightly-main'
Set-Location -LiteralPath $RepoRoot

$env:GIRTH_PUBLISH_BRANCH = $PublishBranch
$env:GIRTH_NIGHTLY_BRANCH = $NightlyBranch
$env:GIRTH_NIGHTLY_RUNNER = '1'
$env:GIT_TERMINAL_PROMPT = '0'
$env:GCM_INTERACTIVE = 'Never'

$LogDir = Join-Path $RepoRoot 'logs'
if (-not (Test-Path -LiteralPath $LogDir)) {
    New-Item -ItemType Directory -Path $LogDir | Out-Null
}

$Stamp   = Get-Date -Format 'yyyy-MM-dd_HHmm'
$LogPath = Join-Path $LogDir ('refresh_' + $Stamp + '.log')

# --- resolve Rscript: newest installed R > historical pin > PATH ----------
$PinnedRscript = 'C:\Program Files\R\R-4.5.2\bin\Rscript.exe'
$Rscript = $PinnedRscript
$RCandidates = @(Get-ChildItem -Path 'C:\Program Files\R' -Directory -Filter 'R-*' -ErrorAction SilentlyContinue |
    ForEach-Object {
        $exe = Join-Path $_.FullName 'bin\Rscript.exe'
        $ver = $null
        if ([version]::TryParse(($_.Name -replace '^R-', ''), [ref]$ver) -and
            (Test-Path -LiteralPath $exe)) {
            New-Object psobject -Property @{ Version = $ver; Exe = $exe }
        }
    })
if ($RCandidates.Count -gt 0) {
    $Rscript = ($RCandidates | Sort-Object Version -Descending | Select-Object -First 1).Exe
}
if (-not (Test-Path -LiteralPath $Rscript)) {
    $Found = Get-Command 'Rscript.exe' -ErrorAction SilentlyContinue
    if ($Found) { $Rscript = $Found.Source }
}

$ExitCode = 1
$RunStart = Get-Date
Start-Transcript -Path $LogPath | Out-Null
try {
    Write-Output ('Nightly refresh starting: ' + (Get-Date -Format 'yyyy-MM-dd HH:mm:ss'))
    Write-Output ('Repo    : ' + $RepoRoot)
    Write-Output ('Rscript : ' + $Rscript)
    if ($args.Count -gt 0) {
        Write-Output ('Args    : ' + ($args -join ' '))
    }

    # The scheduled runner must be isolated from interactive checkouts.
    $PublishReady = $true
    $Git = Get-Command 'git.exe' -ErrorAction SilentlyContinue
    if (-not $Git) {
        Write-Output 'FATAL: git.exe not found; refusing to run a publish-capable refresh.'
        $PublishReady = $false
    }

    # Resolve interrupted-rebase state through Git because .git is a file in
    # linked worktrees. Abort before branch inspection (a rebase detaches HEAD).
    if ($PublishReady) {
        $RebaseStateFound = $false
        foreach ($StateName in @('rebase-merge', 'rebase-apply')) {
            $StateOutput = @(& git -C $RepoRoot rev-parse --git-path $StateName)
            if ($LASTEXITCODE -ne 0) {
                Write-Output 'FATAL: could not inspect Git rebase state.'
                $PublishReady = $false
                break
            }
            $StatePath = ($StateOutput -join '').Trim()
            if (-not [IO.Path]::IsPathRooted($StatePath)) {
                $StatePath = Join-Path $RepoRoot $StatePath
            }
            if (Test-Path -LiteralPath $StatePath) { $RebaseStateFound = $true }
        }
        if ($PublishReady -and $RebaseStateFound) {
            Write-Output 'Interrupted rebase found; aborting before nightly sync.'
            & git -C $RepoRoot rebase --abort 2>&1 | ForEach-Object { "$_" }
            if ($LASTEXITCODE -ne 0) {
                Write-Output 'FATAL: could not abort the interrupted rebase.'
                $PublishReady = $false
            }
        }
    }

    if ($PublishReady) {
        $BranchOutput = @(& git -C $RepoRoot branch --show-current)
        $GitExit = $LASTEXITCODE
        $CurrentBranch = ($BranchOutput -join '').Trim()
        if (($GitExit -ne 0) -or ($CurrentBranch -ne $NightlyBranch)) {
            Write-Output ('FATAL: nightly runner is on ''' + $CurrentBranch +
                          '''; expected dedicated branch ''' + $NightlyBranch + '''.')
            $PublishReady = $false
        }
    }

    if ($PublishReady) {
        $TrackedChanges = @(& git -C $RepoRoot status --porcelain --untracked-files=all)
        if (($LASTEXITCODE -ne 0) -or ($TrackedChanges.Count -gt 0)) {
            Write-Output 'FATAL: dedicated nightly worktree has changes; refusing to overwrite them.'
            if ($TrackedChanges.Count -gt 0) {
                $TrackedChanges | ForEach-Object { Write-Output ('  ' + $_) }
            }
            $PublishReady = $false
        }
    }

    if ($PublishReady) {
        & git -C $RepoRoot fetch origin $PublishBranch --prune 2>&1 |
            ForEach-Object { "$_" }
        if ($LASTEXITCODE -ne 0) {
            Write-Output ('FATAL: could not fetch origin/' + $PublishBranch + '.')
            $PublishReady = $false
        }
    }

    if ($PublishReady) {
        & git -C $RepoRoot rebase ('origin/' + $PublishBranch) 2>&1 |
            ForEach-Object { "$_" }
        if ($LASTEXITCODE -ne 0) {
            & git -C $RepoRoot rebase --abort 2>$null | Out-Null
            Write-Output ('FATAL: could not rebase the nightly branch onto origin/' +
                          $PublishBranch + '.')
            $PublishReady = $false
        }
    }

    # Ship a clean commit left behind by a previous transient push failure,
    # even when tonight's scrape later finds no new source changes.
    if ($PublishReady) {
        $AheadRange = ('origin/' + $PublishBranch + '..HEAD')
        $AheadOutput = @(& git -C $RepoRoot rev-list --count $AheadRange)
        $AheadCount = 0
        if (($LASTEXITCODE -ne 0) -or
            (-not [int]::TryParse(($AheadOutput -join '').Trim(), [ref]$AheadCount))) {
            Write-Output 'FATAL: could not determine whether the nightly branch is ahead.'
            $PublishReady = $false
        }
        elseif ($AheadCount -gt 0) {
            Write-Output ('Recovering ' + $AheadCount +
                          ' previously committed nightly update(s) to origin/' +
                          $PublishBranch + '.')
            & git -C $RepoRoot push origin ('HEAD:' + $PublishBranch) 2>&1 |
                ForEach-Object { "$_" }
            if ($LASTEXITCODE -ne 0) {
                Write-Output ('FATAL: recovery push to origin/' + $PublishBranch + ' failed.')
                $PublishReady = $false
            }
        }
    }

    if (-not $PublishReady) {
        $ExitCode = 1
    }
    elseif (-not (Test-Path -LiteralPath $Rscript)) {
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

    # --- last-ditch alert: R died before it could alert for itself --------
    # All in-pipeline alerting (gh_alert) lives in R's S10 stage, and S10
    # always writes logs/refresh_manifest_<run>.json. Nonzero exit with NO
    # manifest written this run means R never got that far (missing Rscript,
    # parse error, hard crash) -- without this block, that failure mode is
    # silent forever. try/catch so alerting can never mask the exit code.
    if ($ExitCode -ne 0) {
        try {
            $ManifestWritten = @(Get-ChildItem -Path $LogDir -Filter 'refresh_manifest_*.json' -ErrorAction SilentlyContinue |
                Where-Object { $_.LastWriteTime -ge $RunStart })
            if ($ManifestWritten.Count -eq 0) {
                $Title = 'Nightly refresh failed -- died before alerting could run'
                $Body  = ('The ' + (Get-Date -Format 'yyyy-MM-dd HH:mm') + ' run exited ' +
                          $ExitCode + ' without writing a run manifest, so the pipeline''s ' +
                          'own gh alert (R stage S10) never executed. Likely causes: ' +
                          'Rscript.exe missing (R upgrade removed the pinned version), an R ' +
                          'startup/parse failure, or a hard crash. Resolved Rscript path this ' +
                          'run: ' + $Rscript + '. Transcript: ' + $LogPath)
                if ($env:NIGHTLY_ALERT_DRYRUN) {
                    Write-Output ('DRYRUN wrapper alert -- would open/update gh issue: ' + $Title)
                }
                else {
                    $Gh = Get-Command 'gh' -ErrorAction SilentlyContinue
                    if ($Gh) {
                        # same dedupe family as the pipeline's gh_alert: comment on an
                        # open "Nightly refresh failed" issue instead of stacking new ones
                        $Existing = & gh issue list --state open --label auto-refresh --search 'Nightly refresh failed in:title' --json number --jq '.[0].number' 2>$null
                        if ($Existing) {
                            & gh issue comment $Existing --body $Body 2>$null | Out-Null
                            Write-Output ('Wrapper alert: commented on open issue #' + $Existing)
                        }
                        else {
                            & gh issue create --title $Title --body $Body --label auto-refresh 2>$null | Out-Null
                            Write-Output 'Wrapper alert: opened a new gh issue'
                        }
                    }
                    else {
                        Write-Output 'Wrapper alert: gh CLI not found -- alert NOT sent (transcript is the only record)'
                    }
                }
            }
        }
        catch {
            Write-Output ('Wrapper alert failed (non-fatal): ' + $_.Exception.Message)
        }
    }
}
finally {
    Stop-Transcript | Out-Null
}

exit $ExitCode
