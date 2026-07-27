# ---------------------------------------------------------------------------
# setupSchedule.ps1 -- one-time (idempotent) registration of the Windows
# Task Scheduler job that runs the nightly data refresh.
#
#   powershell -NoProfile -ExecutionPolicy Bypass -File scripts\setupSchedule.ps1
#   ...\setupSchedule.ps1 -At '02:00'      # different start time
#   ...\setupSchedule.ps1 -WakeToRun       # allow waking the machine from sleep
#
# Registers 'GirthIndex Nightly Refresh': daily at 23:30 local time, runs
# scripts\runNightly.ps1 (which wraps scripts/nightlyRefresh.R). Re-running
# this script replaces the existing registration. The task runs as the
# current user and does NOT run when nobody has logged on since the last
# boot -- keep the laptop on (or asleep with -WakeToRun) and logged in.
#
# PowerShell 5.1-compatible. This script prepares the isolated nightly
# worktree and REGISTERS the task; it never starts a refresh itself.
# ---------------------------------------------------------------------------

param(
    [string]$At = '23:30',
    [switch]$WakeToRun,
    [string]$WorktreeRoot = ''
)

$ErrorActionPreference = 'Stop'

$TaskName = 'GirthIndex Nightly Refresh'
$SourceRepoRoot = Split-Path -Parent $PSScriptRoot
$PublishBranch = 'main'
$NightlyBranch = 'automation/nightly-main'
$env:GIT_TERMINAL_PROMPT = '0'
$env:GCM_INTERACTIVE = 'Never'

if ([string]::IsNullOrWhiteSpace($WorktreeRoot)) {
    $LocalDataRoot = [Environment]::GetFolderPath('LocalApplicationData')
    if ([string]::IsNullOrWhiteSpace($LocalDataRoot)) {
        throw 'LocalApplicationData is unavailable; pass -WorktreeRoot explicitly.'
    }
    $WorktreeRoot = Join-Path $LocalDataRoot 'GirthIndex\nightly-main'
}
$WorktreeRoot = [IO.Path]::GetFullPath($WorktreeRoot)

$Git = Get-Command 'git.exe' -ErrorAction SilentlyContinue
if (-not $Git) {
    throw 'git.exe is required to prepare the dedicated nightly worktree.'
}

& git -C $SourceRepoRoot fetch origin $PublishBranch --prune
if ($LASTEXITCODE -ne 0) {
    throw ('Could not fetch origin/' + $PublishBranch + '.')
}

& git -C $SourceRepoRoot worktree prune
if ($LASTEXITCODE -ne 0) { throw 'Could not prune stale Git worktree metadata.' }

if (-not (Test-Path -LiteralPath $WorktreeRoot)) {
    $WorktreeParent = Split-Path -Parent $WorktreeRoot
    if (-not (Test-Path -LiteralPath $WorktreeParent)) {
        New-Item -ItemType Directory -Path $WorktreeParent -Force | Out-Null
    }

    & git -C $SourceRepoRoot show-ref --verify --quiet ('refs/heads/' + $NightlyBranch)
    $BranchExists = ($LASTEXITCODE -eq 0)
    if ($BranchExists) {
        & git -C $SourceRepoRoot worktree add $WorktreeRoot $NightlyBranch
    }
    else {
        & git -C $SourceRepoRoot worktree add -b $NightlyBranch $WorktreeRoot ('origin/' + $PublishBranch)
    }
    if ($LASTEXITCODE -ne 0) {
        throw ('Could not create dedicated nightly worktree at ' + $WorktreeRoot + '.')
    }
}

if (-not (Test-Path -LiteralPath (Join-Path $WorktreeRoot '.git'))) {
    throw ($WorktreeRoot + ' exists but is not a Git worktree.')
}

$SourceCommonOutput = @(& git -C $SourceRepoRoot rev-parse --git-common-dir)
if ($LASTEXITCODE -ne 0) { throw 'Could not resolve the source Git directory.' }
$SourceCommonPath = ($SourceCommonOutput -join '').Trim()
if (-not [IO.Path]::IsPathRooted($SourceCommonPath)) {
    $SourceCommonPath = Join-Path $SourceRepoRoot $SourceCommonPath
}

$WorktreeCommonOutput = @(& git -C $WorktreeRoot rev-parse --git-common-dir)
if ($LASTEXITCODE -ne 0) { throw 'Could not resolve the nightly Git directory.' }
$WorktreeCommonPath = ($WorktreeCommonOutput -join '').Trim()
if (-not [IO.Path]::IsPathRooted($WorktreeCommonPath)) {
    $WorktreeCommonPath = Join-Path $WorktreeRoot $WorktreeCommonPath
}
if ([IO.Path]::GetFullPath($SourceCommonPath) -ne
    [IO.Path]::GetFullPath($WorktreeCommonPath)) {
    throw ($WorktreeRoot + ' belongs to a different Git repository.')
}

$RebaseStateFound = $false
foreach ($StateName in @('rebase-merge', 'rebase-apply')) {
    $StateOutput = @(& git -C $WorktreeRoot rev-parse --git-path $StateName)
    if ($LASTEXITCODE -ne 0) { throw 'Could not inspect nightly rebase state.' }
    $StatePath = ($StateOutput -join '').Trim()
    if (-not [IO.Path]::IsPathRooted($StatePath)) {
        $StatePath = Join-Path $WorktreeRoot $StatePath
    }
    if (Test-Path -LiteralPath $StatePath) { $RebaseStateFound = $true }
}
if ($RebaseStateFound) {
    & git -C $WorktreeRoot rebase --abort
    if ($LASTEXITCODE -ne 0) { throw 'Could not abort interrupted nightly rebase.' }
}

$BranchOutput = @(& git -C $WorktreeRoot branch --show-current)
$CurrentBranch = ($BranchOutput -join '').Trim()
if (($LASTEXITCODE -ne 0) -or ($CurrentBranch -ne $NightlyBranch)) {
    throw ('Nightly worktree must use branch ''' + $NightlyBranch +
           '''; found ''' + $CurrentBranch + '''.')
}

$TrackedChanges = @(& git -C $WorktreeRoot status --porcelain --untracked-files=all)
if (($LASTEXITCODE -ne 0) -or ($TrackedChanges.Count -gt 0)) {
    throw 'Dedicated nightly worktree has changes; resolve them before re-registering.'
}

& git -C $WorktreeRoot rebase ('origin/' + $PublishBranch)
if ($LASTEXITCODE -ne 0) {
    & git -C $WorktreeRoot rebase --abort 2>$null | Out-Null
    throw ('Could not rebase nightly worktree onto origin/' + $PublishBranch + '.')
}

$UpstreamRef = 'origin/' + $PublishBranch
& git -C $WorktreeRoot branch --set-upstream-to=$UpstreamRef $NightlyBranch
if ($LASTEXITCODE -ne 0) {
    throw ('Could not set ' + $NightlyBranch + ' to track ' + $UpstreamRef + '.')
}

$Runner = Join-Path $WorktreeRoot 'scripts\runNightly.ps1'
if (-not (Test-Path -LiteralPath $Runner)) {
    throw ($Runner + ' not found after preparing the nightly worktree.')
}

# Register-ScheduledTask -Force updates atomically; a bad replacement cannot
# delete a previously working task before the new definition is validated.

$Action = New-ScheduledTaskAction -Execute 'powershell.exe' `
    -Argument ('-NoProfile -ExecutionPolicy Bypass -File "' + $Runner + '"') `
    -WorkingDirectory $WorktreeRoot

$Trigger = New-ScheduledTaskTrigger -Daily -At $At

$SettingsArgs = @{
    StartWhenAvailable         = $true
    AllowStartIfOnBatteries    = $true
    DontStopIfGoingOnBatteries = $true
    RunOnlyIfNetworkAvailable  = $true
    ExecutionTimeLimit         = (New-TimeSpan -Hours 2)
}
if ($WakeToRun) { $SettingsArgs['WakeToRun'] = $true }
$Settings = New-ScheduledTaskSettingsSet @SettingsArgs

Register-ScheduledTask -TaskName $TaskName -Action $Action -Trigger $Trigger `
    -Settings $Settings -Force `
    -Description 'Girth Index Power 4: isolated nightly scrape/validate/precompute/publish to main' `
    | Out-Null

$Info = Get-ScheduledTaskInfo -TaskName $TaskName

Write-Output ''
Write-Output ('Registered task : ' + $TaskName)
Write-Output ('Worktree       : ' + $WorktreeRoot)
Write-Output ('Publish target : origin/' + $PublishBranch)
Write-Output ('Trigger         : daily at ' + $At + ' local time')
if ($WakeToRun) {
    Write-Output 'Wake to run     : yes (machine wakes from sleep for the run)'
}
else {
    Write-Output 'Wake to run     : no (a sleeping machine runs at next wake via StartWhenAvailable)'
}
Write-Output ('Time limit      : 2 hours; network required; battery OK')
Write-Output ('Next run time   : ' + $Info.NextRunTime)
Write-Output ''
Write-Output 'Reminder: the machine must be ON, or ASLEEP with -WakeToRun, at the'
Write-Output 'trigger time. A shut-down laptop runs nothing -- the data goes stale'
Write-Output 'until the next logon (StartWhenAvailable catches up then), and the'
Write-Output 'GitHub watchdog opens an issue after 5 stale days.'
