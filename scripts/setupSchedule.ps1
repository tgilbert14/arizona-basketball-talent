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
# PowerShell 5.1-compatible. This script only REGISTERS the task; it never
# starts a refresh itself.
# ---------------------------------------------------------------------------

param(
    [string]$At = '23:30',
    [switch]$WakeToRun
)

$ErrorActionPreference = 'Stop'

$TaskName = 'GirthIndex Nightly Refresh'
$RepoRoot = Split-Path -Parent $PSScriptRoot
$Runner   = Join-Path $RepoRoot 'scripts\runNightly.ps1'

if (-not (Test-Path -LiteralPath $Runner)) {
    Write-Output ('ERROR: ' + $Runner + ' not found -- run this script from the repo checkout.')
    exit 1
}

# Idempotent: drop any existing registration first
$Existing = Get-ScheduledTask -TaskName $TaskName -ErrorAction SilentlyContinue
if ($Existing) {
    Unregister-ScheduledTask -TaskName $TaskName -Confirm:$false
    Write-Output ('Removed existing task ''' + $TaskName + ''' (re-registering).')
}

$Action = New-ScheduledTaskAction -Execute 'powershell.exe' `
    -Argument ('-NoProfile -ExecutionPolicy Bypass -File "' + $Runner + '"') `
    -WorkingDirectory $RepoRoot

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
    -Settings $Settings `
    -Description 'Big 12 Talent Lab: nightly scrape/validate/precompute/publish (scripts/nightlyRefresh.R via scripts/runNightly.ps1)' `
    | Out-Null

$Info = Get-ScheduledTaskInfo -TaskName $TaskName

Write-Output ''
Write-Output ('Registered task : ' + $TaskName)
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
