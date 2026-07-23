# Auto-refresh runbook

Operator notes for the nightly data pipeline. One machine (this laptop) does
the scraping; two hosts serve the app (shinyapps.io by direct deploy, Posit
Connect Cloud by git push).

## What runs every night

Task Scheduler fires `GirthIndex Nightly Refresh` at 23:30 local, which runs
`scripts\runNightly.ps1`, which runs `scripts/nightlyRefresh.R` under a
transcript log. Stages, in order:

1. **lock + snapshot** -- take `logs/refresh.lock`, copy the db to
   `backups/pre_run_<ts>.db` before anything touches it
2. **classes** -- `refreshClassYear.R`, newest cycle, both sports
   (commits + portal; per-school replace, never a blind wipe), then an
   ahead-year probe of `MAX(Year)+1` per sport with `--allow-empty`
   (exits 0 without touching the db until 247 opens those pages; a probe
   failure only ever degrades the night to `warn`, never aborts it)
3. **rosters** -- `scrapeRosters.R`, both sports
4. **geocode** -- `geocodeMissing.R` for new commits (state-bbox checked)
5. **records** -- `fetchOutcomes.R` (CFBD; needs `CFBD_API_KEY`; a no-op
   most of the year)
6. **audit + validate** (S4) -- `scripts/auditRefreshHoles.R` runs first
   and can WRITE to the db (it restores baseline rows for any school-year
   a scrape silently emptied), then `scripts/validateRefresh.R <snapshot>`
   gates the night; a FAIL restores the snapshot and the run reports
   `failed`
7. **precompute** -- `scripts/precomputeDefaults.R` rebuilds
   `precomputed/*.rds`, only when the content hash actually changed
8. **publish** -- git commit + push (Connect Cloud republishes on push) and
   `scripts/deployApp.R` (shinyapps.io)
9. **log + alert** -- `refresh_log` row in the db, manifests written, and a
   GitHub issue (label `auto-refresh`) on failure

If nothing changed upstream the run logs `noop` and publishes nothing.

A successful HTTP 200 with zero parsed players is **not** assumed to mean an
empty class. `refreshClassYear.R` logs `VERIFIED EMPTY` only when 247 renders
an explicit no-commitments empty-state message; every other zero response logs
`EMPTY RESPONSE`, retains existing rows, and is surfaced for re-checking. Neither
path deletes a school-year on its own.

## One-time setup checklist

- Register the schedule: `powershell -NoProfile -ExecutionPolicy Bypass
  -File scripts\setupSchedule.ps1` (add `-WakeToRun` if the laptop sleeps
  at night; add `-At '02:00'` to move the time)
- Git identity must be repo-LOCAL (`git config user.name` / `user.email`
  inside the repo, no `--global`). The S7 commit runs as a child of
  Rscript, and R on Windows sets `HOME` to `Documents\`, so git cannot see
  `C:/Users/<you>/.gitconfig` and dies with "unable to auto-detect email
  address". Set on 2026-07-11; re-set it after any fresh clone.
- Connect Cloud: open the app's settings at connect.posit.cloud and confirm
  the "automatically publish on push" toggle is ON for
  `tgilbert14/arizona-basketball-talent` main
- GitHub alerting: `gh auth status` must show a logged-in account with repo
  access (used for the failure/stale issues)
- shinyapps.io: `Rscript scripts/deployApp.R --dry-run` must report the
  `t-lama` account as configured; if not, it prints the
  `rsconnect::setAccountInfo(...)` line to run
- OneDrive: right-click the repo folder, "Always keep on this device" --
  a cloud-only placeholder file breaks both SQLite and git

## CLI flags

`scripts/nightlyRefresh.R` takes positional flags in any order:

| flag | skips |
| --- | --- |
| `no-classes` | class-year scrape |
| `no-rosters` | roster scrape |
| `no-geocode` | geocoding |
| `no-records` | CFBD season records |
| `no-precompute` | rebuilding `precomputed/*.rds` |
| `no-push` | git commit + push (Connect Cloud stays as-is) |
| `no-deploy` | the shinyapps.io deploy |
| `no-alert` | GitHub issue alerts |

Exit codes: `0` success or no-op, `1` failure, `2` degraded -- the manifest
`stages` object says whether anything was published.
`runNightly.ps1` passes flags straight through, so a manual data-only test is:

    powershell -NoProfile -ExecutionPolicy Bypass -File scripts\runNightly.ps1 no-push no-deploy no-alert

## Where things live

- `logs/refresh_<ts>.log` -- PowerShell transcript of each run
  (gitignored); the transcript captures the full R output
- `logs/refresh_manifest_<ts>.json` -- full per-run manifest (gitignored)
- `data/refresh-manifest.json` -- compact committed manifest; the GitHub
  watchdog reads its `finished_at`. Note it is committed mid-run, so its
  stages only reflect the run through precompute (push/deploy/verify read
  `pending`); final statuses live in `refresh_log` and
  `logs/refresh_manifest_<ts>.json`
- `backups/pre_run_<ts>.db` -- pre-run db snapshots (gitignored);
  `backups/recruiting_HEAD.db` is never pruned
- `docs/status.json` -- public-cover beacon. `source_capture` names the
  published source snapshot; `pipeline_status` and `pipeline_checked` report
  the latest known refresh health separately.
- `refresh_log` table in `data/recruiting.db` -- run history; status is one
  of `ok | degraded | failed | noop`. The Home status rail shows both capture
  freshness and the latest pipeline result; a failed validation says that the
  prior snapshot was retained.

## Recovery

Restore last night's data:

1. Copy the newest `backups/pre_run_<ts>.db` over `data/recruiting.db`
2. `Rscript scripts/precomputeDefaults.R` (precomputed defaults must match
   the db or the deployed app serves stale charts)
3. Commit both, push (Connect Cloud republishes), then
   `Rscript scripts/deployApp.R` for shinyapps.io

The audit + validate gates compare against the pre-run snapshot in
`backups/pre_run_<ts>.db` -- that file is the rollback point.

Stuck lock after a crash: delete `logs/refresh.lock` (the pipeline treats a
lock older than 3 hours as stale on its own).

Compare a suspect db against a snapshot at any time:
`Rscript scripts/validateRefresh.R backups/pre_run_<ts>.db`

## Confirmed active-cycle decommits

Do **not** loosen the 40% school-year retention gate to accept a plausible
roster move. After checking the current 247 class page, add one reviewed row to
`scripts/refresh-validation-exceptions.csv` only when all of these are known:
the exact table, school, active class year, baseline/live row counts, removed
commit, retained commit, source URL, review date, and a short expiration. The
validator requires every one of those fields to match. It never waives a
zero-row hole or a historical class. Remove the row once a successful baseline
has rolled forward, or when it expires and needs re-verification.

## New-cycle rollover (automatic)

Every night, after re-scraping the newest class year in the db, the
pipeline probes one year ahead (`MAX(Year)+1`, both sports) with
`refreshClassYear.R --allow-empty`. Until 247Sports opens those pages the
probe scrapes zero rows and exits 0 without touching the db -- no backup
CSV, no delete, no write. The night the pages go live, the probe writes the
first ahead-year rows, `MAX(Year)` advances, and the normal newest-cycle
scrape owns the new cycle from the next run onward. No manual seed needed.

One thing stays human-curated: the ERA coach rows. When a staff changes
(new head coach, new cycle attribution), update the coach-era tables by
hand -- the scraper only moves recruiting data, never coach attribution.

## Known risks

- **Laptop off = stale data.** The task cannot run a shut-down machine.
  `StartWhenAvailable` catches up at the next logon, and the weekly GitHub
  watchdog (`.github/workflows/canary-and-watchdog.yml`) opens a
  "Nightly refresh looks stale" issue once the committed manifest is more
  than 7 days old (which can also just mean a quiet no-change week).
- **OneDrive vs SQLite.** OneDrive can grab the db file mid-write. Keep the
  folder "Always keep on this device", do not pause/resume sync during the
  23:30 window, and if the db ever looks corrupt restore the newest
  `backups/pre_run_<ts>.db`.
- **247Sports blocking.** The scrape works from this residential IP. The
  weekly canary job records whether GitHub's datacenter IPs get 200s --
  evidence for (or against) ever moving the scrape to CI. Do not hammer:
  the scripts already rate-limit and retry politely.
- **Task identity.** The task runs as the logged-in user; a Windows password
  change or a rename of the repo folder means re-running
  `scripts\setupSchedule.ps1`.
