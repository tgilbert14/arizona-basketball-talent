# Project status — Big 12 Talent Lab (Girth Index)

> The single file the agent suite's cold-boot protocol reads before flagging or building.
> Owned by `triage`; reconciled on every `/questboard` sweep. Keep it honest — a stale
> status file makes cold-booted agents re-flag shipped or parked work.

**Current:** v7.0 (2026-07-02) — deployed to shinyapps.io (`Big-12-Talent-Pathways`, account `t-lama`)
and Posit Connect Cloud (git-backed). Stack: shinydashboard + ggplot2 4.0 + ggiraph + leaflet,
large inline vanilla-JS layer, SQLite `data/recruiting.db`.

## ✅ POWER-4 EXPANSION COMPLETE (2026-07-18) — all 67 teams live

SEC, Big Ten, and ACC onboarded conference-by-conference (16→67 teams: Big 12 16 + SEC 16 + Big Ten
18 + ACC 17). Live + verified on Connect Cloud (picker=67, all four conferences pool correctly:
Alabama→SEC, Michigan→Big Ten, Clemson→ACC boards, 0 errors). shinyapps rides tonight's nightly.

**Full-dataset audit (clean bill of health):** DB integrity `ok`; 17,296 football + 2,386 basketball
commits (~248–274/team, balanced across conferences); **zero** duplicate keys; **zero** zero-commit
team-years; geocode 99.3% football / 95.5% basketball (unmapped = internationals); MAX(Year)=2027,
zero rows beyond (cycle-cap holds); rosters 7,322 fb + 970 bb across 67 schools; 670 team-seasons.
**External-validity proof the conference attribution is correct:** blue-chip share ranks SEC 44.5% >
Big Ten 28.3% > ACC 17.6% > Big 12 12.1% — the exact real-world recruiting hierarchy. QC ledger:
0 open, 58 accepted (verified-legit outliers incl. USC/Wash 2022 transition classes, Colorado 2024
Prime portal year, Marcis Ponder 6-11/328 FSU center, 54 internationals), 149 auto-resolved.

**Nightly is Power-4-ready:** roster success floor scales (`ceiling(0.75 * n_teams)` = 51/67), the
validate roster gate is growth-aware (baseline-schools-scoped), the QC sweep runs non-blocking as
S3.5. Lock released; fires at 23:30.

**Phase 1 pipeline fixes shipped en route:** `backfillConference.R` shQuotes run_child args (repo-path
space split the validate baseline); `validateRefresh.R` roster gate scoped to baseline schools
(conference onboarding doubles a current-season RosterYear = legit growth); `qcSweep.R` refreshes
severity on re-observed flags (tukey's exam find).

**REBRAND DONE (commit `9d694dd`):** "Big 12 Girth Index" → **"Power-4 Girth Index"** (app header +
Home hero + PNG export name; landing title/og/hero/brief → "all 67 SEC, Big Ten, ACC & Big 12
programs"; disclaimer generalized; og card regenerated). Conference-aware DATA labels (via
`conf_label`) untouched — a team's boards still name its own conference. Verified live.

**PHASE 2 CONFERENCE LAB DONE (commit `e3b8916`, verified live locally):** new "Conference Lab" tab
comparing all four leagues distribution-first. `CONF_CONFIG` → 4 rows (Okabe-Ito); `CONF_COMPARE_POLICY`
metric-tier registry (GREEN rating/blue-chip/weight/lbs-in rank head-to-head · YELLOW in-state/transfer
carry a "geography/strategy, not talent" caveat · RED win%/SP+/WAT never in the selector — unbuildable);
`conf_spread_data()` (~67 team-aggregate dots, 1GB-safe, realignment-honest — a team counts only years
it was in its current league, wide windows disclose dropped backcast rows); `plot_conf_talent_spread()`
(spread per league: dots + IQR box + median crossbar + mean diamond+n + top/bottom named); table twin +
info modal. Verified: SEC 88.9 > B1G 87.8 > ACC 87.1 > B12 86.6 avg rating; blue-chip SEC 37 > B1G 24 >
ACC 15 > B12 10; YELLOW caveat fires; RED absent; no regressions.

**TEAM PICKER REBUILT (commit `062a0c8`, live):** both g_team/g_compare are now `selectizeInput` with
conference optgroups (Big 12 first, alphabetized within), per-team logos in the dropdown + selected
item, type-to-search, `maxOptions=100` (67 > selectize's default 50 cap), and `allowEmptyOption` for
compare's "— none —". First-visit modal + Home "Pick your team" grid grouped into conference sections
(was a flat 67-logo wall). Helpers `conf_grouped_choices`/`gi_logo_map_js`/`gi_picker_render` ~app.R:110.

**UI/UX SWEEP (Alyssa audit → commit `1de1e73`, live):** amber caveat banner for YELLOW context metrics
(surfaces "reads geography, not talent" above the chart); the active team's dot ringed in gold in the
67-dot Lab so you find yourself; conference color language (Okabe-Ito per league) unified across picker
headers + Home grid + Lab chart from CONF_CONFIG; dynamic conference counts in the honesty modal;
Conference Lab empty state names the fix; picker item title tooltips; first-visit "Skip" link; ⓘ glyph
+ stale "16 Big 12" comment cleaned. All verified live, 0 errors, no regressions.

**Remaining polish (nice-to-have, not blocking):** the global Conference SCOPE selector for the OTHER
tabs' team pickers (the Lab itself is self-contained); `plot_conf_era_timeline` (4 median lines);
`group='conference'` modes on Position DNA + faceted Body Map; a client-side text filter on the
first-visit modal (Alyssa #6 — the bar picker already searches, so lower value); widen picker columns
on tablet (title tooltips ship now as the cheaper fix). Phase 3 = perf/scale hardening (roster stage
rotated by conference; per-conference aggregate rds).

## Shipped 2026-07-16 AM: the cycle-cap fix (2028 preview-year leak) — commit a4455a5

The v8 year-ahead probe scraped MAX(Year)+1 uncapped; 247 lists early commits two cycles out, so
once 2027 seeded, the probe rolled to 2028 and wrote 3 real class-of-2028 commits (incl. Arizona's
Karmello Calloway) — stretching the slider/default window/previews to 2028 and making the nightly
treat 2028 as "the current cycle" (2027 went stale for a day). **Rule: the app tracks at most ONE
cycle ahead of the calendar (calendar+1).** Fixes: calendar+1 cap in nightlyRefresh/refreshAll
(`newest_year()` + skip beyond-ceiling probes), hard refuse-guard in refreshClassYear, `CYCLE_CAP`
WHERE-filter on app.R startup loads (defense in depth), weeklyBrief coverage-CONTRACTION mirror
(a retired class year discloses as coverage change, never as decommits), 3 rows deleted (snapshot
`backups/pre_2028fix_*`), precompute rebuilt on 2024–2027, manifest updated. Verified live on
Connect Cloud (slider max 2027, window 2024–2027); shinyapps picks it up on the next nightly deploy
(or run `Rscript scripts/deployApp.R` by hand). The 2028 class re-enters naturally in Jan 2027.

## DONE 2026-07-17: SEC ONBOARDED (16/16) + the QC sweep & ledger

**SEC is live in the config** (commit `4fef15c` + follow-ups): all 16 teams cleared the 20-row floor,
validate passed, picker shows 32, Alabama smoke = SEC-pooled boards, zero errors. Two bugs fixed en
route: (a) `backfillConference.R` run_child now shQuotes args — the repo path's space split the
baseline arg and failed the validate stage at the last gate; (b) `validateRefresh.R` roster
per-RosterYear gate now compares BASELINE SCHOOLS only (rosters are current-season, so conference
onboarding doubles the same year's totals — 1729→3487 was legit growth, not corruption).

**QC sweep ran (user-directed) — `scripts/qcSweep.R` + the `qc_flags` ledger, now wired into the
nightly as S3.5** (mark + safe-fix every auto-update; `accepted` flags never re-raise; new
high-severity flags exit 2 → manifest note). Findings triaged, 201 flags → 0 open high:
- REAL fixes: 2 Cincinnati players' State TX→OH (247 team-page bug, profiles verified "Dayton, OH");
  Dillon Sykes (utah 2027) wrong CA pin → NULLed (nightly re-geocodes state-pinned); Jeremiah
  Davenport (cincinnati bb 2019) pin was in SINGAPORE → NULLed; **arizona 2018 football class was a
  2-row hole → re-scraped to 20 commits + geocoded**; 10 placeholder Weight-0/Height-0-0 → NULL
  (auto-fix now permanent in the sweep).
- ACCEPTED (verified legit, ledger remembers): 54 internationals correctly pinned abroad (ProKick
  punters, German/Nordic linemen, Pago Pago, NBA Academy Mexico); colorado 2024's 9-commit class
  (Prime's portal year). 53 SEC 2019-20 grad transfers verified REAL (Cade Mays, Brenton Cox) —
  TYPE-ERA cutoff moved to <2019, app's transfers ctx_note reworded.
- Rule refinements: GEO-COLLAPSE counts distinct CITIES (city-granularity pins are by design);
  GEO-BOUNDS international-aware + AS/PR/GU/VI territory bboxes; new STATE-CONFLICT rule catches
  the team-page wrong-state pattern even when the pin agrees with the wrong side.

**DONE: Big Ten ONBOARDED (18/18, pushed `12a8f02`, verified live on Connect Cloud).** 50 teams now
onboarded (16 Big 12 + 16 SEC + 18 Big Ten); only ACC's 17 hidden. Validate passed first try (the
SEC growth-aware roster-gate fix held). QC sweep on the fresh data: 2 thin-class flags (USC 2022,
Washington 2022 — both verified REAL portal-era transition classes, Riley/DeBoer hired Nov 2021,
neighbor years full; accepted in the ledger). Live checks: Michigan Home hero reads "biggest boys in
the Big Ten" + "#16 of 18"; Beef Board titled "Big Ten Beef Board — Big Ten avg 228 lbs", 18-team
pool, 0 errors. Alabama (SEC) confirmed prior. Ledger: 0 open, 57 accepted, 149 fixed.

**IN FLIGHT: ACC backfill (PID 9204, detached, monitored)** — final conference (17 teams); the
`smu`/`stanford`/`north-carolina-state` slugs the map flagged all scrape clean. On DONE → 67/67
onboarded → Phase 2 Conference Lab.

**⚑ FOR THE USER — branding decision (not blocking):** the conference-aware DATA routing is flawless
across all confs, but the app's brand title is still "Big 12 Girth Index" and the GitHub-Pages
landing page (`docs/index.html`) still says "all 16 programs" / "Big 12" throughout — both now
misnomers when viewing a Big Ten/SEC/ACC team. A Power-4 rebrand (title + landing copy + og card)
is a Phase-2 identity call left to the user; the app FUNCTIONS correctly under the current name.

**Fleet training shipped (2026-07-17, canonical repo commit `fea2336`):** the app-timeout + geo-QC
lessons taught to the agent fleet. connor +42 (Connect Cloud idle-sleep/wake-race doctrine + fix
ladder) and tukey +34 ("far but real vs wrong" geo-outlier doctrine) both LICENSED WITH HONORS on
live exams — tukey's exam found + fixed a real severity-refresh bug in `qcSweep.R` (app commit
`d833d20`). Curriculum also landed in the caravan + bowerstone playbooks (+89) and 5 LESSONS lines.

## QUEUED (user-directed 2026-07-16): post-backfill QC sweep + nightly QC ledger

After each conference lands: **(a) one-time QC sweep** — geo-outlier triage that separates "far but
real" (a Florida/Hawaii recruit at Arizona is legit recruiting) from "wrong" (geocoded pin's state ≠
hometown text's state, out-of-US-bounds pins, many rows collapsing to one coordinate = Nominatim
state-centroid failure, 0-mile distance on an out-of-state hometown), with flagged cases verified
against their 247 ProfileUrl; plus completeness (per-school-per-year commit floors ~15-30 fb),
duplicates, NA rates, height/weight range sanity, transfers-2021+-only, conference-label consistency,
and distribution sanity (SEC blue-chip rates should look SEC-like). **(b) productize into the
nightly** — a `qc_flags` ledger table (rule, row key, severity, details, first_seen, status) + a QC
stage after geocode: `accepted` flags (verified-legit outliers) never re-flag; safe wrongness
auto-fixes (state-pinned re-geocode; still wrong → NULL coords = honestly unmapped); new
high-severity flags surface in the run manifest notes + the gh-issue alert path. Owner: caravan/maze
pattern; the point is mark-or-fix on every auto-update, with memory, no alarm fatigue.

## Power-4 expansion plan (paused 2026-07-12, resumed 2026-07-16)

Adding Big Ten + SEC + ACC (16 -> ~67 teams) + a conference-vs-conference "Conference Lab".
**Full plan: [docs/p4-expansion-design.md](p4-expansion-design.md).** User decisions locked: all 67 teams
onboarded conference-by-conference; recruiting-inputs-only comparison (NO conf win%/SP+); dedicated
Conference Lab tab; stay on the server worker (Shinylive is the separate endgame); football gets the
blue-chip-ratio leaderboard, basketball distributions-only.

**DONE + pushed to main:**
- **Phase 0 — the conference gate (`1e63648`).** Conference is a per-team scoping dimension; every
  "Big 12" DATA label routes through `conf_label()`; `conf_slugs(team_conference(team))` is the pooling
  scope. PROVEN byte-identical vs HEAD (a harness rebuilt 46 board signatures old-vs-new = same md5).
  New helpers in R/team_config.R: `team_conference`, `conf_slugs`, `conf_label`, `CONF_CONFIG`,
  `conf_since` (+ `team_big12_since` back-compat shim). Scrapers gained `--conference`/`--slugs` flags.

**DONE + committed — Phase 1 data foundation (config only; teams present but HIDDEN):**
- `data/team_config.csv` = **67 rows** (16 existing Big 12 preserved EXACTLY — colors + conf_since
  verified — plus 51 new ACC/Big Ten/SEC), only the **16 Big 12 onboarded=TRUE**; 51 new logos in
  `www/`; `data/p4_slug_map.csv` (all 67 247 slugs swept). R/team_config.R now loads TEAM_CONFIG from
  the CSV (inline 16 kept as fallback); `onboarded_slugs()` scopes the display universe to the 16, so
  the 51 new teams are present-but-hidden and the 16-team UX is byte-identical (verified: 16 onboarded,
  hidden teams don't leak the pooling scope, colors preserved). New scripts: `validateSlugs.R`,
  `buildTeamConfig.R`, `backfillConference.R` (the lock-holding per-conf backfill orchestrator, NOT yet
  run). Committed after the resume-point commit.
- **VERIFIED 2026-07-16:** slug map clean (all 67 slugs HTTP 200, zero needs-review — the
  `ole-miss`/`nc-state`/`texas-am` worries didn't materialize); config smoke (67 rows, 16 onboarded,
  hidden teams absent from the picker universe); app booted + boards rendered locally off the CSV
  config (plus three nightly deploys since 941c22b ran through it live); `backfillConference.R`
  read end-to-end — sound (lock heartbeat, per-stage checkpoints, validate-vs-snapshot, onboard
  flip LAST, 20-row floor, calendar-capped ahead probe).

**NEXT (not started) — Phase 1 finish (the historical backfill):**
- Run `scripts/backfillConference.R` for **SEC first** (then Big Ten, then ACC) as a long MONITORED
  background job — it holds `logs/refresh.lock` so the nightly stands down safely. ~40 min scrape +
  25-40 min geocode PER conference. Flip each league's `onboarded=TRUE` on success, commit per league.
  Fix any needs-review slugs first.
- **Phase 2:** the Conference Lab tab + `plot_conf_talent_spread` / `plot_conf_leaderboard` (distribution-
  first; the `CONF_COMPARE_POLICY` GREEN/YELLOW/RED metric-tier registry; realignment backcast rule).
- **Phase 3:** perf/nightly scaling (roster stage rotated by conference; per-conference aggregate rds).

**Note:** the nightly 23:30 pipeline still runs on the 16 onboarded teams throughout — safe + unaffected
until a conference is onboarded. Two spawned-task chips (wrapper silent-death, push escalation) were
already FIXED this session (commits f57dd35, 8ed57a5).

## Shipped in v7.0 (the review-driven hardening — committed 26b9cdf)

A 52-agent review produced 65 verified findings; the golden-dozen high-impact fixes are live:
- Compare-aware precompute gate (`at_defaults` checks the compare team + reads debounced years — was serving the wrong cached chart).
- `highlight_colors()` near-white luminance guard (compare team could render invisible).
- Pin lifecycle: clear on chart re-render (per-box) and on box collapse; pointer-cancel leak fixed; drag/spawn clamped to box bounds.
- Capture pipeline: strips pin chrome from exports; html-to-image vendored locally with CDN fallback + guarded toasts.
- iPhone faux-fullscreen fallback; holo card dialog semantics + `prefers-reduced-motion`.
- Honesty: school-scoped player-card lookups, n-chips in receipts tooltips, pc_link on era/map/335/roster tips, team-level trend band, "additions" not "signees" copy.
- Contrast + touch-target pass. Precomputed defaults rebuilt.

## LIVE 2026-07-11 (the nightly auto-refresh)

The unattended data pipeline is shipped, scheduled, and verified on both hosts. First supervised run
went green (22/22 sanity checks, 1 hole auto-healed, shinyapps deployed+verified); the only failure was
S7 push on a missing git identity — fixed repo-local, then the publish completed manually (commit dcca055)
and the Connect Cloud badge reads "data updated Jul 11, 2026". Task Scheduler is registered (23:30 daily).
Also fixed en route: `manifest.json` had drifted since v6.1 — Connect Cloud deploys from the committed
manifest, so stale checksums + 5 unlisted runtime files (beef/era precomputes + vendored html-to-image)
meant Connect Cloud was serving an incomplete/old bundle. New `scripts/updateManifest.R` (files-section
only, packages preserved) is now wired into the pipeline. Details:
- `scripts/nightlyRefresh.R` (S0–S10 orchestrator: lock → snapshot → scrape → geocode → audit+validate
  w/ rollback → refresh_log ledger → hash-gated precompute → scoped commit/push → shinyapps deploy →
  verify-live → manifest + gh-issue alert), `scripts/lib/refresh_utils.R`, `scripts/validateRefresh.R`,
  `scripts/deployApp.R`, `scripts/runNightly.ps1`, `scripts/setupSchedule.ps1` (Task Scheduler 23:30),
  `.github/workflows/canary-and-watchdog.yml`, `.rscignore`, `docs/auto-refresh-runbook.md`.
- Hardened: scrapeRosters (retry/backup/per-team replace/12-of-16 gate/row floor/transaction),
  refreshClassYear (plausibility demotion gate w/ fractional heights, transfer-wipe guard, transaction),
  fetchOutcomes (dynamic years, per-year replace, honest exit), auditRefreshHoles (baseline arg +
  git-HEAD regen + Type-symmetric counts + idempotent heal — the transfer-duplication bug is FIXED),
  refreshAll (honest exit code), app.R ("data updated" badge from the new refresh_log table).
- Scheduler decision: local Task Scheduler (247 verified from this machine; datacenter IPs unproven —
  the canary Action gathers evidence for a future migration).
- Follow-ups still worth doing (from the NEON refresh-data.yml reference): a post-deploy smoke that
  opens a GitHub issue on a live-outage (ours S9-verify only WARNs), and a commit message that names
  what actually changed (freshness marker) rather than a static "[auto]" line.

## Open quests (ranked)

### P1 — accessibility floor (keyboard reach)
The flagship interaction chain (girafe dot → tooltip → pin → player card) is pointer-only; no chart
element is keyboard-focusable (WCAG 2.1.1 Level A). **Owner:** `whisper` / `/fable polish`.
**Fix:** table-twin ("view the numbers") toggle on every chart box as the honest fallback, plus a
keyboard path to the pin action.

### P2 — render robustness + cold-start
- ✅ DONE (2026-07-03) — `girafe_try()` now wraps ALL 18 girafe renders (was 4); a cold-start
  font/memory hiccup degrades to the calm "nudge a control to reload" chart instead of a scary
  sanitized error / apparent outage. Verified live via preview: 0 error banners, live + precomputed
  paths both render.
- ✅ DONE (2026-07-03) — precompute pack extended to beef_board + era_timeline (desktop + phone),
  so all four default-hit views (body_map, dna, beef_board, era_timeline) serve from `.rds` on a
  cold container instead of a fragile SVG build. PRE-serve branches gated on the sub-input defaults
  (beef: AvgWeight/All/commits/vs-ASU; era: AvgRating, year-window-independent). Verified via preview
  logs ("serving precomputed beef_board_* / era_timeline_*"). **Re-run `scripts/precomputeDefaults.R`
  after every data/tooltip change** or the defaults show stale data.
- Missing `bindCache` on class_retention / talent_quadrant / team_scoreboard (recompute conference-wide
  work every flip). **Owner:** `hammer`.

### P2 — honesty depth
- Era timeline compares structurally different pools (transfers 0% of 2016–20 vs ~55% of 2026) under the
  default "commits + transfers" — partly measures portal volume, not coaching. **Owner:** `tales` / `hobson`.
  **Fix:** split-pool line (solid HS commits + dashed additions).
- Beef Board shows per-team means with no per-team n (siblings print it). **Owner:** `tales`. **Fix:** n-chips.

### P3 — code hygiene / showcase
- Extract ~720 lines of static CSS/JS from `app.R` (2,600+ lines) to `www/` (26 JS lines carry R escapes — not a verbatim move). **Owner:** `bowerstone` / `page`.
- No `.rscignore` — the shinyapps bundle ships ~10MB of backups/insights/scripts. **Owner:** `caravan` / `ship`.
- Showcase wow-list (from the review): one-tap 1200×630 share card, tale-of-the-tape versus poster, bump/slope rank-evolution chart, deep-link/bookmarkable state, kiosk mode, watchlist tray. **Owner:** `fable` + `bowerstone`.

### Shipped 2026-07-11 evening (wave 2 — the last of the verified sweep backlog)

- **Wins Above Talent ladder** on Talent vs Results: quasibinomial league talent-to-wins fit,
  expected vs actual wins/season, table twin + info modal (window-aware caveat: the default is ~2
  completed seasons, so one season moves a program a lot — widen for a stabler read). Scoreboard frozen.
- **URL deep links**: the five global controls + active tab serialize to the query string and hydrate
  on load (whitelisted; forged params fall back silently; takes precedence over localStorage).
- **Ranked insight engine** on Home: top-3 by notability score, n-gated (MIN_INSIGHT_N=8), capped at
  the arriving class so a seeded 2027 can't headline.
- **Realignment-honest baselines**: backcast disclosure on era band / body map / quadrant when the
  window spans 2024.
- **Join suffix fix** (kills "Troy Ford Jr." false departures, 0 regressions) + honest scope notes
  (retention drops the redundant "join quality" line; weight-room reads "gains cover X of Y still on a
  roster; the rest graduated/transferred/turned pro" — never a scrape-failure score).
- **Freshness remainder**: ScrapedAt on recruit tables, docs/status.json (data-dated, not regen-dated)
  + landing "data updated" line, OG social card, copy-brief provenance footer, girafe_try auto-retry.
- Pipeline: chronic-push escalation (S0 ls-remote probe + 2nd-consecutive-miss → failed) and the
  wrapper silent-death fix (version-glob Rscript + last-ditch alert) — both from ship's license exam.

### Shipped 2026-07-11 PM (the v8 feature wave — sweep items 1-4 + year-ahead)

- **Table-twin toggles** on beef board / class retention / weight-room board / talent quadrant:
  single-source `*_data()` builders, savant rank rows (quadrant uses a neutral navy ramp), scoped
  captions, full keyboard/aria semantics, visibility-based swap (P1 a11y CLEARED for the boards).
- **247 ProfileUrls** captured for classes 2023-2027 (100% coverage); exact player links everywhere;
  **1,634 transfer hometowns backfilled** from profiles + geocoded (1,480 accepted, 0 bbox rejects;
  ~66 internationals honestly unmapped) — transfers now render on the Distance Lab + Recruiting Map.
- **What-changed-since-last-visit strip** (server-computed snapshot handshake, context-mismatch guard)
  + **auto-published weekly brief** at /brief/ (name-drift collapse, coverage-vs-activity disclosure).
- **Quick wins**: hover spotlight on all 18 charts (opts_hover_inv + nearest), branded disconnect card,
  era split-pool dashed overlay + pool-split tooltips + pool-neutral legend, beef n-chips.
- **Year-ahead**: nightly scrapes MAX(Year)+1 with --allow-empty (self-governing rollover — the Dec
  manual seed is dead); **2027 seeded** (271 fb + 7 bb commits); dynamic ERA_MAX_CLASS; war-room +
  retention pinned to the arriving class (class of N enrolls fall N).
- Suite training: the U.A. "Girth v8 patterns" class landed these patterns in connor/cyrus/ship +
  the four guild playbooks + 15 LESSONS lines (TG-Data-Apps a47d6a5).

### Verified backlog from the 2026-07-11 beef-up sweep (29-agent, all ideas adversarially verified)

High-impact, ranked by impact-per-effort (details in the sweep transcript; each is grounded in real code):
1. **Table-twin "view the numbers" toggle** rendered as Savant-style rank rows — clears the P1 a11y item AND a showcase visual in one build (L).
2. **247 profile hrefs as stable player key** — exact profile links for transfers, better joins, AND unlocks transfer hometowns → transfer geocoding for Distance Lab/Map (M; needs ALTER TABLE first, schema-align drops unknown cols).
3. **What-changed-since-your-last-visit panel** on Home (localStorage snapshot → class_snapshot delta, pool-honest) (M).
4. **URL state for the control bar + per-team OG share cards** — deep links, unfurl-ready (M).
5. **Ranked n-gated insight engine** replacing fixed-order talking points (M).
6. **Era-timeline honesty overhaul**: split-pool dashed HS-only overlay + pool-split tooltips (M; clears P2 honesty item).
7. **Wins Above Talent ladder** — quasibinomial wins~talent with honest intervals (M; scoreboard stays frozen).
8. **Auto-published "what changed this week" brief** on the GitHub Pages landing site, diffed from the nightly backups (M; rides the new pipeline).
9. **Hover ergonomics pack** in girafe_build: opts_hover_inv dim-the-rest + nearest-hover for touch (S — one edit upgrades all 18 charts).
10. **Degraded-state pack**: branded shiny:disconnected overlay + retry affordance (S).
11. **Freshness everywhere**: ScrapedAt on recruit tables, live prose dates, status.json on landing (S; badge already shipped w/ pipeline).
12. **Realignment-honest baselines**: membership-aware bands w/ backcast disclosure on body map/era/quadrant (S).
13. **Published join-match rates + fuzzy-miss sensitivity** for Weight Room/retention name joins (M; 4-6 confirmed false departures today, ~2pp worst-school error).
14. **Power-4 rivals pack** (6-8 marquee programs) — REQUIRES the conference-gate work first or SEC rows corrupt every Big 12 board (L).

## Deliberately deferred (do NOT re-flag)

- **shinydashboard + inline JS is the house stack** — no framework/bslib migration.
- **No emoji / cheesy copy** in the UI (v6.6 de-cheese pass is canon).
- **The scoreboard chart design is frozen** by the user.
- **Year window deliberately does not apply to roster snapshots** (explained in captions).
- **Blue-chip = 247 team-page rating ≥ 90**; the 1–2 player/class discrepancy vs Composite is definitional + documented in Data & Notes.
- ggplot2 4.0 S7 breakage (ggtext in complete themes; interactive legend keys) — worked around, known.

## Hosting note

shinyapps.io sunsets **Dec 31 2026** → Connect Cloud is the migration target (already live);
Shinylive/webR is the roadmap endgame (all packages have wasm binaries; db is read-only). See `docs/ROADMAP.md`.
