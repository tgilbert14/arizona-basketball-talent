# Power-4 Expansion — Design & Build Plan

> Big 12 Girth Index → Power-4 Girth Index. Expand from 16 Big 12 programs to ~67 Power-4
> programs (Big Ten 18, SEC 16, ACC 17, Big 12 16) and add conference-vs-conference comparison,
> without ever breaking the shipped Arizona/Big-12 experience. Grounded by a 9-agent design pass
> (2026-07-12); feasibility verified against live 247Sports + CFBD.

## User decisions (2026-07-12)

1. **Scope:** all 67 P4 teams, **onboarded one conference at a time** (SEC → Big Ten → ACC) behind an
   `onboarded` flag — each a clean revertable checkpoint; nothing half-populated ever renders.
2. **Outcomes:** **recruiting inputs only** for conference-vs-conference (247 rating, blue-chip share,
   measurables). Win%/SP+ stay strictly per-team — never a conference leaderboard (a conference plays
   itself → averages ~.500 by construction; SP+ uses recruiting as a prior → circular).
3. **UI home:** a dedicated **Conference Lab** tab (sibling to Conference Beef); the global Conference
   selector makes existing team boards conference-aware. No global reskin toggle.
4. **Hosting (default):** stay on the server worker with precomputed per-conference aggregates.
   Shinylive/webR remains the separate later endgame (shinyapps.io sunsets Dec 2026), NOT on this path.
5. **Sport parity (default):** football gets the Bud-Elliott blue-chip-ratio leaderboard; basketball
   uses the same distribution grammar without a BCR headline (BCR is football-native; hoops carries
   more internationals the geocoder skips → needs a footnote).

## Verified feasibility

- **CFBD `/teams/fbs`** (one authenticated call, live-verified): 67 P4 teams, 100%-populated
  conference + color + alternateColor + logos[] (ESPN 500px transparent PNG) + campus lat/long.
  This seeds conference, colors, logos (downloaded to `www/<slug>.png` at build time), and campus
  coords for the ~51 new teams. The existing 16 keep their hand-tuned hexes + verified slugs.
- **247 scraper:** `alabama`/`ohio-state`/`clemson`/`michigan` all fetched 200 and parsed clean with
  the current stride-8 parser — **no parser change**. Slugs are NOT name-derivable (UCF=`central-florida`
  already); a landing-page validator sweep gates onboarding, and a wrong slug fails SAFE (skips the team).
- **Nightly budget:** ~40-50 min steady-state at 67 teams (well under the 2h scheduler limit). The
  one-time historical backfill + uncapped geocode runs OFF the scheduler via the lock-holder.
- **Geocoder:** the state-bbox validator already covers all 50 states + DC — national recruits geocode
  with zero changes.

## Architecture

Conference is a **per-team scoping dimension, not a schema change** — team (slug) stays the grain in
every db table; `conference` becomes a real per-team column in `TEAM_CONFIG`. Because the default team
is `arizona` (conference "Big 12"), scoping every board to the active conference returns exactly today's
16 rows. At 68 rows the config moves from an inline literal to a checked-in `data/team_config.csv` with
a boot-time `stopifnot` guard; the standalone `CFBD_NAMES` map folds in as a `cfbd_name` column. The one
hand-maintained artifact is a 247 slug map validated by the landing-page sweep. Conf-vs-conf lives in the
Conference Lab tab with **distribution-first** grammar: the existing `conf_band` idiom (p25/p75 IQR +
median crossbar + jittered clickable dots) lifted from per-team to per-conference — one column per
conference, a clickable dot per member team, IQR box, median crossbar, mean only as a small diamond + n,
top/bottom team labeled. Every cross-conf mark is a **team-aggregate** (~68 dots), which satisfies both
honesty and the 1GB worker ceiling — cross-conf builders read a precomputed ~68-row aggregate table.

## Phased build order (each phase independently shippable)

- **Phase 0 — Conference Gate (still 16 teams). Size M.** Replace the recycled `conference="Big 12"`
  constant with a real per-team vector; add `CONF_CONFIG` (4 rows: Big-12-first order, Okabe-Ito
  aggregate colors, `conf_whole=2024`) + `team_conference()`; scope every board/median/rank/superlative
  to the team's conference before pooling; relabel every hardcoded "Big 12"/"conference median" caption
  to a dynamic `conf_label`; generalize the 5 Big-12-hardcoded guards as no-ops at 16 teams
  (fetchOutcomes GATE 3 `>=14/16` → `ceiling(0.875*n)`; CFBD_NAMES → `cfbd_name` column; validateRefresh
  roster gate → per-RosterYear w/ new-school growth exemption; `--conference/--slugs` scraper filters;
  CFBD campus fallback). Rename `big12_since` → `conf_since` with a back-compat shim.
  **Regression gate: force active_conf='Big 12' and diff every board against pre-change output —
  must be byte-identical.** That is the proof Phase 0 is safe. Risk: the layer is inert alone — one
  missed "Big 12" caption becomes a lie the instant a second conference lands, so the relabel sweep
  must be exhaustive and ship in the same change.
- **Phase 1 — Data Foundation (add 51 teams, still invisible). Size L.** `data/p4_slug_map.csv` from
  the landing-page validator; `scripts/buildTeamConfig.R` emits `data/team_config.csv` (68 rows: CFBD
  conference/colors/logos/campus, existing 16 preserved); an `onboarded` boolean (16=TRUE); an
  off-scheduler lock-holding `backfillConference.R` walks SEC → Big Ten → ACC, one league fully
  backfilled + validated + committed + `onboarded=TRUE` before the next; uncapped geocode + raised-cap
  (~800) hometown backfill as resumable jobs. Risk: the one-time backlog (25-40 min/conference of
  geocoding); a scheduled night colliding mid-backfill — mitigated by the lock + nightly-disabled window.
- **Phase 2 — Conference Lab + conf-vs-conf views. Size M.** New "Conference Lab" tab; global
  `Conference` selectInput as the leftmost SCOPE control (default "Big 12"); team pickers scope to the
  conference, `Compare to` stays full-P4 (Arizona-vs-Georgia works). Builders: `plot_conf_talent_spread()`
  (distribution primary), `plot_conf_leaderboard()` (blue-chip-ratio ranked, football), `plot_conf_era_timeline()`
  (four median lines, 2024-forward solid / pre-join ghosted); `group='conference'` modes on Position DNA
  + faceted Body Map. Metric selector is INPUT-ONLY (win%/SP+ physically absent). `g_conf()` in every
  bindCache key + the precompute gate.
- **Phase 3 — Perf/Scale Hardening. Size S-M.** Classes nightly for all onboarded; roster stage rotated
  one conference/weekday (Big 12 every night); ahead-year probe weekly. Precompute per-conference
  aggregate rds. Faceted Body Map downsamples per facet. Freshness badge splits recruiting vs roster.
  Risk: 1GB worker at ~4x data — mitigated by pre-aggregated reads + lazy per-conference expansion.

## Honesty guardrails (non-negotiable)

- **Metric-tier registry `CONF_COMPARE_POLICY` makes dishonest charts unbuildable:**
  - **GREEN** (head-to-head OK): avg 247 rating, blue-chip share (≥90), measurables (weight/height/BMI/
    lbs-per-inch) — a 92 is a 92 in any league.
  - **YELLOW** (only with a baked-in "reflects geography/strategy, not talent" caveat): in-state share,
    portal/transfer volume, roster retention.
  - **RED** (hard-refused as a conf leaderboard, axis physically absent): win% (zero-sum within a league),
    SP+ (circular), wins-above-talent (regression-to-the-mean ≠ coaching).
- **Distribution-first, always** — the mean is never a standalone bar; it is a diamond + n over a median
  crossbar, IQR box, member dots, and a labeled top/bottom team. Ranges overlap (a top-15 Big 12 class
  beats the SEC floor) and the chart must show it.
- **Realignment backcast rule** — every conference aggregate is over TODAY's membership; any class year
  before a member's join is a BACKCAST. Headline = 2024-forward (all four memberships real); pre-2024
  only as ghosted history with an n-chip of how many current members were actually in-conference (often
  0 for movers). Prevents double-counting Texas/OU on the SEC line and USC/Oregon/UW/UCLA on Big Ten.
- **Show the spread + name the outliers; always show n** (SEC 16 / Big Ten 18 / ACC 17 / Big 12 16).
  Standing **Pac-12 footnote** (collapsed 2024; members split across all four — no column here).
- **WAT/quadrant stays a within-conference residual** (fit scoped to the selected conference), never one
  national line ranked by mean residual.

## Top risks + mitigations

1. **1GB worker at ~4x data** (already OOM-killed Size Lab once) → cross-conf builders scan a precomputed
   ~68-row aggregate, never raw rows; lazy per-conference point expansion; per-facet body-map downsample.
2. **One-time backfill vs the 2h scheduler limit** → off-scheduler lock-holder, per conference, nightly
   disabled for the window. Steady-state levers: classes nightly, rosters rotated, ahead-year weekly.
   Do NOT cut politeness sleeps or parallelize 247 fetches.
3. **247 slug quirks (4 of 67 verified)** → landing-page validator sweep gates onboarding; `onboarded=FALSE`
   keeps unverified teams off every board until data lands.
4. **Cross-conference outcomes** → RED tier hard-refuses conf win%/SP+ at the choices level.
5. **Conference gate must land first** → Phase 0 ships + passes its byte-identical regression gate BEFORE
   any new-team row exists.
