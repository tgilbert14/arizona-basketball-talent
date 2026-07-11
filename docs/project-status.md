# Project status — Big 12 Talent Lab (Girth Index)

> The single file the agent suite's cold-boot protocol reads before flagging or building.
> Owned by `triage`; reconciled on every `/questboard` sweep. Keep it honest — a stale
> status file makes cold-booted agents re-flag shipped or parked work.

**Current:** v7.0 (2026-07-02) — deployed to shinyapps.io (`Big-12-Talent-Pathways`, account `t-lama`)
and Posit Connect Cloud (git-backed). Stack: shinydashboard + ggplot2 4.0 + ggiraph + leaflet,
large inline vanilla-JS layer, SQLite `data/recruiting.db`.

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
