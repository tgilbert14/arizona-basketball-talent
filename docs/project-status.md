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
