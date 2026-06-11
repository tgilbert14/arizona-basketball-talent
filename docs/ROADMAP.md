# Roadmap — Big 12 Girth Index (v7 and beyond)

Written June 2026, grounded in a three-agent research pass (hosting, Shiny
performance, sports-dashboard UI). Guiding constraints, in order: **data
validity + transparency**, **UI/UX clarity** (don't overwhelm), then features.

---

## 0. URGENT — hosting (the ground is moving under us)

**shinyapps.io is shutting down December 31, 2026.** Posit is migrating
everyone to Posit Connect Cloud; existing URLs will auto-redirect, and
paid-plan transition pricing lands July 2026. Do **not** buy a shinyapps.io
paid tier.

| Option | Cost | Cold start | Verdict |
|---|---|---|---|
| **Posit Connect Cloud (free)** | $0 | similar to today, but 4GB RAM / 2 CPU vs our 1GB | **Do now** — forced by Dec 31 anyway; ~half a day |
| **Shinylive (webR/WASM) on GitHub Pages** | $0 forever | **zero** (runs in the browser) | **The endgame.** Every package we use (ggiraph, gdtools, RSQLite, leaflet, ggtext...) was verified to have a webR binary, and our db is read-only. Caveat: ~50–80MB first-visit download (then cached). ~1–3 days incl. phone QA |
| Hetzner VPS + shiny-server | ~$4.50/mo | none (always warm) | Best "real server"; ~1 day setup, ongoing admin |
| Azure App Service B1 / Container Apps (min-replica 1) | $13–18/mo | none with Always-On | Fine, but 2–3× the VPS for the same outcome |
| shinyapps.io paid tiers | $13–119/mo | still sleeps | **Skip** — platform dies in 6 months |

**Plan**: (1) migrate to Connect Cloud free this month; (2) prototype a
Shinylive export as a public "demo mirror" — if phone rendering is acceptable,
it becomes the primary public URL (instant load, no caps, $0).

**Today, free**: in the shinyapps dashboard, drop *Instance Idle Timeout*
from 15 → 5 minutes (idle time burns the 25 active-hours/month cap).

## 1. Performance ladder (impact ÷ effort, descending)

1. ✅ Disk render cache (was memory — wiped before most visitors benefited).
2. **Precompute the default view** (~1 day): visitors mostly see
   Arizona/football/last-5/defaults. Render those girafe objects to .rds at
   deploy time (incl. the phone-width variants) and serve instantly;
   everything else renders live as today.
3. **Static landing page that pre-wakes the app** (an afternoon): free
   GitHub Pages page with screenshots + a Launch button; `fetch()` pings the
   app on page load so the container is warm by the time they click.
4. **PNG-first charts on phones** (~1–2 days): render ragg PNGs with a
   "tap for interactive" swap; SVG parse time is the phone bottleneck.
5. **SVG diet** (hourly increments): interactive attributes only on layers
   that need tooltips; text axis labels instead of base64 logo images on
   the phone variant.
6. Startup trim (half day): precompute miles_away into the db at scrape
   time; drop scraper-only packages from the app's library list.
7. NOT worth it: bslib rewrite (zero speed gain), async/mirai (wrong
   bottleneck at our traffic).

## 2. UI showcase phases (research-backed sequence)

1. **Skeleton shimmer + boot screen** (half day): content-shaped
   placeholders during renders; kills the "is it broken?" feeling.
2. **Savant-style percentile rows** (~1 day): per-player/team percentile
   sliders in pure HTML/CSS (blue→grey→red, the Baseball Savant idiom) —
   instant render, replaces 2–4s SVG panels, instantly legible to fans.
3. ✅ **Holographic player card** (shipped v6) — extend with rarity tiers:
   plain card for 3★, foil for 90+, full holo for 95+ (the
   pokemon-cards-css three-layer technique; screenshot bait).
4. **Count-up numbers + hover lift + pressed states** (hours): the
   micro-interaction trio, GPU-composited only, behind
   prefers-reduced-motion.
5. **PFF-style table restraint** (hours): monochrome era report card with
   exactly one colored grade badge per row — credibility styling.
6. **Bento-grid Home** (2–4 days, optional pilot): CSS grid where tile size
   encodes priority; restacks to one column on phones with no breakpoints.
7. Dark-first theme (1–2 days, decide later): tokenized CSS variables;
   would also need ggplot theme retuning — only if the audience wants it.

## 3. Power-4 expansion (user-gated: core first — core is now done)

**UX rule: the Big 12 default experience must not change.** Expansion adds a
single "Conference" select to the control bar, nothing else.

- Phase A — data: add SEC/B1G/ACC rows to `TEAM_CONFIG` (slugs, logos,
  colors, states), run `refreshClassYear.R` + `scrapeRosters.R` over the new
  slugs (≈52 programs × 11 class years; expect a weekend of scraping with
  the existing per-school-replace safety rails), geocode + audit.
- Phase B — app: conference filter feeding every existing view; "vs
  conference" boards become "vs selected conference"; the quadrant gains a
  conference facet option. No new tabs.
- Phase C — the payoffs only this unlocks: true home-state retention (a
  Tucson kid leaving for Oregon is currently invisible), conference-vs-
  conference beef, national over/underachiever quadrant.
- Watch: memory (4× the rows — prep tables per-conference lazily), and the
  body-map cloud cap already protects render time.

## 4. Player-level outcomes (the credibility moonshot)

Grade classes on what players **became**, not how they were rated at 18.

- Phase A — data: CFBD `/draft/picks` (NFL draft) + season player usage
  endpoints; join by name+school+years with a confidence score on the match
  (name joins are the validity risk — publish match rates in Data & Notes).
- Phase B — metrics: per class = % drafted, % multi-year starters (snap
  proxy), All-conference count; per player = a simple Became tier
  (Draft / Starter / Contributor / Reserve / Departed).
- Phase C — UX (no new tabs): a "What they became" column on the era report
  card, a Became badge on the player card, and one new chart inside Coach
  Eras: rating-at-18 vs outcome tier (the "who evaluates best" chart).
- Transparency: every outcome metric gets an info modal with the join
  method, match rate, and a "name-match ≠ certainty" caveat.

## 5. Data validity standards (applies to everything above)

- Every chart states its player pool (done, v5) and its source.
- Every derived metric ships with an info modal: formula, source, caveat.
- Scrapes keep per-school replace + timestamped backups + the hole audit.
- Geocodes keep state-bbox validation; rejects go to backups for review.
- New joins (outcomes) must publish their match rates before the metric
  ships.

## Suggested order

1. Connect Cloud migration + idle-timeout setting (this month, forced).
2. Performance #2–3 + UI #1–2 (one focused week; biggest felt difference).
3. Shinylive prototype (the $0 zero-cold-start endgame).
4. Power-4 expansion (a weekend of scraping + a filter).
5. Player outcomes (the analyst-credibility moonshot).
