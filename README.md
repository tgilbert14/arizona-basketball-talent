# big-12 talent lab

A Shiny application for exploring Big 12 football and basketball recruiting —
where recruits come from, how big they are, who develops them, and how every
head coach recruits differently.

## Features (v3)

**Global settings** (sidebar, apply to every tab): your team, a compare-to
team (secondary highlight on every board/trend), sport, and class-year range.

- **Home** — conference superlatives + your team's auto-generated talking
  points + logo quick-pick
- **Size Lab** — interactive height × weight *Body Map* (hover any commit),
  *Position DNA* vs the conference, size value boxes + beef rank
- **Conference Beef** — *Beef Board* leaderboards (weight / height /
  lbs-per-inch / BMI, position-group filters incl. a Trenches preset),
  size-over-time with coach-era marks, head-to-head weigh-in
- **Weight Room** — *pounds added per signee* (commit day → current roster),
  biggest player transformations, and the *Measurement Reality Check*
  (how much listed heights shrink between recruiting profile and roster)
- **Coach Eras** — era-shaded timelines (avg rating, blue-chip share, size,
  miles-from-home, % in-state), position-mix by coach, and an era report card
  with 4★+ counts and top signees (e.g. Brennan vs Fisch vs Sumlin vs RichRod
  at Arizona; classes attributed to the staff that ran the signing window)
- **Defensive War Room** (Analyst Brief) — built for Arizona DC Danny
  Gonzales' 3-3-5 odd stack: the *Fit Board* maps every defensive body to an
  odd-stack role (Nose / Rangy End / Edge Tweener / Stack LB / Hybrid S/LB /
  Safety / Corner) with two-deep targets and attrition, defensive bodies vs
  the league by role, plus roster construction, home-state retention, and an
  auto-written defense-first brief
- **Talent vs Results** — CFBD season records + SP+ joined to a rolling
  4-class talent composite: the over/underachiever quadrant (2016–2025) and a
  per-team scoreboard of wins vs talent on hand
- **Pin any card** — click any chart dot/bar to pin its hover card (player
  names, weights, 247 links) on screen; chart downloads are named
  `team-sport-chart-years.png`
- **Recruiting Map** — main + compare-team pipelines on one map in team
  colors, with state-footprint shading and player cards
- **Distance Lab** — the original distance-traveled scatter + box plot,
  restyled in team colors and driven by the global settings

Sidebar quick presets frame any view on **All years / Last 3 / the current
class**, and the Home page leads with a Class-of-2026 snapshot card
(size, rating ± vs prior classes, blue-chips, headliner).

Data scraped from 247Sports (recruiting classes 2016–2026 + current rosters).

## Project layout

| Path | Purpose |
|---|---|
| `app.R` | the Shiny app (v3) — `app_v1.R` / `app_v2.R` are prior versions |
| `R/functions.R` | libraries + shared query helpers (auto-sourced by Shiny) |
| `R/team_config.R` | team names, logos, colors, conference + home state |
| `R/coach_eras.R` | head-coach eras by recruiting class year (editable) |
| `R/girth_functions.R` | height parsing, BMI/lbs-per-inch, position groups |
| `R/girth_plots.R` | all plot builders (shared by app + scripts) |
| `scripts/girth_analysis.R` | renders the full insight pack to `insights/` |
| `scripts/scrapeRosters.R` | scrapes all 16 current rosters → `roster_*` tables |
| `scripts/refreshClassYear.R` | re-scrapes one class year (commits + transfers), safe per-school replace |
| `scripts/geocodeMissing.R` | geocodes new players with state-bounding-box validation |
| `scripts/auditRefreshHoles.R` | restores any school-year wiped by a failed fetch |
| `scripts/fetchOutcomes.R` | CFBD season records w/ quality gates (needs free API key) |
| `scripts/updatingSQLdatabase.R` | original recruiting-class scrape pipeline |
| `data/recruiting.db` | SQLite: `recruit_class_*` (commits + transfers), `roster_*` |

## Roadmap

The framework is built to scale: team vs conference (done) → team vs its own
coaching eras (done) → all Power conferences (add rows to `TEAM_CONFIG`,
re-run the scrapers) → conference vs conference fight cards.

## Refreshing data

```sh
# current rosters (football or basketball, any season year)
Rscript scripts/scrapeRosters.R football 2026

# regenerate the static insight pack (PNGs + talking points)
Rscript scripts/girth_analysis.R
```
