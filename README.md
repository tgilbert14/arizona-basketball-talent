# Power-4 Girth Index

The Power-4 Girth Index is an R Shiny research product for exploring how all
67 SEC, Big Ten, ACC, and Big 12 programs build football and basketball
rosters. It connects recruiting size, talent, geography, player development,
coaching identity, and football outcomes from 2016 through the current 2027
recruiting cycle.

Public cover: <https://girthindex.desertdatalab.com/>

## Product experience

One global lens controls the selected program, comparison program, sport,
class-year window, and player pool across the app. Views remain shareable by
URL, and every conference board is scoped and labeled from the selected
program's current league.

- **Home command center** — editorial product cover, Power-4 coverage ledger,
  sport-aware task routes, class snapshot, generated insights, searchable
  67-program browser, and a five-metric Program Fingerprint.
- **Size Lab** — interactive height × weight Body Map, position DNA, recruit
  cards, conference medians, and selected/compare-team highlighting.
- **Conference Beef** — team leaderboards for weight, height, pounds per inch,
  and BMI with position, source, and class-window controls.
- **Conference Lab** — distribution-first comparisons across all four Power
  conferences. Only recruiting inputs are eligible; geography and portal
  strategy metrics carry visible caveats.
- **Weight Room** — listed pounds added per high-school signee, development
  rankings, player transformations, and the measurement reality check between
  recruiting profiles and current rosters.
- **Coach Eras** — staff-by-staff changes in rating, blue-chip share, body
  profile, recruiting distance, in-state share, and position mix.
- **Defensive War Room** — football-only 3-3-5 roster fit, role depth,
  projected incoming bodies, retention, and an auto-generated analyst brief.
- **Talent vs Results** — football recruiting talent joined to season wins,
  SP+, expected performance, and Wins Above Talent.
- **Talent Origins** — a full-Power-4 state board, position hotbeds, and
  class-by-class source trends for unique HS/prep signees, with conservative
  JUCO/review exclusions, open-cycle flags, and accessible table twins.
- **Program Reach** — selected and comparison recruiting footprints,
  listed-origin miles to campus, outlier-aware class trends, position
  distributions, and the underlying player-level table.
- **Evidence tools** — accessible table twins for major boards, pinned player
  cards, direct 247Sports source links, exact-view URL sharing, contextual
  caveats, and export-ready chart downloads.

## Data and interpretation

Recruiting classes and roster measurements come from 247Sports; football
season records and SP+ come from CollegeFootballData. Reported heights and
weights are directional rather than certified measurements. Every analytical
view carries its player pool, sample context, and relevant limitations.

Talent Origins measures the last listed HS/prep school location among athletes
captured at the 67 Power-4 destinations. It is not birthplace, hometown, or a
census of every national prospect. Obvious JUCO and unreviewed College-name
sources are excluded from the default state view rather than silently guessed.

Conference comparisons are intentionally restricted to comparable recruiting
inputs. The app does not publish cross-conference win-rate, SP+, or Wins Above
Talent leaderboards.

## Project layout

| Path | Purpose |
|---|---|
| `app.R` | Shiny UI, server logic, routing, global state, and public methods copy |
| `R/functions.R` | libraries and shared query helpers, auto-sourced by Shiny |
| `R/team_config.R` | 67-program metadata, conference membership, logos, colors, and campus geography |
| `R/home_fingerprint.R` | pure-data and accessible HTML renderer for the Home Program Fingerprint |
| `R/coach_eras.R` | head-coach eras by recruiting class year |
| `R/girth_functions.R` | height parsing, size metrics, position groups, and data preparation |
| `R/girth_plots.R` | shared interactive plot and table-data builders |
| `R/talent_origins.R` | HS/prep source classification, state/position/trend frames, charts, and accessible tables |
| `www/girth-v9.css` | current application design system and responsive UI |
| `www/girth-v9.js` | progressive accessibility, search, navigation, and exact-view sharing |
| `docs/index.html` | public editorial cover page |
| `scripts/nightlyRefresh.R` | orchestrates recurring data refresh work |
| `scripts/refreshClassYear.R` | refreshes one class cycle across onboarded programs |
| `scripts/scrapeRosters.R` | refreshes current football or basketball rosters |
| `scripts/geocodeMissing.R` | validates and fills recruit geography |
| `scripts/fetchOutcomes.R` | refreshes football season outcomes from CFBD |
| `scripts/weeklyBrief.R` | generates the public Power-4 weekly recruiting brief |
| `scripts/validateTalentOrigins.R` | checks origin classification, dedupe, samples, trends, and table parity |
| `data/recruiting.db` | SQLite recruiting, roster, outcome, and refresh-log tables |

Prior interfaces remain in `app_v1.R` and `app_v2.R` for reference.

## Refreshing data

```sh
# run the orchestrated nightly refresh
Rscript scripts/nightlyRefresh.R

# refresh current football rosters
Rscript scripts/scrapeRosters.R football 2026

# refresh the current recruiting cycle
Rscript scripts/refreshClassYear.R football 2027

# rebuild the weekly public brief
Rscript scripts/weeklyBrief.R
```

## Product principles

- Program pages use one selected-team lens; aggregate pages state their full
  Power-4 scope explicitly.
- Distribution-first conference analysis; context metrics are not grades.
- Visible sample sizes, sources, and limitations.
- No hidden migration state: the shipped universe is all 67 Power-4 programs.
- Mobile, keyboard, reduced-motion, and screen-reader behavior are first-class
  product requirements.
