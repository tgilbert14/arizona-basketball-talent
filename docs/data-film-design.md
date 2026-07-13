# The Data-Film — "The Conference, From Above" (design doc, v0.1)

_2026-07-13 · Fable-frontier project #3 from the distillation session. Status: DESIGN —
build in a fresh session via the `scroll-film` skill once this doc is approved._

## The one feeling
Vertigo at scale: *you are flying over the actual data* — every landform is a true number.

## Concept
The P4/Big 12 talent landscape as ONE continuous camera flight (scroll-film pipeline,
canvas player), where the terrain itself is generated FROM `data/recruiting.db`:

- **Team = mesa.** Footprint from conference/geography (p4_slug_map), **height = talent
  mass** (beef-board metric), **strata colors = coach eras** (era_timeline — each era a
  visible rock layer), surface texture from team accents (team_config.csv).
- **Wins Above Talent = the spires.** Teams that outperform their talent grow impossible
  hoodoo spires above their mesa; underperformers erode below their rim. The WAT story
  IS the landscape drama.
- **The flight** follows the season's narrative (lindir brief over kalin's real deltas):
  conference overview at dawn → the tallest mesas (talent lords) → down the era canyon
  (one program's strata up close — Arizona's) → the WAT spires at golden hour (the
  overperformers) → finale on Arizona's mesa under stadium lights.

## The technique that makes it honest (and novel)
**Data-true terrain, AI-cinematic motion:**
1. **R renders the truth.** `rayshader` (house R stack) builds a literal heightmap from
   recruiting.db metrics → photoreal-graded terrain STILLS (sun, haze, palette) that are
   *quantitatively correct* — every mesa height is the real number, exportable with a
   scale legend.
2. **AI animates the camera, not the facts.** Each rayshader still becomes the
   conditioning frame for a Seedance flight clip (scroll-film Stage 3, dual-pin seams).
   The motion model adds atmosphere/parallax but the landforms it flies over are ours.
3. **Charts land at the dips.** At each scene's linger point, a live chart overlay
   (garth/ggiraph or vera/ECharts, rendered into the copy layer — NOT baked into video)
   fades in and pins the number the terrain just showed: the beef board at the mesas,
   the era timeline in the canyon, the WAT scatter at the spires. Film carries feeling;
   charts carry proof. Interactive on hover, dismissed on scroll.
4. **Honesty gates.** hobson + tukey sign off: terrain scaling documented on-page
   ("mesa height ∝ 247 composite mass, 2016–2026"), no log-scale theatrics without a
   label, WAT methodology linked. The film is a CHART and must obey chart law.

## Architecture
```
recruiting.db ──R script──> heightmap matrix ──rayshader──> graded terrain stills (data-true)
                                   │                              │ (conditioning frames)
kalin deltas ──lindir──> story-brief.json ──gates──> gwaihir prompts ──MCP──> flight clips
                                                                       │
                                             narvi: scd=none 60fps → xfade bake → 1 master
                                                                       │
                    canvas player (Old Pueblo v4 pattern) + CHART OVERLAY LAYER (new)
                    — overlays mount at linger points, driven by the same film-time map
```
New doctrine this project will mint (casebook entries expected): rayshader→Seedance
conditioning fidelity (how much the motion model respects data terrain — measure with
check_seams SSIM against re-rendered ground truth), chart-overlay timing grammar,
data-labeling law for cinematic charts.

## Scenes (sketch — lindir owns the real brief)
| # | Scene | Data | Overlay chart |
|---|---|---|---|
| 1 | Dawn overview — the whole conference range | all-team beef board | none (cold open promises the spires) |
| 2 | The talent lords — tallest mesas up close | top-5 talent mass | beef board top-10 |
| 3 | The era canyon — Arizona's strata | era_timeline | era timeline chart |
| 4 | The WAT spires (the dip→reveal: fog parts) | wins above talent | WAT scatter, ranked insights |
| 5 | Arizona's mesa at night, stadium lights | the brand beat | CTA + methodology link |

## Cost & effort
~5 stills (rayshader = $0) + 5 dives (90) + 4 conns (45) ≈ **630 credits** premium
(or ~210 budget tier) + ~2 sessions (1: R terrain pipeline + brief + prompts through
gates; 2: generate, bake, player + overlay layer, QA, deploy). Needs a credit top-up.

## Risks
- rayshader stills may read "render-y" → Seedance photoreal-izes them (test 1 still + 1
  dive = 97 cr before committing the batch).
- Chart overlays over moving film risk legibility → overlays only at linger points over
  the film's calm frames (lindir places dips accordingly).
- Data honesty vs drama: spires exaggerate — must carry the scale label (hobson gate).

## Owners
lindir (brief) · gwaihir (generation) · narvi (bake/player) · **garth or vera** (overlay
charts) · kalin (deltas/copy) · tukey+hobson (honesty gate) · hammer (rayshader render
pipeline) · smaug (kill-list before ship).
