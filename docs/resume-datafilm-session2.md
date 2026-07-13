# Resume point: "The Conference, From Above" — session 2 (credit-gated)

_Saved 2026-07-13. Balance at save time: ~37 credits. Session 2 needs ~630; Old Pueblo source-side regen ~495._
_When credits refresh: open this file and go. Design doc: [data-film-design.md](data-film-design.md)._

## What is already proven (session 1, committed)
- Data-true terrain pipeline works end-to-end: `scripts/render_terrain_still.R` renders 16 Big 12 roster-mass
  mesas from `work_terrain_mass.csv` (final params: superellipse ^2.6 walls, crown clamp 0.88, phi=31, zoom=0.47).
- Seedance conditioning preserves the data-bearing landforms while photoreal-izing atmosphere —
  fidelity validated by landform-arrangement inspection on `work_terrain_dive.mp4` (mini-tier test, 20 cr).
- First prompt written: `prompts_datafilm/dive_overview.txt`.

## Session 2 run order (the scroll-film skill, stages 1-6)
1. **lindir** — Story Brief for the conference flyover (the one feeling: scale made honest — every mesa IS the
   data). Gate: `check_story_brief.py`.
2. **gwaihir** — full prompt set from the brief. Gates: `lint_prompts.py` + `check_coherence.py` (now includes
   seam-rewind v2 + implied-entity checks — all six checks must pass BEFORE credits move).
3. Stills from rendered terrain frames (gpt_image_2 conditioning, ~7 cr each) → dives (seedance_2_0 std 1080p,
   72 cr/8s; `get_cost` preflight EVERY call; trust `balance`, never arithmetic).
4. **narvi** — seams (`check_seams.py`), 60fps minterpolate scd=none, xfade master, WebP frames, canvas player
   per the old-pueblo exemplar (`~/OneDrive/Documents/VGS - R/old-pueblo-scroll/index.html`).
5. Deploy clean bundle to a new public Pages repo; QA including background-tab boot.

## Budget sketch (re-preflight everything)
- 5-6 terrain stills conditioned: ~40 cr · 5 dives + 4 connectors @ 72 cr: ~650 cr → trim to 8s clips or mini-tier
  the connectors if the refill is small. Hard rule: no generation before Stage 1-2 gates pass.

## Also queued behind credits
- Old Pueblo source-side regen (~495 cr): re-generate the v2 scenes with first+last-frame pinning to lift the
  night-seam SSIMs (0.43-0.50 measured); plan in old-pueblo-scroll/docs/build-status.md.
