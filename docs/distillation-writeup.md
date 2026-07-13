# Measured Capability Distillation for Multi-Agent Systems
_A one-page technical summary for grant/SBIR narratives. Desert Data Labs, 2026-07-13._

## The problem
Frontier LLMs produce the best creative-technical work, but production systems run on
smaller, cheaper models. "Write better instructions" is the standard answer; nobody
measures whether it works. DDL built a system that makes the capability gap **measurable
and monotonically closable**.

## The system (four layers, all in production)
1. **Casebook** — frontier-model decisions captured as worked examples (situation →
   decision → reasoning → outcome → transferable rule), because judgment transfers
   through cases, not principles. (13 cases from one build.)
2. **Executable gates** — the checkable subset of doctrine compiled into scripts that any
   model must pass before resources are spent: content-filter lexicon linting, narrative
   structure validation (timestamp monotonicity, causal-connective audits, copy budgets,
   claim-vs-timeline reconciliation), and rendered-output verification (per-seam SSIM).
3. **Exam bank** — real past failures with known answers, used to license agents per the
   training protocol (found / missed / hallucinated scoring).
4. **The tournament loop** — identical tasks run by the same agent definitions on the
   frontier model vs a small model; artifacts scored by a **blind judge** who does not
   know which model produced which. Every "tell" that distinguishes the small model
   becomes a new checklist line, gate check, or exam — then re-run.

## Measured results (five cycles, one task class)
| Cycle | Small-model score | Frontier ref | Finding |
|---|---|---|---|
| 1 | 65 / 100 | 92 | Blind judge correctly identified the small model; 4 tells named (velocity flattening, internal contradictions, blanket-negative safety, intention mismatch) |
| 2 (after checklist patches) | 65 | 90 | Old tells eliminated — **failure migrated** to numeric self-consistency & round-number drift: checklists alone don't close the gap |
| 3 (after gates made the new tells machine-checkable) | 68 | 92 | Numeric tell class **extinct** (judge praised the gate-forced truthNotes); failure migrated to cross-file coherence — 'strong local prose, weak global bookkeeping.' Taxonomy complete: style → numeric → coherence → open synthesis (the floor) |
| 4 (coherence gate live + gate-forced repair pass) | **74** | 89 | Biggest single-cycle jump; gap nearly halved (24 → 15). The coherence gate caught a live contradiction **the small model produced despite being warned about that exact failure class in its prompt** — instruction did not prevent the tell; the executable gate did, and a one-pass small-model repair fixed it. The small model **won the honesty dimension outright** (0.93 vs 0.82; the blind honesty judge guessed it was the frontier film) — gate-forced sourcing discipline now *exceeds* frontier default behavior on that axis. Residual gap concentrated in craft/synthesis (0.62 vs 0.88: stock titles, a reframe announced rather than landed, no fusion image) — the taxonomy's predicted floor. Judging upgraded this cycle to five per-dimension **isolated** judges (0–1 + pass/fail + reason-then-discard + Unknown escape), which localized the gap per-axis and also caught two bookkeeping slips in the frontier reference itself |
| 5 (rewind gate v2 + three gate-forced repair passes + evidence-disciplined judging) | **82** | 85 | The coherence tell class is **extinct**: after the broadened seam-rewind gate forced two more small-model repairs, a 3-judge evidence-disciplined panel scored the small model's coherence **above the frontier reference, unanimously** (0.92 vs 0.80). The small model now wins 2 of 5 dimensions (coherence, honesty 0.95 vs 0.68). **Honest caveats:** the reference's decline (92 → 85) reflects *sharper judging finding real latent defects in the un-repaired reference* (a wrong mile marker in its motif centerpiece; copy asserting a car window under a no-vehicles guard; an undeliverable "Mile 0" marker and "breath in the air" under its own text/people guards) — the reference never went through the gate-repair loop, so a gap of 3 is not parity. The craft floor (0.66 vs 0.90) never moved across five rounds: **that stable ~0.25 craft deficit is the true frontier premium.** Methodology hardening this cycle: one judge verdict was invalidated for a *hallucinated quote* (it counted the guard negation "no return to the pine-belt hover" as the action itself); the fix — an evidence-discipline rubric clause requiring a verbatim quote for every counted defect — produced unanimous, fully-quoted verdicts on re-run |

**Exam-bank result (same day):** the small-model agent passed 3/3 real-failure licensing
exams at full marks — exact root causes, complete prescriptions, calibration-aware
routing. Refined thesis after cycle 4: **explicit knowledge distills ~losslessly through
doctrine + worked examples; gate-enforced disciplines can push the small model PAST
frontier default behavior on their axis (measured: honesty, 0.93 vs 0.82); the persistent
frontier gap is confined to open-ended synthesis** — which is precisely what the
blind-judged tournament isolates and measures. Corollary with cost implications: the
small model failed to follow an explicit warning about a failure class but repaired it
perfectly when a gate pointed at the instance — **verification is cheaper than
generation**, so gates + small-model repair loops beat frontier-model generation for
every checkable quality axis.

Secondary validated result (same week): **data-true generative terrain** — real database
metrics rendered as literal landform heightmaps (R/rayshader), used as conditioning
frames for video-generation models that photoreal-ize atmosphere while preserving the
data-bearing landforms — a novel honest-visualization technique bridging statistical
graphics and generative cinematography (fidelity verified by landform-arrangement
inspection; quantitative SSIM protocol defined).

## Why it matters
- **Innovation:** a repeatable, blind-judged methodology for distilling frontier-model
  workflow quality into small-model production systems — with the failure taxonomy
  (checkable / examinable / judge-only) discovered empirically.
- **Commercial:** the same loop hardens every DDL deliverable pipeline (field-data apps,
  dashboards, cinematic data stories) against model-tier cost reduction.
- **Evidence trail:** all artifacts version-controlled; every cycle logged in a training
  ledger with scores, tells, and the patch each tell produced.
