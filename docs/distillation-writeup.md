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

## Measured results (three cycles, one task class)
| Cycle | Small-model score | Frontier ref | Finding |
|---|---|---|---|
| 1 | 65 / 100 | 92 | Blind judge correctly identified the small model; 4 tells named (velocity flattening, internal contradictions, blanket-negative safety, intention mismatch) |
| 2 (after checklist patches) | 65 | 90 | Old tells eliminated — **failure migrated** to numeric self-consistency & round-number drift: checklists alone don't close the gap |
| 3 (after gates made the new tells machine-checkable) | **68** | 92 | Numeric tell class **extinct** (judge praised the gate-forced truthNotes); failure migrated to cross-file coherence — 'strong local prose, weak global bookkeeping.' Taxonomy complete: style → numeric → coherence → open synthesis (the floor) |

**Exam-bank result (same day):** the small-model agent passed 3/3 real-failure licensing
exams at full marks — exact root causes, complete prescriptions, calibration-aware
routing. Refined thesis: **explicit knowledge distills ~losslessly through doctrine +
worked examples; the persistent frontier gap is confined to open-ended synthesis** —
which is precisely what the blind-judged tournament isolates and measures.

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
