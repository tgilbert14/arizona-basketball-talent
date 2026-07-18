# Roadmap — Power-4 Girth Index (v9 and beyond)

Updated July 18, 2026. The product now covers all 67 programs in the SEC,
Big Ten, ACC, and Big 12 across football and basketball. The foundation is
live: conference-aware analysis, Conference Lab, automated refreshes, a public
cover, and a weekly change brief.

The roadmap is ordered by trust and user value: protect the data first, make
the experience fast and legible second, then add deeper analytical layers.

## Completed foundation

- ✅ 67-program Power-4 configuration and historical recruiting backfill.
- ✅ Conference-aware boards, ranks, labels, and comparison pools.
- ✅ Conference Lab with distribution-first, realignment-honest comparisons.
- ✅ Football and basketball paths with searchable program selection.
- ✅ Nightly refresh, validation, rollback, QC ledger, and freshness status.
- ✅ Weekly brief that separates recruiting activity from coverage expansion
  and historical data repairs.
- ✅ v9 product shell: editorial Home, program fingerprint, direct task routes,
  responsive controls, clearer page orientation, and a rebuilt public cover.
- ✅ Talent Origins with raw-row geography, conservative HS/prep source
  classification, state and position boards, open-cycle trends, table twins,
  exact deep links, and a consolidated Program Reach workspace.

## 1. Release hardening

1. **Production smoke pack:** automate first-load, tab-navigation, deep-link,
   chart-fallback, and mobile viewport checks after every deployment.
2. **Performance budget:** record cold-start, default-view, and chart-render
   timings; fail the release check when a regression crosses the budget.
3. **Phone-first chart fallback:** serve lightweight static charts first on
   constrained devices, with an explicit opt-in to the interactive version.
4. **Degraded-state UX:** add a branded reconnect screen, retry affordance,
   and plain-language messages when a data source or optional outcome feed is
   unavailable.
5. **Accessibility sweep:** keep table twins for charts, finish keyboard paths
   for player selection, and include screen-reader checks in release QA.

## 2. Reports, saves, and sharing

1. **Brief builder:** pin charts, players, and caveats into one shareable
   scouting report that retains the active program, sport, and year window.
2. **Saved comparisons:** let a visitor keep a short watchlist of programs and
   reopen exact analysis states from Home.
3. **Export upgrade:** add accessible HTML/PDF exports with sources, sample
   sizes, refresh date, and methodology attached automatically.
4. **Weekly subscriptions:** generate program- and conference-specific briefs
   after the global brief proves reliable over several refresh cycles.

## 3. Player outcomes

Grade classes on what players became, not only how they were rated at 18.

- Join draft results, starts or snap-count proxies, honors, and roster status
  with an explicit confidence score for every player match.
- Publish join coverage and sensitivity before showing any outcome ranking.
- Add a “What they became” field to player cards and Coach Eras.
- Compare rating-at-signing with outcome tiers to surface programs that develop
  or identify talent better than the market.

## 4. Portal lifecycle and retention

- Track arrival, development, departure, destination, and role when the source
  supports each event.
- Distinguish high-school development, transfer acquisition, and roster churn;
  never compress those strategies into one unlabeled score.
- Show cohort retention and contribution alongside portal share so strategy is
  interpreted in context.
- Keep moves outside the tracked 67-program universe explicitly unresolved.

## 5. Conference eras and realignment

- Build time-aware conference distributions instead of backcasting today’s
  membership onto older classes.
- Add an era timeline for league medians and spread, with realignment markers.
- Offer current-membership and historical-membership lenses side by side when
  both can be supported honestly.

## 6. Talent Origins extensions

- Canonicalize prep-school names before publishing a durable Talent Factories
  leaderboard; keep alias changes reviewable and versioned.
- Add selected-state destination flow and program pipeline lift without a
  Sankey, using ranked bars and explicit current-conference caveats.
- Add per-capita state production only after joining a defensible national
  participation denominator; raw captured counts must not masquerade as rates.

## Data validity standards

- Every metric states its player pool, sample size, time window, and source.
- Every derived metric includes its formula and most important caveat.
- Snapshot changes are classified as activity, cycle coverage, program
  coverage, or historical maintenance before they reach a headline.
- Scrapes keep per-program replacement, timestamped backups, hole audits,
  validation, and rollback.
- New joins publish match rates before the metric ships.
- Realignment, source gaps, and untracked destinations are disclosed, not
  silently imputed.

## Suggested order

1. Ship v9 and lock the production smoke/performance baseline.
2. Build saved comparisons and the brief builder.
3. Add player outcomes with published match quality.
4. Add portal lifecycle and cohort retention.
5. Add conference-era timelines and realignment lenses.
