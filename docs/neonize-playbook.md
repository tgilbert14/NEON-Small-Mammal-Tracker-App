<!-- neonize-playbook-stub: v1 -->
# NEONize playbook: pointer for Small Mammal Tracker

The NEON suite playbook has one home, read by Claude Code and Codex alike:
https://github.com/tgilbert14/NEON-Driver-Cascade/blob/master/docs/neonize-playbook.md
(side-by-side clone: `../NEON-Driver-Cascade/docs/neonize-playbook.md`). Read it before
suite work. Fold every suite rule, pattern or gotcha into that file, never into this one.
Anything here beyond this pointer and the app facts below is a stale fork: report it.

## This app

- Repo `NEON-Small-Mammal-Tracker-App` · watched branch `main` · product `DP1.10072.001`
- Rung specialist: `mara`
- Refresh: `refresh-data.yml` fires Saturdays 13:17 UTC and its calendar gate runs the first-week Saturday; producer, validator and restricted publisher push the validated candidate to review branch `automation/small-mammal-data-refresh`. A person opens and merges the PR into `main`, and that merge is the deploy.
- Local authority files: `DEPLOY.md` (release steps and Connect checks), `docs/BUILD-TEST-HANDOFF.md` (dated session ledger and next action), `docs/EXPERT-REVIEW.md` (Mara's science review), `docs/DRIVER-KNOWLEDGE-PACKAGE.md` (metric definitions the Driver consumes), `docs/project-status.md` (current version and open items)

## App-specific rules

- Search index measure: peak site-wide MNKA per species (`mnka_series()` on `clean_mam(bundle)` for that species, per-plot MNKA summed within each month, maximum month), after `species_level_only()`; it is a search key, never a ranking.
- Seasonal-driver card: the annual response is the app's own captures per 100 trap-nights per year, and the monsoon prior uses lag 1 year (slow seed-eater response).

## Owner-settled house rules (both tools)

- Default demo, startup and picker pre-selection site: **SRER** (then JORN, SJER, MOAB, ONAQ) unless this app documents a reason.
- Copy voice on covers, share cards and in-app copy: plain words a non-expert follows; say what NEON is; no em dashes; no sales pitch (price, access, ease); no hype adjectives; no sincerity labels ("honest", "real data"); never state what the reader can already see.
- No visible "illustration" badge on poster art; the art's status lives in alt text and the image provenance record.

Cold boot: also grep the fleet's `.claude/agents/LESSONS.md` in TG-Data-Apps (Codex: the
`dev-teams` mirror) for your role name: `grep -E '^- \[[0-9-]*\] [a-z0-9+-]*<role>[ +]'`.
