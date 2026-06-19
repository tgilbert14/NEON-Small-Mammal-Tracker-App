# The NEONize Playbook

**How to build (or remake) a NEON data-product app to the Small Mammal Tracker quality bar.**

"NEONize a product" = take any NEON data product and ship an R/Shiny app that is the
small-mammal flagship's equal in **flow, UI, statistics, creativity, QC, and honesty** —
but with insights *native to that product*, not a reskin. The flagship is the
**NEON Small Mammal Tracker** (`App-NEON-Small-Mammal-Tracker/`, DP1.10072.001); the
recruiting-analytics ancestor is the **Big 12 Girth Index**
(`arizona-basketball-talent/`, see `docs/girth-index-patterns.md`).

This doc is the contract. It has three layers:
1. **The quality bar** — the dimensions every NEONized app must hit.
2. **The reusable full stack** — what ports wholesale (design system, data bundling, shared helpers, the pin-card system, report PDF).
3. **The NEONize procedure** — the agent-driven research → design → build → adversarially-verify → ship loop, run fresh per product.

---

## 1. The quality bar (the six dimensions)

Every NEONized app is judged on the same axes the flagship nails:

| Dimension | What "flagship quality" means |
|---|---|
| **Flow** | A splash/site-picker → instant demo-on-startup → an Overview that leads with the answer → progressive tabs. One global "selected entity" reactive every tab reads. No dead ends; every empty state offers the next action. |
| **UI** | DDL light "Girth Index" house style: warm paper bg, white cards w/ 3px colored top borders, Rubik, the navy/cardinal/gold triad. bslib `page_sidebar`. `info_pop()` ⓘ on every card. Mobile-first. Dark-mode via one toggle that every chart honors. |
| **Statistics** | Defensible, cited methods (Hill/Chao1/rarefaction/Schnabel/etc.). Every headline number has an `insight_banner()` "answer up front". n-gates before reporting. De-pseudoreplication. The right effort/scale fixed before any comparison. |
| **Creativity** | Playful framing with real science underneath — emoji, rarity tiers, celebratory confetti on standouts, a shareable "trading card", a signature interactive (the Size Lab pin-card scatter). Show-off, not gimmick. |
| **QC** | The app is *useful to the people who collect the data*. Click-to-inspect flag→modal/record patterns. Honest outlier flags that are KEPT not deleted, phrased "verify, not wrong". A downloadable per-entity QC record. |
| **Honesty** | The non-negotiable. Every claim is stated where it lives (on the chart, screenshot-safe). Caveats for what the method can't say. No false precision. "Not detected ≠ absent." Match rates published for joins. Deliberately-omitted analyses stay omitted (e.g. SMI). |

If a feature can't be done honestly on the product's data, it doesn't ship — it gets a caveat or a "why not" note instead.

---

## 2. The reusable full stack (ports wholesale)

A NEONized app is a **lean independent sibling directory** (copy-with-attribution, like the
mammal/beetle apps — NOT a shared package; independent deploys must stay self-contained). Copy
these from the flagship and adapt the data layer:

### 2a. Design system & chrome — copy verbatim
- `global.R`: the `DDL` token list (navy `#0C234B`, navy2 `#16386e`, cardinal `#AB0520`, gold `#FFD200`, gold2 `#c9a300`, sky, green, ink, muted, bg, paper, line); `app_theme` (bslib bs5 + Rubik); `asset_url()` (mtime cache-bust); `spin()`, `info_pop()`, `insight_banner(icon, ..., tone)`, `glow_badge()`.
- `ui.R`: `page_sidebar`, the `<head>` library block (Rubik, sweetalert2, canvas-confetti, driver.js, **html-to-image@1.11.11**, styles.css, app.js), the splash/national-site-picker (STATIC `leafletOutput`, never inside a `renderUI` — the Connect Cloud re-bind race), the loading overlay, the DDL business footer.
- `server.R`: `plotly_theme(p)` (theme-aware, the navy+gold hoverlabel, `displayModeBar=FALSE`), `note_plot()` empty-state, `ctx_anno()` (BUT see gotcha #5), the `is_dark()` reactive.
- `www/styles.css` `:root` tokens + dark-theme block; `www/app.js` (count-up engine, confetti, loading overlay, the custom-message handlers).

### 2b. Data bundling — copy the pattern, swap the product
- `scripts/refresh_data.R`: per-site `loadByProduct` → trim to a `keep` column vector → xz-compress → `data/sites/<SITE>.rds`. Build with **R-4.1.1** (neonUtilities; R-4.5.2 crashes on `loadByProduct`). Token in gitignored `.neon_token` (env `NEON_TOKEN`).
- `read_bundle()` (defensive — NULL on missing/corrupt, never crash boot), `load_site_bundle()`, `data/site_index.rds` (one row/site for the picker), the manifest→republish discipline (Connect Cloud serves the *published* snapshot — rebuilt bundles aren't live until `writeManifest()` + commit + republish). See `docs/data-bundling-pattern.md`.
- A committed `data-sample/` demo so the app runs bundle-only with no network (demo-on-startup).

### 2c. Shared analysis helpers — port the defensible ones
From `R/helpers.R`: `species_level_only()` (drop genus-only/morphospecies before any richness), `make_species_pal()` (one color per species across all charts), Hill numbers / `species_accum()` (rarefaction + Chao1 w/ CI), `mode_chr()`, `safe_*()` NA-safe reducers, the n-gate idioms. The diversity family ports to almost any taxon product.

### 2d. The interactive-downloadable-plot funnel — the signature every app gets
The Size Lab (`www/pincards.js` + the plotly `customdata` pattern; see `size-lab-feature` memory) is
the template for **the one interactive every NEONize app should ship**: a "position entities in a
2-D space → pick one → inspect → take it with you" funnel. The full funnel, in order:

1. **Position** every entity (individual / plot / species / taxon — the product's unit) on one chart,
   coloured by a meaningful class, with **a filter (species/site/etc.) and an honest, gated overlay**
   (a fit line drawn *only* where the relationship is real; framed as what it IS, e.g. "a QC map, not
   a body-condition index").
2. **Click → pin a profile card** (draggable/resizable, gold leader line anchored to DATA coords).
3. **Chip on the card → a per-entity profile / QC record** (`output$…Card` + `individual_qc_flags()`
   analog: ranked, *"verify not wrong"* data-quality flags). **Scroll it into view** on open (custom
   message → scroll the rendered card node, §4).
4. **Download the works:** the chart with pins baked in (html-to-image PNG), the profile/QC card
   (PNG), and the raw per-entity record as **analysis-ready CSV metadata** (`downloadHandler`).

**It is plotly, not ggiraph** (the apps are already plotly; no second rendering stack). This funnel —
click-for-profile, QC checks, downloadable plot + card + metadata — is a **default deliverable**, not a
one-off; map it to each product's unit. Carry the hard-won gotchas (§4).

### 2e. Report PDF — `R/report_pdf.R`
Base `grid`/`grDevices` `cairo_pdf` (no LaTeX/Chrome), streamed by a `downloadHandler`. Re-theme
the page geometry from `DDL`; swap the per-product content renderers.

### 2f. What does NOT port (product-specific — design fresh every time)
The **entire data model and its "unit of analysis."** For small mammals the unit is the
*tagged individual* and its mark-recapture career — so the dossier, Hall of Fame, MNKA detection,
age/lifespan, tag-identity QC, home-range/trap-grid, body-measurement outliers are all
mark-recapture-specific and port to **nothing** without individuals. Before building, answer:
**what is this product's unit, and what is its capture career analog?** (For count/cover products
there are no individuals — the unit is the plot, the species, or the trap×bout. See the beetle
app note in `revamp-design` memory and the plant-app research.)

---

## 3. The NEONize procedure (run fresh per product)

A repeatable loop, each phase an agent fan-out (Workflow), staying in the loop between phases.
This is exactly how the Size Lab and the plant-diversity sibling were built.

**Phase 0 — Understand the flagship + the ancestor.** Deep-read the reference apps so the port map is accurate (what's reusable vs product-specific).

**Phase 1 — Research the product (the gated step — REQUIRED EVERY TIME).** A workflow fanning out:
- A **schema agent** (WebFetch the NEON product page + neonUtilities docs): exact tables, field names, sampling design, data volume, gotchas.
- A **domain agent** (Jornada for plants/rangeland; Fauna for wildlife; Aquatics for water): the scientifically-meaningful, *cited* product-native insights + their honest caveats + what to AVOID over-claiming.
- A **stats agent** (Quinn): the statistically-correct computation of each metric + the pseudoreplication/scale/effort traps + the analysis-ready export shape.
- An **architecture agent** (Tim): the port map — reuse/adapt/skip/net-new, file-by-file.
- An **innovation agent** (Sarah): the flagship interactive + the dossier/QC-card analog + one novel-but-grounded idea, evidence-based.

**Phase 2 — Design.** Synthesize the research. Lock: the unit of analysis; the tab structure; the flagship interactive; the "select an entity → profile + downloadable QC card" funnel; the data/bundling strategy (which demo site, the `keep` vector). Confirm the one genuine fork with the user if close; otherwise proceed.

**Phase 3 — Build.** Scaffold the sibling directory. Reuse §2 wholesale; build the product-specific data layer (`helpers.R`), the renders (`server.R`), the tabs (`ui.R`), the interactive (`pincards.js` adaptation), the styles. Author the cohesive core yourself (tight coupling), parallelize only genuinely-independent pieces.

**Phase 4 — Adversarially verify (the discipline that repeatedly pays).** A review workflow over the **git diff** with fresh eyes per lens (Wes/JS, Vera/chart, the domain+Quinn/honesty, Aaron/chaos-field-user, a pure R-correctness hunter). It WILL find real regressions you introduced — the Size Lab review caught a blocker (a dead-after-re-render scatter) the happy-path tests missed. Triage by severity, fix blocker+high+certain, run again.

**Phase 5 — Verify in the running app.** `preview_start`, load the demo (the `setInputValue('demoBtn', …, {priority:'event'})` trick), exercise every new surface headlessly (real interactions, not synthetic `.click()` lies — drive plotly via `gd.emit('plotly_click', …)` with a full point object incl. `data:{}` so the binding doesn't choke), screenshot proof, fix, repeat until zero server + console errors.

**Phase 6 — Ship hygiene.** Memory entry (what it is + the gotchas). Manifest→republish. A landing/og card if public.

---

## 4. The gotcha catalog (carry into every NEONize)

- **R version:** R-4.5.2 runs the app but **crashes on `neonUtilities::loadByProduct`** (access violation). Pull/bundle data with **R-4.1.1**. Launch R via **PowerShell**, not git-bash (git-bash segfaults R here). Reference neonUtilities by a *computed* package name so the rsconnect scanner doesn't pin it into the manifest (the deploy is bundle-only + lean).
- **plotly re-render kills event handlers:** a Shiny+plotly re-render runs `Plotly.purge`+`newPlot` on the SAME div, silently wiping `gd.on()` listeners. **Never** gate binding on a persistent expando — re-attach `plotly_click` on every render (rAF-debounced MutationObserver scan). This was the Size Lab blocker.
- **plotly pin anchors must be DATA coords**, recomputed via `gd._fullLayout.xaxis.l2p()+_offset` on `plotly_relayout` + a `ResizeObserver` — frozen pixels drift on resize/fullscreen/rotate. Anchor from the data point, not the click event (touch has no `clientX`).
- **`ctx_anno()`/`add_annotations` accumulates** across reactive re-renders (the binding doesn't clear it) — fold the caption into the `layout(annotations=...)` list instead, so it's replaced wholesale. (Invisible when copies overlap, but real.)
- **Named-vector `updateSelectInput`** spams console warnings — wrap choices as `as.list(setNames(...))`. Build filter choices from the *plotted* subset so a choice can't land on an empty chart.
- **selectize fires `change` via jQuery `.trigger()`** — a native `addEventListener('change')` never sees it. Listen on `shiny:inputchanged` (jQuery) or the widget's own event.
- **`validate(need())` doesn't display in some widget outputs** (stale output persists) — return a real message-chart/empty-state instead.
- **`asset_url()` bakes the cache-bust version at app start** (ui is an object, built once) — a running server serves the old `?v=` after you edit a `www/` file; **restart** to pick up JS/CSS changes in preview.
- **html-to-image over WebGL fails** — force SVG (`scatter`, not `scattergl`/`toWebGL`) for any chart you want to export; `Plotly.Plots.resize(gd)` before `toPng` (a tab that rendered hidden can be 0-sized); strip live animation classes before capture.
- **Register pin-binding listeners BEFORE any aux handler in the IIFE.** A `Shiny.addCustomMessageHandler(...)` (or any statement) placed near the top of `pincards.js`, before the `DOMContentLoaded`/`shown.bs.tab` bind listeners, can throw during head-eval and abort the IIFE so binding never registers — tap-to-pin silently dead, with **no captured console error** (the throw predates the preview's console hook). Put the binding listeners first; put aux handlers last and `try`-guarded. (Caught verifying the Size Lab scroll fix — it had killed the whole pin layer.)
- **The `dataSig` pin-clear must ignore the highlight/"tracking" trace.** Selecting an entity appends a gold highlight trace (N→N+1); a trace-count-based signature flips and wipes every pin the instant the user opens a profile from a pin (the happy path). Filter the highlight trace out of the signature.
- **Scroll-into-view: target the rendered card node, NOT the uiOutput wrapper.** A bslib `uiOutput` in a fill layout is `display:contents` — it has **no box**, so `scrollIntoView` on `#…Output` is a silent no-op. Scroll the actual rendered child (`#…CardNode` / the empty-state node), polling until it exists AND has `height > 1` (the card re-renders async after the select). (The Size Lab scroll bug: a fixed-delay scroll to the wrapper did nothing.)
- **Never pool repeated visits as independent samples.** NEON re-surveys the same plots/quadrats yearly. Pooling years into a richness / rarefaction / Chao estimate treats one quadrat's 7 visits as 7 spatial samples — it inflates richness ~2× and the incidence-unit count several-fold, and conflates spatial with temporal turnover. Compute snapshot metrics on **one survey per unit** (a `latest_snapshot()`); reserve the multi-year table for the explicit time-series. (Caught by the plant-app review.)
- **Area-scaled metrics (density, per-ha, cover share) must be scoped to the population actually sampled over that area.** NEON nested-samples small stems / fine scales over a SMALLER area than the headline area variable — dividing everything by the big area biases the small classes low (a flat curve that's a sampling artifact, not biology). Scope to the protocol threshold (e.g. trees ≥10 cm DBH over `totalSampledAreaTrees`) and label it. Quadratic/RMS stats (QMD) must be POOLED (`sqrt(ΣD²/Σn)`), never a mean of per-unit RMS values (Jensen). (Veg-app review blocker.)
- **One fixed output id, not one-per-entity.** A `renderPlotly`/`renderUI` registered under a per-row id (`output[[paste0("spark_", id)]]`) accumulates a new binding for every entity the user opens (a slow leak). Use a single fixed output that reads the selected-entity reactive.
- **Cover/percentage SHARES need a structural-zero denominator** (divide by all sampled units, not only where-present) — present-only means inflate patchy categories and distort the share. And a headline metric must use **one shared function** in the bundler and the app, or the picker and the hero will show different numbers for the same thing.
- **dplyr `summarise()` sees earlier newly-created columns** — `richness = mean(richness)` then `sd = sd(richness)` makes sd operate on the scalar mean (→ NA). Compute the spread before the reassignment.
- **Adversarially verify the DIFF with a fresh agent** every time — it has caught real regressions on every session it was run (incl. the plant app's year-pooling blocker and the Size Lab's dead-after-re-render blocker).

---

## 5. The flagship feature inventory (steal the best, per product)

From the **Small Mammal Tracker**: the splash national picker (by-site / by-species), demo-on-startup,
the hero stat band (clickable → ranked-breakdown modal), the species-first Overview with an
auto-written narrative (`site_insights()` compute→rank→glue), the Population tab (MNKA+CPUE,
detection-corrected abundance, species accumulation+Chao1, env-driver correlation overlays with the
driver-semantic color system), the Community Pulse (sex/age, Hill profile, per-plot trends,
body-size profile, lifespan, phenology), the **Hall of Fame** leaderboard (rarity tiers, re-sortable),
the **Dossier** trading card (+ downloadable PNG), the **Size Lab** (pin-card scatter + QC card),
the click-to-inspect QC modals, the report-card PDF, the two-site compare.

From the **Girth Index**: highlight-one-in-a-grey-cloud, named-quadrant scatter, violin+jitter+mean
"position DNA", before/after arrow chart, percentile-band trend, the holographic trading card, the
reusable hover-card builder, the narrative-insight generator, the config-driven entity picker.

For each new product, map these to the product's unit and KEEP the ones that stay honest;
invent the product-native ones the research surfaces.

---

## 6. Deployment & maintenance — the full lifecycle (dev → deploy → self-update)

The suite has **migrated off shinyapps.io to Posit Connect Cloud with a GIT-BACKED deploy**.
This is now the standard; shinyapps.io (small-mammal reference) is legacy and slated to follow.

**Deploy model (the new standard — Connect Cloud, git-backed):**
- The app lives on Connect Cloud, pointed at the GitHub repo + its watched branch. **A push to the
  watched branch IS the deploy** — Connect Cloud auto-republishes. So there are **no shinyapps.io
  secrets, no `rsconnect/` dir, and no `deploy.R` step** (those are the legacy shinyapps path).
- Required in-repo: a lean **`manifest.json`** (`rsconnect::writeManifest()`; bundle-only, keep
  `neonUtilities` OUT via the computed-package-name trick), the committed `data/` bundles, and a
  `docs/index.html` GitHub Pages showcase whose `APP_URL` points at the live Connect Cloud app.
- Branch naming is split across the suite (`main` vs `master`) — each workflow must push to the
  branch its own Connect Cloud app watches. Standardize new repos on `main`.

**Auto-refresh + self-deploy (`.github/workflows/refresh-data.yml`) — copy this shape:**
- **Schedule (identical across the suite):** `cron: "0 6 * * 0"` (Sunday 06:00 UTC = Saturday 23:00
  America/Phoenix, off-peak), with a **gate job** that proceeds only on the **first Saturday of the
  month** (`dow=6 && day<=7`, `TZ=America/Phoenix`) — cron can't say "first Saturday", so fire weekly
  and gate. `workflow_dispatch` with a `skip_download` input always proceeds (fast redeploy test).
- **Flow:** gate → checkout → `setup-r` + deps → fetch raw + rebuild `data/sites/*.rds` (+ any
  overlays) → **commit/push to the watched branch (= the deploy on Connect Cloud)** → optionally open
  a data-refresh PR. Time-box + `continue-on-error` the heavy/optional steps so they can't block the
  deploy. `NEON_TOKEN` is an optional secret (anonymous works, slower).
- **Two deploy triggers seen in the wild — prefer auto-push:** (a) *auto* — push refreshed data
  straight to the watched branch (mammal/bird/phe/plant). (b) *PR-merge* — open a PR a human merges
  (veg) — this is NOT self-deploying; convert to auto-push unless a review gate is wanted.

**Derived/master apps (e.g. Driver Cascade):** their bundle is built FROM sibling repos' bundles, so
CI must obtain them — `git clone --depth 1` each sibling repo (use the real slugs, not dir names:
NEON-Small-Mammal-Tracker-App, NEON-Plant-Diversity, NEON-Breeding-Birds,
NEON-Plant-Phenology-Explorer, NEON-Vegetation-Structure-Explorer, NEON-Ground-Beetle-Tracker),
copy their `data/`, run the build script, commit the derived `.rds`. A master app needs a **GitHub
remote + a Connect Cloud app** before any of this works.

## 7. Per-app readiness checklist (audit every app against this)

Data bundles: `data/sites/*.rds` present + valid (loadable, non-empty) · `data/site_index.rds`
(picker) · `data-sample/demo.rds` (instant demo) · all git-tracked · refreshed within the cadence.
Automation: `.github/workflows/refresh-data.yml` on the **standard schedule** · self-deploys via
**auto-push** (not PR-merge) · `manifest.json` present · GitHub **remote** exists · `docs/index.html`
`APP_URL` is live. NEONization: cover/landing splash · **in-app sibling links** + `docs` cross-promo
grid covering the WHOLE suite · mobile-responsive CSS (`@media`, prefers-reduced-motion) · **QC-flag
system** (§ below) · metadata/codebook view · comprehensive downloads (CSV + card PNG + report PDF) ·
entity pin-cards · current shared chrome (styles.css + app.js + pincards.js).

**The QC-flag system (gold standard — every app gets it; first ported to birds):** `<entity>_qc()` →
ranked *"verify, not wrong"* flags (high/warn/info) + the EXACT offending rows behind each; surfaced
on the entity profile INSIDE the export node (PNG captures it); each flag **clickable → inspector
table** of offending rows + per-flag CSV; a full **QC-report CSV** (`<entity>_qc_report()`); clean
path shows a green reassurance. Tune thresholds **data-derived + domain-grounded** (ask the domain
agent) and validate on contrasting sites so it never cries wolf (target ~0 high on clean NEON data).
CSS class convention: standardize on `.qc-flag-<level>` (not `.qc-flag.<level>`). Full recipe +
bird thresholds: memory `neonize-qc-flag-pattern`.

**Sibling links + cover page:** maintain ONE registry of the suite (name · emoji · tagline · DPID ·
github.io showcase URL · live Connect Cloud URL) and render it both in `docs/index.html` (the
`.series-grid`) AND in-app (an "Explore the NEON series" block in About/footer). When a new app ships,
add it to the registry so EVERY sibling links to it (Breeding Birds + Driver Cascade were missing).

---

*Living doc. Plant-diversity (DP1.10058.001) was the first full NEONize; birds/phenology/veg/cascade
followed. §6–7 added from the suite-wide automation+bundle audit (the Connect-Cloud git-backed deploy
migration, the shared off-peak schedule, the QC-flag generalization). Keep the **Cody** subagent
(hosting/CI) and a future **neonize** subagent in sync with §6–7.*
