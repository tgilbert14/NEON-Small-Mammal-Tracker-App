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

### 2g. The site-picker map contract (Explore/About popup + sidebar sync) — copy from Ground Beetle
The splash national picker must behave identically across apps (the June 2026 audit found 5 of 7 broke a contract). Three contracts:
- **Explore/About popup.** Each `addCircleMarkers` dot carries a native leaflet popup (built by `site_popup_html(row)`, bound via the `popup=` arg, NOT a server round-trip, so it survives map hide/re-show): an **"Explore this site"** button (`onclick` raises the loading overlay client-side, then `Shiny.setInputValue('siteExplore', code, {priority:'event'})`) and an **"About this site"** button (`Shiny.setInputValue('siteInfo', code, {priority:'event'})`, an instant modal, no load). Options `maxWidth=300,minWidth=230,autoPan=TRUE,closeOnClick=FALSE`.
- **Sidebar sync on select (the #1 cross-app divergence).** Picking a site on the map MUST update the sidebar dropdowns so they reflect what is loaded. Bug pattern: the `observeEvent(input$stateSel)` cascade sets `choices` only (never `selected`) and snaps to `sites[[1]]`, so the data loads but the sidebar stays on the OLD site. Fix is a **`pendingSite` bridge**: `rv$pendingSite=NULL` in reactiveValues; the map Explore handler sets `rv$pendingSite<-code` then `updateSelectInput(session,"stateSel",selected=state)`; the state-cascade observer honors it: `sel <- if (!is.null(rv$pendingSite) && rv$pendingSite %in% sites) rv$pendingSite else sites[[1]]; rv$pendingSite<-NULL; updateSelectInput(session,"site",choices=sites,selected=sel)`.
- **One shared `load_site()`** for the sidebar Load button, the map Explore, and the browse list, so behavior is identical everywhere.
PASS TEST: pick a NEW map site, data loads, and the sidebar state+site dropdowns now read THAT site. Reference port: **Ground Beetle** (`mapPickerServer()` + its map-picker R file), zero divergences, copy from it.

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
- **A leaflet map that init'd in a hidden tab paints BLANK on first open** (only tiles half-drawn / no markers) until a control changes and the draw observer re-fires. TWO causes, BOTH must be fixed: (1) the leaflet container was 0-size while its tab was hidden, so it needs its size recomputed when the tab is shown; (2) the proxy draw never ran for the DEFAULT selection on first paint. Fix: (a) on `shown.bs.tab`, dispatch a window `resize` (leaflet's own listener re-measures) — **`leaflet::invalidateSize` is NOT an exported R function**, it is a JS map method, so there is no R-side proxy call; do the size kick in JS. (b) Make the draw a NAMED function (not an anonymous `observe` body) and call it from an EAGER `observe({ draw() })` so the default selection draws on load, AND from a server observer keyed on a `tabShown` input that JS sets (after the resize tick) on `shown.bs.tab` for the relevant nav values — re-firing the draw for the current default with no control change. Drive it in the smoke test via `session$setInputs(tabShown="…")` + assert the named draw fn runs for defaults. (MSC tortoise: the Cohort-signals track map AND the Map-tab k-anon heat were both blank on first open.)
- **The HARDER leaflet-blank variant: a map in a `display:none` BLOCK that is shown/hidden by a MODE TOGGLE (not a tab) is BLANK on the FIRST reveal and only works on the 2nd+ — and the resize-kick + re-fire above CANNOT fix it.** Root cause is one level deeper than size: under Shiny's default `suspendWhenHidden=TRUE`, an output inside a `display:none` block **never RENDERS, so the htmlwidget NEVER BINDS at startup**. A window `resize` cannot `invalidateSize` a leaflet that was never instantiated, and a re-fired `leafletProxy(id)` draw lands BEFORE the widget binds and is silently DROPPED (console floods `Couldn't find map with id <id>`) — until a real show cycle finally binds it, which is exactly why pick #1 is blank but pick #2 works. The size/re-fire fix treats a symptom; the bind is the disease. **Definitive fix (apply ONE consistent pattern to every map that lives in a toggled/hidden block): `outputOptions(output, "<map_id>", suspendWhenHidden = FALSE)` so the widget BINDS at init regardless of which block is visible; then on reveal `invalidateSize()` the now-bound instance and re-fire the proxy draw.** Note this is the OPPOSITE call from the DTOutput-0-width rule below — leaflet is safe with `suspendWhenHidden=FALSE` (it binds fine at 0px and recomputes on `invalidateSize`), whereas a DT computes at 0px while hidden and won't redraw, so DTs keep default suspension. Make the JS size-kick find each bound instance directly — `HTMLWidgets.findAll(".leaflet").forEach(w => w.getMap() && w.getMap().invalidateSize())` — rather than only a global window `resize` (a just-shown 0×0 map does not always pick the resize up on the first tick); run it on a short delay AFTER the `shinyjs::show()` flips display. Regression (testServer is blind to the client canvas): grep the server source to assert the map id is `suspendWhenHidden = FALSE` and the JS does `invalidateSize` on `getMap()` — a "tidy-away" of the outputOptions re-suspends the map and fails CI. The REAL guard is a chromote driver: open the app, stay on the default mode, switch ONCE, and assert the now-visible map div has non-zero `getBoundingClientRect` + a `<canvas>` with height>50 + ZERO `find map with id` console errors. (MSC tortoise: the cohort↔individual toggle hid `track_map`/`cohort_hr_map` with `display:none`; this class recurred 3x because every prior fix kicked size/re-fired the draw but never made the hidden widget BIND. Driving it in real chromote confirmed `track_map_bound_at_init:true` and first-pick `outer 341×520, canvas 409×624, 0 errors`.)
- **A single-species / single-site product still needs a Community-Pulse filter — filter by COHORT STRATUM, not taxon.** When the unit is one species (or one site), wire the page filter to the biologist-assigned strata (sub-area / sex / size-or-age class), default "All". Drive it off the roster table that carries the strata, restrict each chart's records to the stratum's entity ids, and wrap each chart in a **lazy** small-n gate: `gate <- function(expr){ if (n < FLOOR) return(note_plot("…suppressed…")); eval.parent(substitute(expr)) }` — `substitute`/`eval.parent` so a suppressed stratum NEVER runs the (now-empty) plot body. A forged/unknown filter value must fall back to the full cohort (clean no-op), not error. Test it with `session$setInputs(communityFilter="area:…")` + assert it narrows and every kept row is in the chosen stratum. (MSC tortoise: one species, filter by sub-area/sex/size-class.)
- **Re-theme via TOKEN remap + a CI grep guard.** When porting/replacing a palette, keep the CSS variable NAMES and remap only their VALUES in the `:root` + `[data-bs-theme=dark]` blocks (the flagship trick — hundreds of usages re-theme from one edit), then fix the handful of hardcoded hex literals (gradients, status tints, plotly marker colors in server.R/ui_helpers.R, the report-PDF `PG` list, the mascot SVG, JS `confirmButtonColor`). Then add a smoke-test GREP guard: assert the specific OLD hexes are gone from every theme-bearing file AND the new house tokens are present — a regression to the old palette fails CI, not by eye in the field. (MSC tortoise: killed the brown/sand/sage theme for the small-mammal navy/gold/coral house palette this way; the grep caught literals the token remap missed — map-caption rgba overlays, an inline "gold diamond" span, the PDF palette.)
- **Never pool repeated visits as independent samples.** NEON re-surveys the same plots/quadrats yearly. Pooling years into a richness / rarefaction / Chao estimate treats one quadrat's 7 visits as 7 spatial samples — it inflates richness ~2× and the incidence-unit count several-fold, and conflates spatial with temporal turnover. Compute snapshot metrics on **one survey per unit** (a `latest_snapshot()`); reserve the multi-year table for the explicit time-series. (Caught by the plant-app review.)
- **Area-scaled metrics (density, per-ha, cover share) must be scoped to the population actually sampled over that area.** NEON nested-samples small stems / fine scales over a SMALLER area than the headline area variable — dividing everything by the big area biases the small classes low (a flat curve that's a sampling artifact, not biology). Scope to the protocol threshold (e.g. trees ≥10 cm DBH over `totalSampledAreaTrees`) and label it. Quadratic/RMS stats (QMD) must be POOLED (`sqrt(ΣD²/Σn)`), never a mean of per-unit RMS values (Jensen). (Veg-app review blocker.)
- **One fixed output id, not one-per-entity.** A `renderPlotly`/`renderUI` registered under a per-row id (`output[[paste0("spark_", id)]]`) accumulates a new binding for every entity the user opens (a slow leak). Use a single fixed output that reads the selected-entity reactive.
- **Cover/percentage SHARES need a structural-zero denominator** (divide by all sampled units, not only where-present) — present-only means inflate patchy categories and distort the share. And a headline metric must use **one shared function** in the bundler and the app, or the picker and the hero will show different numbers for the same thing.
- **dplyr `summarise()` sees earlier newly-created columns** — `richness = mean(richness)` then `sd = sd(richness)` makes sd operate on the scalar mean (→ NA). Compute the spread before the reassignment.
- **A `DTOutput` (or any htmlwidget) in a *full-width* card inside a bslib fill-container collapses to width 0** and never draws — only a `&nbsp;` placeholder, NO error logged, the widget payload arrives but DataTables can't init at 0 px. The fill flex column shrink-wraps the `shinycssloaders` spinner wrapper to nothing (same root cause as the map_picker leaflet-0-width bug). Fix: drop `spin()` and wrap in a plain `div(class="…-wrap", style/​CSS width:100%, DTOutput(id, width="100%"))`. DTs in `layout_columns` are fine (the grid gives width); only bare full-width cards bite. **Do NOT "fix" it with `outputOptions(suspendWhenHidden=FALSE)`** — that makes it worse: the DT then computes while the tab is hidden (0 px) and won't redraw on reveal. Leave default suspension so it computes on first reveal at real width (like the working `invTable`).
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

### The Expected-vs-Observed QC module (the EcoPlot recipe — reusable)

A transferable pattern for any NEON organismal product whose `taxonID`/code is a registry
symbol with an external *"what should be here"* authority. First built for **Plant Diversity**
(DP1.10058.001): resolve each site's coordinates → its NRCS **Ecological Site** reference plant
community (offline via Soil Data Access), then compare it to what NEON observed.

- **Build-time location→reference-list join frozen to `.rds`.** `scripts/build_expected_lists.R`
  (raw SDA REST — no `soilDB`) writes `data/expected/<SITE>.rds`; the deployed app makes **zero**
  federal API calls. A second build (`build_plant_authority.R`) freezes the USDA PLANTS nativity +
  synonym authority to `data/authority/plants_lookup.rds`. Both are public domain.
- **Three-bucket framing, completeness-NEVER-red as a hard rule:** A confirmed (green) / C
  observed-not-expected = the review lane (clay; split introduced vs native-not-in-reference) / B
  expected-but-absent = completeness (neutral). NEON samples a tiny area, so "expected but absent"
  is non-detection or a real state-transition — frame it as completeness or ecology, **never** as
  missing data or error. The inverse over-claim (treating the reference list as truth the data must
  match) is scientifically wrong.
- **Exact symbol join, not fuzzy.** NEON taxonomy *is* USDA PLANTS, so `taxonID` = the `plantsym`
  SDA returns = the symbol USDA's API takes. Collapse synonyms to the accepted symbol first (else a
  synonym fakes an "unmatched" QC signal). Drop SDA aggregate codes (`2FA`, genus-level) before
  comparing. Run on `species_level_only(latest_snapshot())`, never the year-pooled table.
- **Surface the coarse-ID rate FIRST.** Share of records resolved only to genus/family/kingdom
  frames every other flag (coarse IDs can't match a species-level reference list). Direct count, no
  inference — zero false positives.
- **Publish the match rate + a provenance row** distinguishing fetch-failure from genuine-empty (the
  difference between an honest empty state and a fake "0% detected"). Every name-join ships its rate.
- **Three clickable + downloadable bucket tables + a combined report CSV**, info-dot on every card,
  plain-English framing literally on the page, EDIT citation deep-link (the canonical worked example
  is **SRER** → `R041XC318AZ`). Fast-follows: out-of-range flag once USDA's distribution endpoint is
  pinned; fan-out to all sites; colour the picker markers by MLRA / `% detected`.

---

### Mosquito Pulse — the newest gold-standard bars (audit every app against these)

NEON-Mosquito-Pulse is the newest build and sets bars the older apps don't meet:
1. **Persistent honesty banners** — a synthetic/preview-data banner (red on no-data, yellow on any-synthetic) plus a persistent hero-caveat ("a within-site activity index, not a population"). Any app shipping a preview/synthetic bundle flags it visibly and persistently.
2. **Effort denominator + median (not mean)** — a seasonal/annual index divides by attempted effort (including zero-catch nights) and uses a skew-robust median across years.
3. **Honest richness** — Chao2 with CI + sample-coverage, incidence rarefaction, a "minimum estimate" caveat; refuses false precision at low coverage. Richness shows uncertainty, never a bare point estimate.
4. **Three-tier QC + clickable inspector** — the standard high/warn/info "verify, not wrong" flags, each opening the exact offending rows.
5. **Downloadable codebook + provenance-complete CSV** — exports carry the columns needed to re-derive counts and replay the QC filtering.
6. **Locked data palettes** — genus/sex/category colors are literal R vectors, never CSS tokens (a reserved hue for the disease-vector flag); data-encoding colors stay theme-independent.
7. **Reduced-motion + a11y** — `prefers-reduced-motion` honored on all animation; pin cards keyboard-operable (role/tabindex); decorative SVG `aria-hidden`.
8. **Tab-resize dispatch** — `shown.bs.tab` fires a window `resize` so Leaflet/Plotly in initially-hidden tabs render at the right size (no 0-width widgets).
9. **Scope chips** — persistent site-vs-all-N badges so a view's scope is always labeled.
10. **Cross-site inference** — Spearman rho with CI plus a space-for-time confounding caveat on every cross-site gradient.

Source: `NEON-Mosquito-Pulse/` (`global.R`, `R/mos_helpers.R`, `server.R`, `www/app.js`, `www/pincards.js`). Fold the relevant bar into each app as it is touched.

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
