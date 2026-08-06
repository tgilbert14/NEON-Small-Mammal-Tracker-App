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

### 2b-i. "Search the network" tab — a bundled, precomputed search index (reusable)
Every app with a national picker gets a **Search** `nav_panel` (value `"search"`, placed after the cross-site tab) that queries a SMALL precomputed `.rds`, never a live fetch — so it stays instant on the bundled load.
- **Index builder** `scripts/build_search_index.R` READS the committed bundles (`data/sites/*.rds`), NOT NEON. For a biotic app it writes a tidy **one-row-per-(taxon × site)** table to `data/search_index.rds` with: display name + flair (emoji/nickname/group), site + name/state/domain, the app's **honest within-site MEASURE**, total captures/individuals, and `year_min/year_max`. Compute the measure through the app's OWN cleaned-bundle machinery so it matches the rest of the app — Small Mammal's measure is **peak site-wide MNKA per species** = run `mnka_series()` on `clean_mam(bundle)` filtered to that species, sum per-plot MNKA within each month, take the max month. Filter `species_level_only()` first so the search list matches the richness counts. Tiny: 604 rows / 145 species / 46 sites = **8.2 KB**.
- **Load once at boot** in global.R via `read_bundle("data/search_index.rds")` → `SEARCH_INDEX`; a `search_taxon_choices()` helper builds the grouped, "· N sites" selectize labels (server-side selectize so the list isn't shipped in the page).
- **Two query modes** (a small radio): (a) **Find a taxon** — selectize autocomplete → DT of every site it occurs at (measure + years), sortable, each row a **"Go to this site →"** button; (b) **Threshold query** — the app-specific quantitative ask (Small Mammal: "species recorded at > N sites" and "sites with > X species"), a slider → DT of matching taxa/sites.
- **Go-to-site jump** reuses the EXACT map-click path: the inline button raises `Shiny.setInputValue('searchGoSite', code, {priority:'event'})`, the observer calls `nav_select("tabs","overview")` then the app's `load_site_full(code)` — so the sidebar state/site selectors sync and the load is identical everywhere (no second loader).
- **Honesty:** show the result count ("12 of 46 sites"), an empty state, and a one-line caption that the measure is a **within-site index** + species counts reflect trapping effort — NOT an absolute population and NOT a fair cross-site ranking. (The measure is a SEARCH KEY here, never a leaderboard.)
- **Manifest:** the lean generator's `Sys.glob("data/*.rds")` already sweeps in `search_index.rds` — just re-run `scripts/write_manifest.R` (canonical; keeps `users` + per-file `checksum`, stays lean).

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
- **Never set a bslib theme font via `font_google(...)` on Connect Cloud — it is a cold-start network landmine.** `font_google()` defaults to `local = TRUE`, which **downloads the font from Google's servers and compiles it into the theme AT APP STARTUP (server-side)**. Connect Cloud idles the worker after ~6 min and **wipes the cache on recycle**, so this live fetch runs on **every** cold start against an empty cache; when Google Fonts is slow/unreachable the Sass compile blocks/fails during boot -> black screen + "start-up error", and a manual republish only re-primes the cache until the next recycle (the classic "worked fine, spontaneously broke, republish fixes it, recurs" loop — confirmed in the Connect logs: `Downloading google font Rubik to local cache` immediately before `Stopping server...`). **Fix:** name web fonts as PLAIN CSS families via `bslib::font_collection("Rubik", "system-ui", ..., "sans-serif")` (a serif fallback stack — `"Fraunces","Georgia","Cambria","Times New Roman","serif"` — for Fraunces headings) so the theme compiles offline with ZERO network at boot, and deliver the real glyphs client-side via a non-blocking `tags$link(rel="stylesheet", ...fonts.googleapis.com... &display=swap)` in ui.R. Bit the WHOLE suite at once (SMT, Driver Cascade, Plant Diversity, Phenology, Veg Structure) because every app inherited the same `font_google` chrome — audit any new NEONize for it.
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

### The seasonal-driver card (`R/seasonal_env.R` + `output$seasonalDriver`) — the cascade read, in-app (reusable)

Every env-panel app ships the Driver Cascade's **seasonal-aggregate** driver read as a small `.ec`
card that sits **right under the monthly env-driver ranking** (`uiOutput("seasonalDriver")` after the
`envDriverRank`/`envCorrNote` card). It fixes the blind spot of the monthly scan: the monthly
`env_corr_scan` *deseasonalizes* monthly catch vs monthly driver, so it averages the monsoon out of
the very annual cycle it subtracts. The seasonal card aggregates the driver by SEASON (monsoon
Jul–Sep, winter Oct–Mar, spring temp Mar–May) into **one value per year** and correlates at the
prior's stated lag, at annual resolution.

**Port recipe (verified on Small Mammal + Ground Beetle):**
- Copy `R/seasonal_env.R` **verbatim** from the flagship (it is generic — `seasonal_aggregates`,
  `seasonal_driver_links`, `seasonal_biome`, the permutation + circular-shift nulls, `SEASON_LABELS`).
  Only `SEASON_LABELS` wording is app-tunable; keep `.WATER_LIMITED = c("JORN","MOAB","ONAQ","SRER","YELL")`.
  `source("R/seasonal_env.R", local = FALSE)` in global.R after helpers.R.
- Build the app's ANNUAL response — `data.frame(year=<int>, value=<num>)` — from the app's OWN annual
  metric so the card never disagrees with the rest of the app (Small Mammal: 100·captures/trap-nights
  per year; **Ground Beetle: `annual_trend(d)$cpn`** = catch per 100 trap-nights per year). Derive the
  site code via the app's `rv$siteCode` (or a `mode_chr(siteID)` fallback), `biome <- seasonal_biome(code)`,
  then `seasonal_driver_links(rv$env, resp, biome=biome, lags=…)`.
- **The per-product LAG override is the load-bearing choice.** `lags` carries the stated prior lag per
  driver. Slow seed-eater boom (mammals) = `precip_monsoon = 1L` (next-year crop). **A FAST
  within-season responder (beetles, mosquitoes) = lag 0 on ALL drivers**:
  `lags = c(precip_monsoon = 0L, precip_winter = 0L, temp_spring = 0L)` — beetle activity-density and
  monsoon water act the same year; temperate degree-days → emergence is also same-year. Set the lag
  from the organism's response time, not by copy-paste.
- The card renders the **biome-LEAD** driver first (`links[links$expected,][1,]`): temperate → `temp_spring`,
  water-limited → monsoon/winter. **Honest fallback gotcha:** if a water-limited site has NO testable
  precip season that year-pairs (n<3 after merge), `links[links$expected,]` is empty and the card falls
  back to the full `links` (so `temp_spring` can surface at a desert site — e.g. JORN/MOAB in the beetle
  build). That is correct, not a bug: it means precip wasn't testable there, not that the biome prior flipped.
- Reuse the flagship CSS verbatim: `.ec-seasonal-note`, `.ec-seasonal-caveat` (+ the existing `.ec`
  chrome). Report **r WITH n and the simple per-link p**, show the conservative **season-corrected
  `p_adj`** too, **n-gate** (no p under 6 years), **NEVER deseasonalize the annual aggregate**, and
  always carry the small-n caveat + the link to `https://tgilbert14.github.io/NEON-Driver-Cascade/`.
- Verified leads: SRER → monsoon lag0; HARV/CPER → temp_spring lag0. Source: `NEON-Ground-Beetle-Tracker/`
  (`R/seasonal_env.R`, `server.R output$seasonalDriver`, `ui.R` Seasonality panel, `www/styles.css`).

---

## 6. Deployment & maintenance — the full lifecycle (dev → deploy → self-update)

The suite has **migrated off shinyapps.io to Posit Connect Cloud with a GIT-BACKED deploy**.
This is now the standard; the Small Mammal reference app's retired shinyapps records are absent.

**Deploy model (the new standard — Connect Cloud, git-backed):**
- The app lives on Connect Cloud, pointed at the GitHub repo + its watched production branch.
  **A reviewed merge to the watched branch is the production decision** — it makes that exact
  revision available to Connect, but the deployment is not proven until Connect republishes it.
  Build and data automation must publish an immutable candidate to a restricted review
  branch and open/update a PR; it must never push directly to production. There are **no
  shinyapps.io secrets, no `rsconnect/` dir, and no `deploy.R` step** (those are the legacy path).
- Required in-repo: a lean **`manifest.json`** (`rsconnect::writeManifest()`; bundle-only, keep
  `neonUtilities` OUT via the computed-package-name trick), the committed `data/` bundles, and a
  `docs/index.html` GitHub Pages showcase whose single primary CTA points at the live Connect app.
- Branch naming is split across the suite (`main` vs `master`) — each workflow must target a
  dedicated review branch whose PR merges into the branch its Connect app watches. Standardize new
  repos on `main`, with branch protection and required release gates.

**The `manifest.json` is a deploy GATE — the terra/GDAL landmine (ask `Connor`, the deploy expert):**
A normal code/data push deploys fast, but a WRONG manifest blocks the WHOLE publish (`Failed to
publish content`). Connect Cloud **compiles packages from SOURCE** on its **jammy image (system GDAL
3.4.1)** — RSPM binaries are NOT reliably used, so a package must *compile* there.
- **The killer:** `leaflet → raster → terra`, and **terra ≥ 1.8-54** ships `gdal_multidimensional.cpp`
  calling the 3-arg `GDALMDArray::AsClassicDataset` (a **GDAL 3.8** overload, unguarded in releases) →
  `compilation failed for package 'terra'` on GDAL 3.4.1. terra's multidim support landed in **1.8-54
  (2025-06-01)**.
- **The first fix:** pin **`terra` to `1.8-50`** (last
  release before 1.8-54). It compiles on GDAL 3.4.1 and still satisfies `raster 3.6-32`'s
  `terra (>= 1.8-5)`, so leaflet/raster are untouched. terra/raster are **install-only** (the app uses
  leaflet for maps and never calls them) → **zero runtime impact**. Surgical: terra's version appears
  twice in the manifest (Version + RemoteSha), but those fields must describe the package actually
  installed; text substitution is not proof of a reproducible build.
- **The complete fix:** CI installs the known-good eight-package geospatial closure from exact source
  tarballs (`terra`, `sf`, `s2`, `units`, `wk`, `classInt`, `raster`, `sp`) under pinned R, then
  `write_manifest.R` records and verifies the actual installed versions. Ordinary packages use one
  dated Posit snapshot. Commit only the validator-produced manifest artifact; never hand-edit
  `Version`, `RemoteSha`, `Repository`, or `platform` to make a gate pass.

**Reliable-redeploy discipline (run on EVERY app change):** Connect can't deploy what doesn't build,
so the manifest must round-trip whenever the package set changes:
1. **Did this change add/remove a `library()`/`pkg::` (a new feature pulling a new package, a dep
   bump)?** NO → keep the committed manifest and run the release gates. YES → regenerate it.
2. **Regenerate from a CLEAN, fully-committed tree** (`writeManifest()` filesystem-scans — never
   mid-WIP): `Rscript scripts/write_manifest.R`.
3. **Verify before commit:** parses · lean (`neonUtilities`/`arrow` absent) · **`terra` == 1.8-50** ·
   leaflet chain present · the new package present · `plotly` ⇒ `data.table` present.
4. **Commit the validator-produced manifest in the SAME PR as the code**, then merge only after the
   exact candidate passes every release gate.
5. **Confirm the Connect build goes GREEN and semantic smoke passes.** A failed publish can leave the previous good build
   serving, so "the app still loads" is NOT proof your change shipped — verify the new element is live.

**The manifest gate becomes a MERGE LOOP when the agent has no local R — kill it, don't grind it:**
The redeploy discipline above assumes you can run `Rscript scripts/write_manifest.R` locally. A cloud
agent (ChatGPT/Codex, a generic sandbox) usually CAN'T — Connect's toolchain is jammy **GDAL 3.4.1**
plus the pinned geo closure, which isn't on a stock runner. With the CI gate
(`git diff --exit-code -- manifest.json`, `permissions: contents: read`), every runtime edit then
forces: push → CI fails the gate BY DESIGN → download the **validated** `*-manifest-<sha>` artifact
(never the `-UNVALIDATED-` one) → commit it byte-for-byte → re-run the exact head → merge. With
`concurrency: cancel-in-progress: true`, any quick re-push *cancels* the run, and each run
source-compiles terra/sf (45–100 min timeouts). This IS the "failed merge over and over" the ChatGPT
cover rework hit — the flagship's cover took ~10 PRs, and veg's single Living-Poster PR burned **13 CI
runs**. It is process-by-design, not a git error (all those PRs did merge). Two ways out:
- **Make the gate byte-DETERMINISTIC** so a promoted manifest stays green. In `write_manifest.R`: strip
  each source-built package's wall-clock `Built` field; canonicalize the geo pins to the deployable lane
  (`Source`="CRAN", `Repository`="https://cran.r-project.org"); freeze floating RSPM aliases by
  **targeted text-substitution** (`readLines`/`gsub`/`writeLines`) — NEVER a `jsonlite` reserialize (it
  mangles `writeManifest`'s canonical format AND destroys the exact `url::` tarball refs in
  `RemotePkgRef`); pin `platform`=4.5.2 and `locale`="C". Then CI can "regenerate twice, require identical
  bytes" and a faithful promotion stops flapping. A read-only `verify_manifest.R` twin (re-derive the
  appFile closure, check per-file md5, assert no `Built`) makes "committed == regenerated" enforceable
  without granting write.
- **Kill the manual round-trip** (owner decision): let the pinned validator WRITE its own output back — a
  deliberate `workflow_dispatch` "regenerate & commit manifest" job — or provision the agent env with the
  pinned R+GDAL-3.4.1 image. Do **not** auto-commit on every PR: that breaks the "write only to the final
  restricted publisher" boundary. Reference implementation in the flagship:
  `.github/workflows/regenerate-manifest.yml` (manual dispatch, job-scoped `contents: write`, refuses
  `main`, regenerate-twice determinism guard, `verify_bundle.R` before it commits). Also settle the default branch — the suite is split `main`/`master`
  (Driver-Cascade is `master`); rename it to `main` or an agent that assumes `main` hits base-branch push
  failures on one repo in three.

**Auto-refresh + reviewed release (`.github/workflows/refresh-data.yml`) — copy this shape:**
- **Schedule (identical across the suite):** `cron: "0 6 * * 0"` (Sunday 06:00 UTC = Saturday 23:00
  America/Phoenix, off-peak), with a **gate job** that proceeds only on the **first Saturday of the
  month** (`dow=6 && day<=7`, `TZ=America/Phoenix`) — cron can't say "first Saturday", so fire weekly
  and gate. `workflow_dispatch` with a `skip_download` input always proceeds (fast redeploy test).
- **Flow:** gate → checkout → pinned R + declared dependencies → fetch raw into an empty stage →
  rebuild bundles and indexes → verify exact site/schema/index/checksum/manifest/offline contracts →
  publish the immutable candidate to a restricted review branch → open/update a PR. Heavy optional
  enrichments may degrade honestly, but required scientific and release gates must fail closed.
  `NEON_TOKEN` is optional (anonymous access works more slowly).
- **Deploy trigger:** an intentional PR merge to the Connect-watched branch. Direct automation pushes
  to a production branch are prohibited; the review boundary is part of data provenance.

**Derived/master apps (e.g. Driver Cascade):** their bundle is built FROM sibling repos' bundles, so
CI must obtain them — `git clone --depth 1` each sibling repo (use the real slugs, not dir names:
NEON-Small-Mammal-Tracker-App, NEON-Plant-Diversity, NEON-Breeding-Birds,
NEON-Plant-Phenology-Explorer, NEON-Vegetation-Structure-Explorer, NEON-Ground-Beetle-Tracker),
copy their `data/`, run the build script, commit the derived `.rds`. A master app needs a **GitHub
remote + a Connect Cloud app** before any of this works.

## 7. Per-app readiness checklist (audit every app against this)

Data bundles: `data/sites/*.rds` present + valid (loadable, non-empty) · `data/site_index.rds`
(picker) · `data-sample/demo.rds` (instant demo) · all git-tracked · refreshed within the cadence.
Automation: `.github/workflows/refresh-data.yml` on the **standard schedule** · publishes a verified
candidate to a restricted review branch + PR · never pushes production directly · `manifest.json`
present + lean (no `neonUtilities`/`arrow`) + the **entire geospatial closure** pinned and truthfully
recorded (Connect Cloud compile gate — see §6) · GitHub **remote** exists ·
`docs/index.html` primary CTA is live + the **Connect build is GREEN** (a failed publish serves the old build). NEONization: cover/landing splash · **in-app sibling links** + one prominent Driver route on companion `docs` covers · mobile-responsive CSS (`@media`, prefers-reduced-motion) · **QC-flag
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
github.io showcase URL · live Connect Cloud URL). Driver Cascade is the suite ambassador: every
companion cover gets one prominent Driver route instead of repeating the full directory. Render the
complete registry in the in-app About/footer and on Driver. When a new app ships, update that registry
so Driver and every in-app sibling directory can reach it.

**Suite Living Poster V1:** every companion Pages cover uses the same structural frame:
focusable skip target; DDL topline plus one Driver route; a 3–7-word hook; one <=12-word
promise; one contextual CTA; one dominant app-specific editorial image in a responsive
`picture` with meaningful alt text and durable provenance; no ornamental illustration
badge; and a compact scope/honesty/Source/Feedback footer.
No metrics, methods band, feature cards, second CTA, or full companion directory. Reuse
the grammar, not a clone: each app owns its palette, motif, copy, focal crop, and scientific
limitation. Carry a compact echo of the same hook, promise, art authority, Driver route,
and action into the functional app without replacing its task flow. Validate both surfaces
at desktop, 390px, and 320px using the actual framework gutters and complete persistent
controls; record the exact Pages artifact and Connect deployment commit before calling the
cover shipped.

**Enforce the frame mechanically — don't just prescribe it.** Ship `scripts/check_cover.mjs` (Pages)
and `scripts/check_in_app_landing.mjs` (the Connect echo) as zero-dependency Node CI contracts that
assert the frame instead of trusting it: exactly one `<h1>`/`<main>`, the skip link and `aria-label`'d
Driver nav, the single hook/promise/CTA, the full OG/Twitter set incl. the 1200×630 card, NO
`fetch()`/`http:`/Google-fonts at load, 44px+ touch targets, and the `prefers-reduced-motion` /
`prefers-contrast` / `forced-colors` seams — plus **asset integrity**: each image's SHA-256 cross-checked
against `docs/IMAGE-PROVENANCE.md`, with self-parsed PNG/JPEG/WebP dimensions and byte budgets so an art
swap can't silently regress. Require `docs/IMAGE-PROVENANCE.md` as a hash-linked artifact (per-asset
tool·date·prompt·reference·dims·bytes·SHA-256; AI-generated vs third-party attribution kept separate;
retired assets quarantined; an explicit "the art carries no scientific value — facts stay selectable
HTML" boundary). Asset hygiene: a PNG master + full/compact WebP + the 1200×630 og, byte-identical in
`docs/assets` (Pages) and `www/assets` (Connect); **no Git LFS** — budgets live in the contract test, and
any art/copy/crop change is ONE coordinated PR touching the images, both hash pins, and the provenance
record together (binaries can't auto-merge). This turns the two `.mjs` files into the reusable "Cover
Contract" every companion inherits with the Living Poster frame.

**Motion, the scroll-video ideas, per-app divergence, and the ChatGPT + Higgsfield asset pipeline** live in
`docs/COVER-MOTION-KIT.md` — the rough *starting kit* a subagent builds a cover from and then alters per
project (the poster archetype is the front-door default; covers can also be immersive-scroll-led or
data-led depending on the subject — the owner does not want them to look the same).

## 8. Working across agents (Claude + Codex)

This suite is built by more than one agent — Claude Code and ChatGPT/Codex — often on the same repo,
switching back and forth. They can't share live memory, so they collaborate through **durable
artifacts in the repo**, and that already works if you keep four things true:

- **One source of truth, two front doors.** `CLAUDE.md` (Claude) and `AGENTS.md` (Codex) stay thin;
  the real content lives in tool-neutral docs — this playbook, `.claude/agents/LESSONS.md`,
  `docs/BUILD-TEST-HANDOFF.md`, `docs/project-status.md`. Whichever agent boots cold lands in the same
  context. Keep the two front doors pointing here, not duplicating it (and note the split: small-mammal
  carries both files; veg/driver have only `AGENTS.md` — add `CLAUDE.md` there so Claude boots warm too).
- **The handoff log IS the async channel.** `docs/BUILD-TEST-HANDOFF.md` is the dated, append-only
  ledger both agents must close a session with. Tag each entry with the tool (`[Claude]` / `[Codex]`)
  and end it with a one-line **next action**, so a tool switch is a clean pickup, not a cold restart.
- **Cross-review, don't self-review.** Different model families have different blind spots, so the
  highest-value review is the *other* vendor's agent on your diff — extend the "adversarially verify
  the diff with a fresh agent" rule to "with the other vendor's agent." When Codex opens a PR, have
  Claude review it; when Claude opens one, have Codex review.
- **Let the contracts be the trust layer.** The `check_*.mjs` / `verify_*.R` gates and the manifest
  determinism gate (§6) are tool-agnostic referees — neither agent has to trust the other's self-report;
  CI enforces the invariants objectively. Invest in contracts, not agent-specific trust.
- **Know the map — every repo, both tools.** Each suite repo carries the same front door: `CLAUDE.md`
  (Claude) + `AGENTS.md` (Codex) pointing at the same tool-neutral docs, plus a project-local
  `.claude/agents/LESSONS.md`. **Branch defaults are split and must never be assumed:** Small Mammal and
  Vegetation deploy from **`main`**; **Driver-Cascade deploys from `master`** (Connect Cloud watches it —
  documented, not renamed, because a rename must repoint the external Connect setting first). Driver's
  release gate is *semantic* (`compare_manifests.R`), while the byte-exact siblings use `git diff` — worth
  cross-pollinating (see §6).

Rough division by strength (not a rule): Codex for tight visual/UX/prototype iteration; Claude for
cross-repo diagnosis, science-contract/honesty review, Driver synthesis, and learning-loop upkeep —
with the handoff log carrying state between them.

## 9. How we work (principles for every session)

These govern every session in either tool; they outrank any single feature.

- **Plan, question, and challenge before you code.** Before anything non-trivial: restate the goal in
  your own words, surface the load-bearing assumptions, name the risks and the blast radius, and ask
  whether this is even the right thing to build. A short written plan plus the one sharp question beats
  charging in — a wrong assumption caught in planning is free; caught in a deploy it is not. Reversible
  and well-scoped -> proceed. Risky, outward-facing, or ambiguous (a watched-branch change, a rename, a
  contract/schema edit, anything touching production bytes) -> plan it and confirm first. Standing
  preference, not a per-task ask.
- **Promote proven patterns into skills — curator-gated.** When a pattern earns it (used across >=2 apps,
  or it survived a real reviewed build — not just once in theory), don't leave it buried in one repo:
  propose it to `curator`, who vets and names it and, via the `skill-creator` skill, turns it into an
  invokable skill and promotes it to canonical `TG-Data-Apps` so the whole suite and both tools can call
  it. Bar = proven + reusable; gate = curator approval, so the skill set stays curated, not cluttered.
  (The Cover Contract §7 and the manifest-determinism recipe §6 are the first two candidates.)
- **Know your tools; suggest new ones.** Start substantive work by taking stock of what's available —
  skills, subagents, MCP tools, workflows — and pick the right one instead of hand-rolling. When a task
  would be better served by a tool we don't have, or a newly-available one fits, say so by name and say
  what it would do. A capability we forget we have is one we don't have.
- **Always propose an improvement.** Close substantive work by naming at least one concrete improvement
  you noticed — even out of scope, even small. Ideas are cheap; the only cost is forgetting to say them.
  Log the durable ones to `LESSONS.md` / the handoff so they aren't lost.

---

*Living doc. Plant-diversity (DP1.10058.001) was the first full NEONize; birds/phenology/veg/cascade
followed. §6–9 now encode the review-branch release boundary, pinned-build evidence, the shared
off-peak schedule, the QC-flag generalization, cross-agent working, and the how-we-work principles.
Keep the suite's canonical playbook and each app-local copy synchronized whenever these contracts change.*
