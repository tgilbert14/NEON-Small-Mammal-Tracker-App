# Girth Index → reusable patterns (Desert Data Labs design playbook)

Extracted from the **Big 12 Girth Index** (`arizona-basketball-talent/`, deployed at
girthindex.desertdatalab.com) by three breakdown agents. This is the house style + the
reusable machinery to port into this app and future Desert Data Labs projects.

## 1. Design system

- **Light theme**, not dark. Soft paper background gradient, **white card surfaces**, a
  three-color brand triad used as **card-header accents**.
- **Brand triad (girth):** navy `#0C234B`, cardinal `#AB0520`, gold `#FFD200`; ink `#1c2733`,
  muted `#6b7a89`, page bg `#eef2f8→#e7edf5`. Per-entity colors live in **one config table**
  with a colorblind-safe fallback (`#0072B2`).
- **Font:** Rubik (Google Fonts), three weights — 400 body, 600 titles, 800 numbers/headings.
- **Card recipe:** 10px radius, soft *navy-tinted* shadow (`rgba(12,35,75,.08)`, never plain
  black), **3px colored top border**, centered title, hover lift (`translateY(-3px)`).
- **Card-header colors carry meaning:** primary (blue/green) = standard, danger (red) =
  featured/alert, warning (amber) = insights/caveats.
- **Approachability:** question-style titles ("who has the biggest bodies?"), em-dash helper
  captions under every chart, a numbered "how-to" strip, plain-language transparency notes
  behind small ⓘ modals, mobile-first, "free · no sign-up."

## 2. Visualization patterns (ranked by reuse value)

1. **Highlight-one-entity-in-a-grey-cloud** — plot everyone in grey, color only the focal
   entity (`role = case_when(...)` → `role_cols`). Works for scatter, bars, lines.
2. **Violin + jittered interactive points + mean diamond** ("Position DNA") — the single best
   body-size visual: grey violins = whole population shape, colored dots = focal group, diamond
   = group mean, median crossbar drawn *under* the points.
3. **Named-quadrant scatter** — median crosshairs split a 2-var scatter into 4 labeled corners
   ("SKYSCRAPERS / GIANTS / JITTERBUGS / BOWLING BALLS") + `ggrepel` labels.
4. **Before/after arrow chart** — first value → latest value per individual, arrowhead + delta
   label. (Maps to recapture weight change.)
5. **Percentile-band trend** — focal trajectory vs population p25–p75 ribbon + dashed median,
   with era/season shading and dot size = sample size.
6. **Icons as markdown axis labels** (`ggtext::element_markdown` + `theme_*_md` incomplete-theme
   trick for ggplot2 4.x) — logos/silhouettes instead of text ticks.
7. **Leaflet footprint map** — smoothed convex hulls (`sf::st_convex_hull` + `smoothr::smooth`),
   custom icon markers, jitter, rich popups.
8. **Reusable hover-card builder** (`top_players_tip`) — ranked "1. Name — 320 lbs" tooltip used
   on every dot so hovers are consistent.

Tech: ggplot2 + **ggiraph** (interactive SVG), leaflet. Visual-only downsampling guard
(`if (nrow>1200) sample`, compute stats on full data). In THIS app we render the same patterns
in **plotly** to keep one library.

## 3. Interaction machinery (pure fn + thin shiny layer + one config + one global reactive)

- **Narrative-insight generator** (`make_talking_points`): a *pure* function returning a
  `character` vector. Each sentence = **compute → rank (`which()` on a sorted board) → `glue`**,
  with `slice_max` for superlatives and a threshold `ifelse` for editorial color ("#N of M …
  Games are won up front"). Every block guarded so missing data drops a sentence. `head(pts, 4)`
  for previews. → ported here as `site_insights()`.
- **Clickable stat card → modal with ranked list:** value box wrapped in `div.vb-link` with an
  `onclick`, or `Shiny.setInputValue('x_request', {...})` → server `observeEvent` (in `tryCatch`)
  → `showModal`/`sendCustomMessage`. One `lapply` registers many info modals from a config list.
  → ported here as the clickable hero stats → `stat_breakdown()` modal.
- **At-a-glance hero card:** a pure `snapshot()` → flat named list **+ deltas vs rolling
  baseline** (returns `NULL` when empty) → thin `renderUI` glues into a grid with a `delta_html()`
  colorizer; the same snapshot feeds the clickable stat boxes so numbers never disagree.
- **Config-driven entity picker → global state:** a logo/marker grid built by `lapply` over a
  config table; each observer does `updateSelectInput(global)` + `nav/tab jump`; **one** global
  reactive is the single source of truth every tab reads; choice persisted to `localStorage`.

## 4. Branding (use verbatim)

- Footer: **"Built by Desert Data Labs · feedback, bug reports, or want something like this
  built for your project? desertdatalabs@gmail.com"** — email link cardinal/accent, weight 600,
  with a subject-prefilled `mailto:...?subject=...`.
- A "✋ want one for your project?" custom-build CTA repeated in 3 places (footer, a dedicated
  feedback box, an inline pill).
- Non-affiliation disclaimer.
- Marketing site: **desertdatalabs.com** (plural). Sister app lives at the **singular**
  girthindex.desertdatalab.com (intentional — both domains owned).
