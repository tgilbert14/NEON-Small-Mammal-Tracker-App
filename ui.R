# ===========================================================================
# NEON Small Mammal Tracker — ui.R
# A dark "night-field observatory" dashboard for chasing individual rodents
# across the National Ecological Observatory Network.
# ===========================================================================

# `spin()` and `info_pop()` now live in global.R so server.R can use them too.

# a card header with the title on the left and an info popover pushed right
card_head <- function(icon, title, ...)
  bslib::card_header(class = "with-info", bsicons::bs_icon(icon),
                     tags$span(class = "ch-title", " ", title), ...)

# one rarity-tier badge (used in the visible legend + popovers)
tier_badge <- function(label) {
  m <- rarity_meta(label)
  tags$span(class = "tag-badge",
    style = sprintf("background:%s;border-color:%s;color:#fff", m$color, m$color),
    paste(m$icon, label))
}

# the rarity key, reused in the legend strip and the help dialog
rarity_key_items <- list(
  c("Legendary", "15+ captures"), c("Epic", "10–14"), c("Rare", "6–9"),
  c("Uncommon", "3–5"), c("Common", "1–2")
)

ui <- bslib::page_fillable(
  theme = app_theme,
  window_title = "NEON Small Mammal Tracker",
  fillable = FALSE,

  # ---- head: fonts, icons, libs, our CSS/JS ------------------------------
  tags$head(
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = NA),
    tags$link(rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Rubik:wght@400;500;600;700;800&display=swap"),
    tags$link(rel = "stylesheet", href = "https://cdn.jsdelivr.net/npm/sweetalert2@11.10.0/dist/sweetalert2.min.css"),
    tags$script(src = "https://cdn.jsdelivr.net/npm/sweetalert2@11.10.0/dist/sweetalert2.all.min.js"),
    tags$script(src = "https://cdn.jsdelivr.net/npm/canvas-confetti@1.9.2/dist/confetti.browser.min.js"),
    tags$link(rel = "stylesheet", href = "https://cdn.jsdelivr.net/npm/driver.js@1.3.1/dist/driver.css"),
    tags$script(src = "https://cdn.jsdelivr.net/npm/driver.js@1.3.1/dist/driver.js.iife.js"),
    tags$script(src = "https://cdn.jsdelivr.net/npm/html-to-image@1.11.11/dist/html-to-image.js"),
    tags$link(rel = "stylesheet", href = asset_url("styles.css")),
    tags$script(src = asset_url("app.js")),
    tags$script(src = asset_url("pincards.js")),
    tags$script(src = asset_url("confirm.js"))
  ),
  useShinyjs(),

  # ---- persistent top control bar (theme + help) -------------------------
  # The sidebar is gone in v2: the picker map IS the way to select a site, and
  # its date window now lives on the landing (see the select panel inside the
  # splash). The two controls that have to stay reachable everywhere — the theme
  # toggle and the help dialog — sit in this slim top-right bar above the hero.
  div(class = "top-bar",
    div(class = "top-bar-brand",
      tags$span(class = "tb-mark", "\U0001F43E"),
      tags$span(class = "tb-title", "Small Mammal Tracker")),
    div(class = "top-bar-actions",
      actionButton("help", tagList(bs_icon("question-circle"), " How it works"),
                   class = "btn-outline-dark btn-sm tb-help"),
      div(class = "tb-theme",
        tags$span(class = "tb-theme-lab", bs_icon("circle-half")),
        input_dark_mode(id = "colorMode", mode = "dark")))
  ),

  # ---- full-screen loading overlay (shown client-side on Load click) -----
  div(id = "loadOverlay", class = "load-overlay",
    div(class = "load-card",
      div(class = "load-spin mascot-spin", MASCOT_MOUSE),
      div(class = "load-title", "Loading site data"),
      div(id = "loadSite", class = "load-site"),
      div(class = "load-bar"),
      div(class = "load-note",
          "Building the leaderboard, maps, and charts. Live NEON downloads can take up to a minute.")
    )),

  # ---- main: hero stats + tabs -------------------------------------------
  # (The big gold banner now lives INSIDE the splash/landing — output$splash —
  #  so it shows only on site selection and frees room for the loaded view,
  #  which has its own context bar.)
  uiOutput("heroStats"),

  # "Track an individual" picker — was in the sidebar, now a hidden body block
  # revealed on site load (server: shinyjs::show("indivPickerWrap")). Same ids
  # (indiv, surpriseBtn, bioLinks) so the server logic is untouched.
  hidden(div(id = "indivPickerWrap", class = "indiv-picker-wrap",
    div(class = "ipw-row",
      div(class = "ipw-sel",
        selectizeInput("indiv", label = tagList(bs_icon("search"), " Track an individual"),
                       choices = NULL, width = "100%",
                       options = list(placeholder = "Pick a tagID…"))),
      actionButton("surpriseBtn", tagList(bs_icon("dice-5-fill"), " Surprise me"),
                   class = "btn-outline-dark ipw-surprise")),
    uiOutput("bioLinks")
  )),

  # idle splash before any data is loaded
  # National site-picker splash — built STATICALLY here, not via a server
  # renderUI. The picker map is a leaflet htmlwidget; when it was delivered
  # inside a renderUI it failed to bind on Connect Cloud (the dynamic
  # dependency-deliver → re-bind → shiny:value race), so shinycssloaders spun
  # forever. A STATIC leafletOutput gets its deps in the page <head> at first
  # paint and binds reliably. SITE_INDEX / SPECIES_RANGES / GENUS_GROUPS /
  # species_choices() are globals (global.R, sourced before ui.R). Visibility is
  # toggled via shinyjs show/hide("splash"); the server keeps output$pickerMap
  # + its leafletProxy marker swaps.
  div(id = "splash",
    div(class = "splash-guide",
      div(class = "sg-bubble", "Pick a site to start!"),
      div(class = "sg-mascot", MASCOT_MOUSE)),
    local({
    idx <- SITE_INDEX
    if (is.null(idx) || nrow(idx) == 0) {
      div(class = "splash",
        div(class = "app-hero app-hero-splash",
          h1(class = "app-title", "NEON Small Mammal Tracker",
             span(class = "title-tag", "unofficial")),
          p(class = "app-subtitle",
            "Meet the small mammals NEON catches across the country: what lives where, who the regulars are, and what eight years of capture records reveal.")),
        p("The site map could not load in this deployment. Reload the page to try again."))
    } else {
      g_order <- vapply(GENUS_GROUPS, function(g) g$key, character(1))
      grps <- unique(idx[, c("group_key", "group_label", "group_color")])
      grps <- grps[order(match(grps$group_key, g_order)), ]
      legend <- div(class = "picker-legend",
        tags$span(class = "pl-label", "Most-caught family:"),
        lapply(seq_len(nrow(grps)), function(i)
          tags$span(class = "pl-item",
            tags$span(class = "pl-dot", style = sprintf("background:%s", grps$group_color[i])),
            grps$group_label[i])))
      ord <- idx[order(idx$name), ]
      fallback <- tags$details(class = "picker-list",
        tags$summary(class = "picker-list-summary",
          tags$span(class = "pls-label", bs_icon("list-ul"),
                    tagList(" Browse all ", nrow(ord), " sites")),
          tags$span(class = "pls-chevron", bs_icon("chevron-down"))),
        div(class = "picker-list-grid",
          lapply(seq_len(nrow(ord)), function(i)
            tags$a(class = "picker-list-link", href = "#",
              onclick = sprintf("smtLoadStart('%s \\u00b7 loading\\u2026');Shiny.setInputValue('pickFromList','%s',{priority:'event'});return false;",
                                gsub("'", "\\\\'", ord$name[i]), ord$site[i]),
              tags$b(ord$site[i]), sprintf(" · %s ", ord$name[i]),
              tags$span(class = "pll-meta", sprintf("%s · %s caps", ord$state[i], format(ord$captures[i], big.mark = ",")))))))
      has_species <- !is.null(SPECIES_RANGES) && nrow(SPECIES_RANGES) > 0
      div(class = "splash splash-map",
        div(class = "app-hero app-hero-splash",
          h1(class = "app-title", "NEON Small Mammal Tracker",
             span(class = "title-tag", "unofficial")),
          p(class = "app-subtitle",
            "Meet the small mammals NEON catches across the country: what lives where, who the regulars are, and what eight years of capture records reveal.")),
        p("NEON live-traps small mammals at ", tags$b(nrow(idx)), " field sites across the U.S. and Puerto Rico. ",
          "Tap a dot on the map to pick a site, or switch to ", tags$b("by species"),
          " to see where one animal turns up across the country. ",
          "You can also pick from the list below the map."),
        if (has_species) div(class = "picker-mode",
          radioButtons("pickMode", NULL, inline = TRUE,
            choiceNames = list(tagList(bs_icon("geo-alt-fill"), " By site"),
                               tagList(bs_icon("bezier2"), " By species")),
            choiceValues = c("site", "species"), selected = "site")),
        conditionalPanel("input.pickMode != 'species'", legend),
        if (has_species) conditionalPanel("input.pickMode == 'species'",
          div(class = "range-controls",
            selectizeInput("rangeSpecies", label = NULL, width = "100%",
              choices = species_choices(),
              options = list(placeholder = "Pick a species to map its range…")),
            uiOutput("rangeSummary"))),
        # recently-viewed tap-chips (delighter): rendered server-side from the
        # localStorage ring buffer, sitting just above the picker map so a
        # returning visitor can jump straight back without hunting the map.
        uiOutput("recentsStrip"),
        div(class = "picker-map-wrap",
          spin(leafletOutput("pickerMap", height = "560px"), img = "rat1.gif"),
          div(class = "picker-map-hint", bs_icon("hand-index-thumb"),
              " Drag to pan · scroll to zoom · Alaska & Puerto Rico are out there too")),

        # ---- relocated select panel (was the sidebar) ----------------------
        # The same input ids the server's cascade + load path depend on
        # (stateSel, site, dateRange, provisional, loadBtn). Tapping a dot is the
        # primary path; this panel is the by-name path and the place to narrow
        # the date window before loading. Same ids, so server.R is untouched.
        div(class = "select-panel",
          div(class = "sp-head", bs_icon("sliders"),
              " Or pick a site by name, and set the date window"),
          div(class = "sp-row",
            div(class = "sp-field",
              selectInput("stateSel", label = tagList(bs_icon("geo-alt-fill"), " State"),
                          choices = NULL, width = "100%")),
            div(class = "sp-field",
              selectInput("site", label = tagList(bs_icon("pin-map-fill"), " Site"),
                          choices = NULL, width = "100%")),
            div(class = "sp-field sp-field-date",
              dateRangeInput("dateRange", label = tagList(bs_icon("calendar3"), " Date window (all years by default)"),
                             format = "yyyy-mm", startview = "year",
                             start = as.Date("2013-01-01"), end = Sys.Date()))),
          uiOutput("siteBio"),
          div(class = "prov-toggle",
            checkboxInput("provisional",
              tagList("Include ", tags$b("provisional"), " (newest, unpublished) data"), value = FALSE),
            div(class = "prov-hint", "Off = the curated bundle (instant). On = a live fetch that adds NEON's latest provisional records.")),
          actionButton("loadBtn", tagList(bs_icon("globe-americas"), " Explore this site"),
                       class = "btn-primary btn-lg load-btn sp-load", onclick = "smtLoadStart()")),

        div(class = "picker-actions",
          actionButton("compareBtn", tagList(bs_icon("bar-chart-steps"), " Compare two sites"),
                       class = "btn-outline-dark btn-lg")),
        div(class = "picker-tour",
          tags$a(href = "#", onclick = "smtTour();return false;",
                 bs_icon("signpost-2"), " Take a 30-second tour")),
        fallback)
    }
  })),

  div(id = "mainTabsWrap", class = "main-tabs-wrap",
    navset_card_tab(id = "tabs",

      nav_panel(
        title = tagList(bs_icon("compass"), " Overview"),
        value = "overview",
        # quick-jump buttons to the best parts (Girth-style home navigation)
        div(class = "home-nav",
          actionButton("goPopulation", tagList(bs_icon("graph-up-arrow"), div("Population"),  tags$small("abundance & richness")),       class = "home-btn"),
          actionButton("goCommunity", tagList(bs_icon("bar-chart-line-fill"), div("Community"), tags$small("who's out there & when")),   class = "home-btn"),
          actionButton("goDossier", tagList(bs_icon("person-vcard"),   div("Track an animal"), tags$small("open any one's dossier")),    class = "home-btn home-btn-star"),
          actionButton("goSizeLab", tagList(bs_icon("rulers"),         div("Size Lab"),       tags$small("body-size map + QC cards")),  class = "home-btn"),
          actionButton("goRange",   tagList(bs_icon("fire"),           div("Home range"),     tags$small("heatmap + replay of a star")), class = "home-btn"),
          actionButton("goMap",     tagList(bs_icon("map-fill"),       div("Plot map"),       tags$small("species across the site")),  class = "home-btn"),
          actionButton("goFame",    tagList(bs_icon("trophy-fill"),    div("Hall of Fame"),   tags$small("rank every individual")),      class = "home-btn")
        ),
        # lead with the awesome plot — species composition, most common first
        card(full_screen = TRUE,
          card_head("collection", "Species of this site, most common first",
            info_pop("Species composition",
              p("How many ", tags$b("captures"), " (bar length) and ", tags$b("individuals"), " (label) each species contributed."),
              p("Brighter bars = caught more times per animal (a \"trap-happy\" species)."))),
          uiOutput("speciesBarInsight"),
          div(class = "chart-hint", bs_icon("hand-index-thumb"), " Tap a bar for that species' full breakdown."),
          spin(plotlyOutput("speciesBar", height = "440px"))),
        # the plain-English story sits underneath
        card(
          card_head("stars", "The story so far",
            info_pop("The story so far",
              p("Written automatically from the live data for this site & window, the same numbers the rest of the app is built on."))),
          uiOutput("siteInsights")),
        h4(class = "section-title", bs_icon("binoculars"), " Who you'll meet"),
        uiOutput("meetLocals")
      ),

      nav_panel(
        title = tagList(bs_icon("graph-up-arrow"), " Population"),
        value = "population",
        div(class = "tab-head",
          div(class = "tab-head-text",
            h4("Defensible population signals"),
            p("Minimum Number Known Alive (MNKA) and catch-per-unit-effort are honest abundance indices; the accumulation curve shows whether trapping ran long enough to find every species."))
        ),
        conditionalPanel("output.hasEnv == true",
          card(full_screen = TRUE,
            card_head("bar-chart-steps", "Which environmental driver does this population track best?",
              info_pop("Driver comparison",
                p("For every co-located driver, we scan lags 0–12 months and keep the ", tags$b("strongest correlation"), " with monthly catch-per-effort."),
                p("Bars show that best correlation (sign = direction); the label is the lag at which it peaks. The longest bar is the signal this population follows most closely; the others are candidate co-drivers."),
                p(tags$b("Reading the r-value:"), " r runs from -1 to +1 and measures how tightly two things move together: near the ends they rise and fall in near-lockstep, near 0 they barely relate. The ", tags$b("sign is the direction"), ": “+” means more driver → more animals, “−” means more driver → fewer."),
                p("It is ", tags$b("not"), " the percentage of the population explained; that is ", tags$b("r²"), ", the square of r (so r = -0.50 means about a quarter, 0.25, of the month-to-month swings line up). With only a handful of months and many lags scanned, read r as a ", tags$b("lead worth a closer look"), ", not a proven cause."),
                p(class = "pop-caveat", bs_icon("exclamation-triangle"),
                  " A longer bar isn't proof of cause: drivers correlate with each other, and scanning many lags can flag a strong match by chance. Treat this as a ranking of leads to investigate."))),
            uiOutput("envCorrNote"),
            spin(plotlyOutput("envDriverRank", height = "300px")),
            uiOutput("seasonalDriver"),
            div(class = "env-pick-hint",
              bs_icon("arrow-down-circle"),
              " Pick a driver just below to overlay it on the ", tags$b("population trend"),
              " and watch the abundance rise and fall with it."))),
        # "Compare with environment" lives right above the abundance chart it
        # overlays (moved off the sidebar, then down here next to its target).
        # Hidden until the site has env data; choices + show/hide driven by id.
        hidden(div(id = "envPickerWrap", class = "env-pop-bar",
          div(class = "env-pop-row",
            div(class = "env-pop-sel",
              selectInput("envLayer",
                label = tagList(bs_icon("cloud-drizzle-fill"), " Compare with environment",
                  info_pop("Environmental overlays",
                    p("Overlay a co-located NEON data product, measured at ", tags$b("this same site"),
                      ", behind the population & seasonality charts to see what the booms and busts track."),
                    tags$ul(
                      tags$li(tags$b("Precipitation"), ": the rain pulse that feeds desert seed crops"),
                      tags$li(tags$b("Air temperature"), ": thermal limits on activity & plant growth"),
                      tags$li(tags$b("Plants flowering"), ": the annual seed-crop precursor granivores key on"),
                      tags$li(tags$b("Green-up (leaf-out)"), ": forage & cover, and an early precip-pulse proxy"),
                      tags$li(tags$b("Plants fruiting"), ": a near-direct food-supply signal (mast at forest sites)")),
                    p("Picking a layer shows it at its ", tags$b("most-correlated lag"), " by default; use the ", tags$b("lag"), " slider to shift the driver forward in time and explore other lags: a rain pulse can take months to become a rodent boom (the classic desert ", tags$em("pulse–reserve"), " response; Noy-Meir 1973; Brown & Ernest 2002)."),
                    p(class = "pop-caveat", bs_icon("exclamation-triangle"),
                      tags$b(" These overlays show correlation, not proof of cause."),
                      " Drivers are often correlated with each other (warm months are also dry months), so read a strong match as a lead to investigate, not a settled mechanism."))),
                choices = c("None" = "none"), width = "100%")),
            div(id = "envLagWrap", class = "env-pop-lag",
              sliderInput("envLag", tagList(bs_icon("hourglass-split"), " Lead time (months)"),
                          min = 0, max = 12, value = 0, step = 1, width = "100%"),
              div(class = "env-lag-hint", "0 = same month · 3 = driver 3 months earlier"))),
          uiOutput("envSourceNote")
        )),
        card(full_screen = TRUE,
          card_head("incognito", "Detection-corrected abundance",
            info_pop("Detection-corrected abundance", placement = "left",
              p("Traps miss animals. On NEON's ", tags$b("multi-night bouts"), " (pathogen grids run ~3 nights in a row), the ", tags$b("recaptures"), ", animals caught more than once in the same bout, tell us how many we ", tags$em("didn't"), " catch, so we can estimate the true number present."),
              p("The ", tags$span(style="color:#0C234B;font-weight:700", "navy line + band"), " is the estimated abundance N̂ with a 95% interval; the ", tags$span(style="color:#6b7a89;font-weight:700", "grey line"), " is MNKA (minimum known alive). The gap between them ", tags$em("is"), " the detection correction."),
              p("When a driver leads this population (per the ranking above), its monthly value is overlaid here at that best lag (right axis) so you can watch the abundance track it."),
              tags$ul(
                tags$li(tags$b("k ≥ 3 nights"), " → Schnabel estimator; ", tags$b("k = 2"), " → Chapman. Single-night grids can't be estimated (that's what MNKA/CPUE are for)."),
                tags$li("p̂ is the ", tags$b("per-night detection probability"), " (Model M0): the share of present animals we'd expect to catch on any one night."),
                tags$li("We hide the estimate when there are ", tags$b("fewer than 3 within-bout recaptures"), ", too few to be stable."),
                tags$li("N̂ is ", tags$b("animals on the sampled grid(s)"), ", ", tags$em("not"), " a per-hectare density; a true density needs spatially-explicit capture-recapture (SECR), which a single 10×10 grid per plot can't support, so we deliberately decline it.")),
              p(tags$em("The math assumes the population didn't change over the bout and every animal is equally catchable; real animals aren't, so read this as a defensible index, not a census. Schnabel 1938; Chapman 1951; Otis et al. 1978."))),
            div(class = "detect-head-right",
              conditionalPanel("output.detectTempActive == true",
                div(class = "temp-unit-toggle", title = "Temperature units",
                  radioButtons("tempUnit", NULL, c("°F" = "F", "°C" = "C"),
                               selected = "F", inline = TRUE))),
              tags$span(class = "card-hint", "navy = estimate · grey = known alive"))),
          uiOutput("detectHead"),
          spin(plotlyOutput("detectPlot", height = "400px")),
          uiOutput("detectNote")),
        layout_columns(col_widths = c(7, 5),
          card(full_screen = TRUE,
            card_head("people-fill", "MNKA & catch-per-effort, by plot",
              info_pop("MNKA & CPUE",
                p(tags$b("MNKA"), " (Minimum Number Known Alive) counts how many individuals were ", tags$em("known"), " to be alive each month: caught that month, or before ", tags$em("and"), " after. A transparent abundance index (Krebs 1966)."),
                p("The dotted grey line (", tags$b("right axis"), ") is ", tags$b("catch per effort"), ": captures per 100 ", tags$b("trap-nights"), ". A trap-night is one trap set out for one night, computed from ", tags$em("this site's own data"), " (the actual traps set × the nights they ran), so a smaller grid or fewer nights doesn't skew it, and it does ", tags$em("not"), " assume a fixed 100-trap grid. Sprung or disturbed traps count as half a trap-night (Nelson & Clark 1973)."),
                p("It's a relative-abundance ", tags$b("index"), " for comparing sites and trends, not a population estimate (it doesn't correct for detectability; that's what the detection-corrected abundance tab is for)."))),
            uiOutput("mnkaInsight"),
            spin(plotlyOutput("mnkaPlot", height = "440px"))),
          card(full_screen = TRUE,
            card_head("graph-up", "Species accumulation",
              info_pop("Species accumulation",
                p("As more trapping bouts accumulate, how many ", tags$b("species"), " have been found? When the curve flattens, you've probably found them all. Genus-only IDs (\"X sp.\") are excluded so an unidentified catch isn't counted as its own species."),
                p("The dashed ", tags$b("Chao1"), " line is a ", tags$b("bias-corrected minimum estimate"), " of true richness, a floor that includes species not yet caught, shown with a 95% interval. When ", tags$em("doubletons"), " (species caught as exactly 2 individuals) are scarce it's unstable and flagged as a lower bound (Chao 1987; Gotelli & Colwell 2001)."))),
            uiOutput("accumInsight"),
            spin(plotlyOutput("accumPlot", height = "440px")))
        ),
        conditionalPanel("input.envLayer && input.envLayer != 'none'",
          card(full_screen = TRUE,
            card_head("bullseye", "Environmental response · catch-per-effort vs the driver",
              info_pop("Response scatter",
                p("Each point is one month: its ", tags$b("catch per 100 trap-nights"),
                  " against the value of the selected environmental driver (with your ",
                  tags$b("lag"), " applied)."),
                p("A rising cloud means more animals when the driver is high; the dashed line is an OLS fit. This is the same signal as the correlation banner above, shown as a shape so you can spot thresholds or saturation."))),
            spin(plotlyOutput("envScatter", height = "420px"))))
      ),

      nav_panel(
        title = tagList(bs_icon("bar-chart-line-fill"), " Community Pulse"),
        value = "community",
        div(class = "tab-intro", bs_icon("info-circle"),
            "The whole community for this site & window, not one animal. Hover any chart for details."),
        card(card_head("gender-ambiguous", "Who's out there · sex & age structure",
               info_pop("Sex & age", p("The ", tags$b("sex"), " and ", tags$b("life-stage"), " breakdown of every handled animal. The number in the middle is the total handled."))),
             layout_columns(col_widths = c(6, 6),
               spin(plotlyOutput("sexDonut", height = "300px"), img = "rat1.gif"),
               spin(plotlyOutput("ageDonut", height = "300px"), img = "rat1.gif"))),
        card(full_screen = TRUE,
          card_head("diagram-3-fill", "Diversity profile · effective number of species",
            info_pop("Hill numbers",
              p("Three views of diversity, all in the same intuitive unit, an ", tags$b("effective number of species"), ", indexed by ", tags$em("q"), ", how much rare species count:"),
              tags$ul(
                tags$li(tags$b("q = 0"), " · plain ", tags$b("species richness"), " (every species counts equally)."),
                tags$li(tags$b("q = 1"), " · ", tags$b("exp(Shannon)"), ": the effective number of ", tags$em("common"), " species."),
                tags$li(tags$b("q = 2"), " · ", tags$b("inverse Simpson"), ": the effective number of ", tags$em("dominant"), " species.")),
              p("They always shrink (q0 ≥ q1 ≥ q2). When q1 sits close to q0 the community is ", tags$b("even"), "; when it drops far below, a few species ", tags$b("dominate"), "."),
              p(tags$em("Abundance = distinct individuals per species, so a much-recaptured animal isn't counted twice. Hill 1973; Jost 2006; Chao et al. 2014.")))),
          uiOutput("hillInsight"),
          layout_columns(col_widths = c(7, 5),
            spin(plotlyOutput("hillPlot", height = "260px")),
            uiOutput("hillNote"))),
        card(full_screen = TRUE,
          card_head("activity", "Captures over time · by species",
            info_pop("Captures over time",
              p("Captures by species per month, summed across ", tags$b("all plots"), " (the site total) by default. Use the ", tags$b("plot picker"), " to drill into a single plot."),
              p("Values are ", tags$b("raw captures"), " (not effort-adjusted). Only species with a handful of captures are drawn, to keep the legend readable."))),
          div(style = "max-width: 280px; margin: 0 2px 8px;",
            selectInput("plotTrendPlot", tagList(bs_icon("pin-map"), " Show plot"),
                        choices = c("All plots (site total)" = "all"), width = "100%")),
          spin(plotlyOutput("plotTrend", height = "440px"))),
        card(full_screen = TRUE,
          card_head("rulers", "Body-size profile · weight distribution by species",
            info_pop("Body-size profile",
              p("A ", tags$b("violin"), " for each species shows the spread of ", tags$b("adult"), " body weights, one mean weight per individual (so a much-recaptured animal isn't counted many times), wide where many animals fall, with a line at the mean. Species with at least 8 weighed adults are shown."),
              p("Species are ordered lightest → heaviest (log scale, since a pocket mouse and a woodrat differ ~30×)."),
              p("If you've opened an individual's dossier, a ", tags$span(style="color:#c9a300;font-weight:700", "gold diamond"), " marks where it sits in its species."))),
          spin(plotlyOutput("sizeViolin", height = "420px"))),
        card(full_screen = TRUE,
          card_head("rulers", "Body measurements by species (adults)",
            info_pop("Measurements by species",
              p("The field measurements NEON records, summarised per species: the ", tags$b("median"), " with the ", tags$b("5th–95th-percentile range"), " (a typical-adult envelope) and the number of records (", tags$b("n"), ")."),
              p("A small ", tags$span(class = "mz-flag", "⚠"), " marks a measure that contains an ", tags$b("extreme value"), " beyond that range, likely a data-entry error (e.g. a 357 g kangaroo rat). The value is ", tags$b("kept in the data"), " for your QC but is excluded from the median and the range; hover the ⚠ for the raw min–max."),
              p(tags$b("Adults only"), ": juveniles and subadults are much lighter and shorter, so pooling growth stages would inflate the range and pull the median; restricting to staged adults gives an honest \"typical adult size.\""),
              p(tags$b("Weight"), " and ", tags$b("hindfoot length"), " are taken at nearly every handling; ", tags$b("tail"), " and ", tags$b("ear"), " are measured far less often, so those cells are blank (", tags$b("—"), ") for species with fewer than 3 records."),
              p(tags$em("Median + percentile range, not a \"mode\": for a continuous measurement the median and spread are the honest summary. A species' mean weight and the \"heaviest species\" call shown elsewhere need ≥8 staged adults, or they're withheld as too noisy. Counts confirmed species-level IDs only.")))),
          spin(DT::DTOutput("speciesMeas"))),
        card(
          card_head("hourglass-split", "How long do they live? · longest confirmed alive",
            info_pop("Longest confirmed alive",
              p("The ", tags$b("longest we confirmed an individual alive"), ": its age at last capture (a conservative age at first capture, plus how long we tracked it). Shown per species where at least ", tags$b("5"), " individuals were each caught ", tags$b("3+ times"), "."),
              p("It's a ", tags$b("floor, not a lifespan"), ": animals still alive or that left the grid aren't counted (one not seen again is ", tags$em("presumed gone: death or dispersal"), ", never assumed dead), and the record only spans the years sampled here. NEON keeps a tag on an animal for life and doesn't recycle tag numbers, so a multi-year career is a ", tags$b("real recapture span"), ", not a tag mix-up; true lifespan is simply longer than what we caught."),
              p("Because it's the ", tags$b("single longest-tracked individual"), ", species we trap more often reach higher floors just from having more chances, so read each as its own floor, ", tags$b("not a ranking"), " of which species lives longest."),
              p("A model-based estimate from apparent survival (the gold-standard mark-recapture approach) is deliberately ", tags$b("not shown"), ": it needs an offline-validated capture-session model and would over-claim on this data."),
              p(tags$em("Captive maxima (where shown) are from the AnAge database and run several times the wild value, a sanity ceiling, not an expectation."))) ),
          uiOutput("lifespanBanner")),
        card(full_screen = TRUE,
          card_head("calendar-heart", "Breeding phenology · when the population reproduces",
            info_pop("Breeding phenology",
              p("By calendar month, the share of adults that are ", tags$b("reproductively active"), ":"),
              p(tags$span(style="color:#2f7fb5", "● breeding males"), " (scrotal) and ",
                tags$span(style="color:#c2255c", "● reproductive females"), " (pregnant or lactating)."),
              p("Peaks reveal the breeding season; the dip shows the off-season."))),
          uiOutput("phenoInsight"),
          spin(plotlyOutput("phenoPlot", height = "320px")))
      ),

      nav_panel(
        title = tagList(bs_icon("rulers"), " Size Lab"),
        value = "sizelab",
        div(class = "tab-head",
          div(class = "tab-head-text",
            h4("Body-size lab · every individual on one map",
               info_pop("Body-size lab",
                 p("Every individual NEON tagged here is a ", tags$b("dot"), ", placed by its ",
                   tags$b("hind-foot length"), " (x) and ", tags$b("weight"), " (y) and coloured by ",
                   tags$b("species"), ", like a height-vs-weight recruiting board, but for rodents."),
                 p(tags$b("Tap a dot"), " to pin its card; drag the card around, resize it from the corner, and ",
                   tags$b("download the chart with the cards still on it"), ". Tap ", tags$b("“Open QC history card”"),
                   " on a card for that animal's every-capture measurements + automatic data-quality flags."),
                 p(class = "caveat", bs_icon("exclamation-triangle"),
                   " This is a ", tags$b("morphometric / QC map, not a body-condition index"),
                   ": in these desert rodents hind-foot length barely predicts mass (r ≈ 0.15), so a dot far from its species' cloud flags an unusual or mis-typed record, not “fitness.”"))),
            p("Each dot is one animal, by hind-foot length × weight, coloured by species. Tap a dot to pin a card; open its QC history card for every capture's measurements and automatic flags, then download any of it.")),
          div(class = "sizelab-controls",
            selectInput("scatterSpecies", tagList(bs_icon("funnel"), " Species"),
                        choices = c("All species" = "all"), selected = "all", width = "230px"),
            selectInput("scatterPlot", tagList(bs_icon("grid-3x3"), " Plot"),
                        choices = c("All plots" = "all"), selected = "all", width = "150px"),
            div(class = "sizelab-adults",
              checkboxInput("scatterAdults", "Adults only", value = FALSE)))
        ),
        card(full_screen = TRUE,
          card_head("bullseye", "Hind-foot length × weight, by species",
            info_pop("Reading this map",
              p("Pick ", tags$b("one species"), " from the filter to add its ",
                tags$b("size–mass fit line"), " (drawn ", tags$em("only"), " where length really predicts mass: n ≥ 15, |r| ≥ 0.3) and ",
                tags$b("median crosshairs"), " that split it into body-shape quadrants."),
              p("The ", tags$span(style = "color:#c9a300;font-weight:700", "gold diamond"),
                " marks the individual you're currently tracking."))),
          div(class = "sizelab-toolbar",
            tags$button(class = "smt-snap-btn", type = "button", onclick = "smtSaveScatter()",
                        bsicons::bs_icon("camera-fill"), " Download (with pinned cards)"),
            tags$button(class = "smt-clear-btn", type = "button", onclick = "smtClearPins()",
                        bsicons::bs_icon("eraser-fill"), " Clear pins"),
            tags$span(class = "sizelab-hint", bs_icon("hand-index-thumb"),
                      " interactive · tap a dot to pin its card")),
          div(class = "smt-pinnable",
            spin(plotlyOutput("bodyScatter", height = "540px"), img = "rat1.gif"))),
        uiOutput("qcHistoryCard")
      ),

      nav_panel(
        title = tagList(bs_icon("fire"), " Home Range"),
        value = "homerange",
        div(class = "tab-head",
          div(class = "tab-head-text",
            h4("Trap-grid home range",
               info_pop("Trap-grid home range",
                 p("NEON small-mammal plots use a grid of ", tags$b("up to 10×10"), " traps spaced 10 m apart (columns A–J, rows 1–10)."),
                 p(tags$em("Not every plot is the full 10×10:"), " some sites or bouts deploy fewer traps. For example, a reduced grid is used where capture rates are very high. Empty cells here can mean “no trap set” as well as “not caught.”"),
                 p("The ", tags$b("heatmap"), " colors each cell by how many times this animal was caught there; white dots mark the actual capture cells and the gold ✕ is its centre of activity."),
                 p("The ", tags$b("replay"), " animates its captures in order: press ▶ to watch it move."),
                 p(tags$b("Hotspot blur"), " smooths the grid to show its core area."))),
            p("Where on the plot's trap grid this animal kept turning up. Hit play to replay its captures over time.")),
          div(class = "hr-controls",
            div(class = "hr-indiv",
              tags$label(class = "hr-indiv-lab", `for` = "indivHR",
                         tagList(bs_icon("search"), " Tracking")),
              selectizeInput("indivHR", label = NULL, choices = NULL, width = "260px",
                             options = list(placeholder = "Pick an individual…"))),
            div(class = "hr-toggles",
              checkboxInput("blurMode", "Hotspot blur", value = FALSE)))
        ),
        layout_columns(col_widths = c(6, 6),
          card(full_screen = TRUE, card_head("grid-3x3-gap-fill", "Capture heatmap"),
               spin(plotlyOutput("trapHeat", height = "460px"), img = "rat1.gif")),
          card(full_screen = TRUE, card_head("play-circle", "Capture replay"),
               spin(plotlyOutput("trapReplay", height = "460px"), img = "rat1.gif"))
        )
      ),

      nav_panel(
        title = tagList(bs_icon("map-fill"), " Plot map"),
        value = "map",
        div(class = "tab-head",
          div(class = "tab-head-text",
            h4("Species diversity across the site",
               info_pop("Site map",
                 p("Each circle is a NEON trapping ", tags$b("plot"), ", placed at its real coordinates."),
                 p("Circle ", tags$b("size"), " = total captures there; ", tags$b("color"), " = species (same colors as the charts)."),
                 p("When an individual is selected, its plots get a pulsing ", tags$span(style="color:#ffd24a", "gold ring"), "."),
                 p(tags$b("Recapture movement"), " (toggle, top-right): curved ", tags$span(style="color:#15b8a6", "teal arcs"), " link grids where the same tagged animals were recaptured: thicker = more individuals made that move. It's mark-recapture (\"here, then there\"), ", tags$b("not"), " a tracked route; a selected individual's own moves draw in gold."),
                 p("Hover a circle for its species + counts; switch basemap top-right."))),
            p("Each marker is a plot, sized by captures and colored by species. The selected individual's plots glow gold.")),
          div(class = "map-controls",
            selectInput("view", "Basemap", width = "160px",
                        choices = c("Satellite" = "Esri.WorldImagery",
                                    "Terrain" = "Esri.WorldTopoMap",
                                    "Light" = "CartoDB.Positron",
                                    "Dark" = "CartoDB.DarkMatter")),
            sliderInput("rad_size", "Marker scale", min = .3, max = 2.5, value = 1, step = .1, width = "150px"),
            div(class = "map-flow-toggle",
              checkboxInput("showFlow", tagList(bs_icon("share-fill"), " Recapture movement"), value = FALSE)),
            actionButton("reloadMapBtn", tagList(bs_icon("arrow-clockwise"), " Redraw"),
                         class = "btn-outline-dark btn-sm"))
        ),
        uiOutput("mapInsight"),
        spin(leafletOutput("map", height = "620px"), img = "rat1.gif")
      ),

      nav_panel(
        title = tagList(bs_icon("trophy-fill"), " Hall of Fame"),
        value = "fame",
        div(class = "tab-head",
          div(class = "tab-head-text",
            h4("Capture leaderboard",
               info_pop("How the leaderboard works",
                 p("Every animal NEON caught is ranked by how often it turned up in traps."),
                 p(tags$b("Click any row"), " to open that individual's dossier."),
                 p("Switch ", tags$b("category"), " to re-rank by weight, career length, roaming, or weight-for-its-species (chonk)."),
                 tags$hr(),
                 p(tags$b("Rarity tiers"), " come from total captures:"),
                 lapply(rarity_key_items, function(it)
                   div(class = "pkey", tier_badge(it[1]), tags$span(it[2]))))),
            p("Every individual ranked. Pick a category, then click a row to open its dossier.")),
          div(class = "leader-cats",
            radioButtons("leaderCat", NULL, inline = TRUE,
              choiceNames = list(
                HTML("&#127942; Most caught"), HTML("&#127947; Heaviest"),
                HTML("&#9201; Longest career"), HTML("&#128506; Biggest roamer"),
                HTML("&#129482; Chonkiest")),
              choiceValues = c("captures", "weight", "career", "roam", "chonk"),
              selected = "captures"))
        ),
        # always-visible rarity key so "Epic vs Rare" is never a mystery
        div(class = "rarity-legend",
          tags$span(class = "rl-label", "Rarity"),
          tier_badge("Legendary"), tier_badge("Epic"), tier_badge("Rare"),
          tier_badge("Uncommon"), tier_badge("Common"),
          tags$span(class = "rl-sep", "·"),
          tags$span(class = "rl-label", style = "letter-spacing:0;text-transform:none;",
                    "by total captures (15+ → Legendary)")),
        uiOutput("leaderBanner"),
        uiOutput("famePodium"),
        div(class = "tab-intro", bs_icon("hand-index-thumb"),
            " Tap any row (or a podium card) to open that animal's dossier: measurements, home range, capture history, and a shareable card."),
        spin(DT::DTOutput("leaderboard"))
      ),

      nav_panel(
        title = tagList(bs_icon("search"), " Search"),
        value = "search",
        div(class = "tab-head",
          div(class = "tab-head-text",
            h4("Search the network",
               info_pop("Search the network",
                 p("Search every NEON small-mammal site at once, straight from the bundled data so it answers instantly."),
                 p(tags$b("Find a species"), " lists every site where that animal was trapped, with its within-site index and the years it was caught there."),
                 p(tags$b("Find sites"), " filters sites by how widespread a species is, or by how many species each site has recorded."),
                 p("The number shown is ", tags$b("MNKA"), " (minimum number known alive): a within-site index of how many of that species were known to be alive at one time. It is not a population count, and it is not a fair ranking between sites with different trapping effort."))),
            p("Look across all NEON sites at once. Find where a species turns up, or which sites are the most species-rich. Pick a result to load that site.")),
          div(class = "search-modes",
            radioButtons("searchMode", NULL, inline = TRUE,
              choiceNames = list(
                HTML("&#128269; Find a species"),
                HTML("&#128205; Find sites")),
              choiceValues = c("taxon", "threshold"),
              selected = "taxon"))
        ),

        # (a) FIND A SPECIES -------------------------------------------------
        conditionalPanel("input.searchMode == 'taxon'",
          card(
            card_head("search", "Find a species across the network"),
            div(class = "search-controls",
              selectizeInput("searchTaxon", "Species",
                choices = NULL, width = "100%",
                options = list(placeholder = "Start typing a species name…",
                               maxOptions = 2000))),
            uiOutput("searchTaxonCaption"),
            spin(div(style = "width:100%", DT::DTOutput("searchTaxonTbl")))
          )
        ),

        # (b) FIND SITES (threshold query) -----------------------------------
        conditionalPanel("input.searchMode == 'threshold'",
          card(
            card_head("funnel", "Find sites by a threshold"),
            div(class = "search-controls",
              radioButtons("threshKind", "Query", inline = TRUE,
                choiceNames = list("Species recorded at more than N sites",
                                   "Sites with more than X species"),
                choiceValues = c("widespread", "richness"),
                selected = "widespread"),
              conditionalPanel("input.threshKind == 'widespread'",
                sliderInput("threshN", "Minimum number of sites (N)",
                            min = 1, max = 30, value = 5, step = 1, width = "320px")),
              conditionalPanel("input.threshKind == 'richness'",
                sliderInput("threshX", "Minimum species per site (X)",
                            min = 1, max = 40, value = 10, step = 1, width = "320px"))),
            uiOutput("searchThreshCaption"),
            spin(div(style = "width:100%", DT::DTOutput("searchThreshTbl")))
          )
        )
      ),

      nav_panel(
        title = tagList(bs_icon("person-vcard"), " Dossier"),
        value = "dossier",
        uiOutput("dossierHero"),
        uiOutput("tradingCardWrap"),
        layout_columns(col_widths = c(7, 5),
          card(full_screen = TRUE,
            card_head("graph-up", "Measurements through time",
              info_pop("Measurements through time",
                p("Each capture's ", tags$b("weight"), " (navy) and ", tags$b("hind-foot length"), " (cardinal) plotted over time."),
                p("The shaded band is the ", tags$b("middle 50% of adult weights"), " for this species, so you can see whether the animal runs heavy or light against grown adults (juveniles would make the band misleadingly low). The ♦ marks its heaviest capture."))),
            spin(plotlyOutput("measPlot", height = "360px"))),
          card(full_screen = TRUE,
            card_head("speedometer2", "Chonk Index · weight rank",
              info_pop("The Chonk Index",
                p("An honest ", tags$b("adult weight percentile within species"), ": i.e. \"how heavy is this animal for its kind?\""),
                p("50 = a perfectly typical adult; the delta shows how far above/below typical it sits."),
                p(tags$em("Why not a body-condition index? In these desert rodents foot length barely predicts mass, so a fancier index would just rank noise. The body-size map below shows the real relationship.")))),
            spin(plotlyOutput("chonkGauge", height = "360px")))
        ),
        card(full_screen = TRUE,
             card_head("bullseye", "Body-size map · where it sits among its species",
               info_pop("Body-size map",
                 p("Every measured animal plotted by ", tags$b("weight × hind-foot length"), ". The faint grey dots are all other species; the colored cloud is ", tags$b("this animal's species"), " (by life stage)."),
                 p("The ", tags$b("gold diamonds"), " are this individual's captures: high in the cloud = a big one."),
                 p("A dashed ", tags$b("size–mass fit line"), " is drawn ", tags$em("only"), " for species where length actually predicts mass (so you're never shown a fake trend.)")),
               tags$span(class = "card-hint", style = "margin-left:auto", "this animal in gold")),
             spin(plotlyOutput("morphoPlot", height = "420px"))),
        card(card_head("clock-history", "Capture history",
               info_pop("Capture history", p("Every individual capture event for this animal: date, plot, trap cell, measurements, and field notes. Use the search box to filter."))),
             spin(DT::DTOutput("capHistory")))
      ),

      nav_panel(
        title = tagList(bs_icon("info-circle"), " About"),
        value = "about",
        uiOutput("aboutPanel")
      )
    )
  ),

  # (The site report card is now a server-side PDF streamed by downloadHandler
  #  output$reportPdf — see the hero downloadLink — so there's no hidden HTML to
  #  print here anymore.)

  # ---- Desert Data Labs business footer ----------------------------------
  div(class = "ddl-footer",
    div(tags$a(class = "custom-cta",
      href = "mailto:desertdatalabs@gmail.com?subject=NEON%20Small%20Mammal%20Tracker",
      span(class = "hand", "\U0001F44B"), "Questions or feedback? Email Desert Data Labs.")),
    p(style = "margin-top:12px",
      HTML("Built by <strong>Desert Data Labs</strong> · Tucson, AZ · feedback or bug reports → "),
      tags$a(href = "mailto:desertdatalabs@gmail.com?subject=NEON%20Small%20Mammal%20Tracker", "desertdatalabs@gmail.com")),
    p(style = "font-size:12px;opacity:.85",
      "Data: NEON Small Mammal Box Trapping (DP1.10072.001). Not affiliated with NEON, Battelle, or the NSF. An educational data-exploration tool.")
  )
)
