# ===========================================================================
# NEON Ground Beetle Tracker — ui.R
# A clean, card-based dashboard for exploring carabid biodiversity across the
# National Ecological Observatory Network.
# ===========================================================================

card_head <- function(icon, title, ...)
  bslib::card_header(class = "with-info", bsicons::bs_icon(icon),
                     tags$span(class = "ch-title", " ", title), ...)

ui <- bslib::page_sidebar(
  theme = app_theme,
  window_title = "NEON Ground Beetle Tracker",
  fillable = FALSE,

  tags$head(
    tags$link(rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Rubik:wght@400;500;600;700;800&display=swap"),
    tags$link(rel = "stylesheet", href = "styles.css")
  ),
  useShinyjs(),

  # ---- sidebar -----------------------------------------------------------
  sidebar = sidebar(
    width = 320, class = "control-deck",
    div(class = "brand",
      div(class = "brand-mark", "\U0001FAB2"),  # beetle
      div(
        div(class = "brand-title", "Ground Beetle Tracker"),
        div(class = "brand-sub", "NEON carabid biodiversity")
      )
    ),

    selectInput("stateSel", label = tagList(bs_icon("geo-alt-fill"), " 1 · Pick a state"),
                choices = NULL, width = "100%"),
    selectInput("site", label = tagList(bs_icon("pin-map-fill"), " 2 · Pick a site"),
                choices = NULL, width = "100%"),
    uiOutput("siteBio"),

    dateRangeInput("dateRange", label = tagList(bs_icon("calendar3"), " 3 · Date window"),
                   format = "yyyy-mm", startview = "year",
                   start = "2016-01-01", end = "2023-12-31"),

    actionButton("loadBtn", tagList(bs_icon("bug-fill"), " Load this site"),
                 class = "btn-primary btn-lg w-100"),
    div(class = "demo-hint", bs_icon("info-circle"),
        " Bundled sites load instantly. Live NEON pulls (where enabled) take a moment."),

    uiOutput("srcNote"),

    hr(class = "deck-hr"),
    div(class = "deck-foot",
      bs_icon("database"), " NEON ", tags$code("DP1.10022.001"),
      br(), tags$a(href = "https://www.neonscience.org/data-collection/ground-beetles",
                   target = "_blank", bs_icon("box-arrow-up-right"), " about the data"),
      br(), tags$a(href = "https://desertdatalabs.com", target = "_blank",
                   bs_icon("box-arrow-up-right"), " Desert Data Labs")
    )
  ),

  # ---- hero + stats ------------------------------------------------------
  div(class = "app-hero",
    h1(class = "app-title", "NEON Ground Beetle Tracker",
       span(class = "title-tag", "unofficial")),
    p(class = "app-subtitle",
      "Ground beetles (Carabidae) are a textbook bioindicator. Explore who lives where, how diverse each site is, and when beetles are active — across the National Ecological Observatory Network.")
  ),
  uiOutput("heroStats"),
  uiOutput("splash"),

  # ---- tabs --------------------------------------------------------------
  div(id = "mainTabsWrap",
    navset_card_tab(id = "tabs",

      nav_panel(
        title = tagList(bs_icon("collection"), " Overview"), value = "overview",
        card(full_screen = TRUE,
          card_head("bar-chart-line-fill", "Carabid community — most abundant first",
            info_pop("Community composition",
              p("Each bar is a species; length is total ", tags$b("individuals"),
                " caught, and the label shows ", tags$b("catch per 100 trap-nights"),
                " so effort is accounted for."))),
          spin(plotlyOutput("commBar", height = "460px"))),
        h4(class = "section-title", bs_icon("binoculars"), " Meet the beetles"),
        uiOutput("meetBeetles")
      ),

      nav_panel(
        title = tagList(bs_icon("diagram-3-fill"), " Diversity"), value = "diversity",
        layout_columns(col_widths = c(5, 7),
          card(full_screen = TRUE,
            card_head("diagram-3-fill", "Diversity profile — effective species",
              info_pop("Hill numbers",
                p("Diversity in one unit — an ", tags$b("effective number of species"), ":"),
                tags$ul(
                  tags$li(tags$b("q0"), " richness (all species count equally)"),
                  tags$li(tags$b("q1"), " exp(Shannon) — common species"),
                  tags$li(tags$b("q2"), " inverse Simpson — dominant species")),
                p("When q1 sits near q0 the community is even; far below = a few species dominate."))),
            spin(plotlyOutput("hillPlot", height = "300px")),
            uiOutput("hillNote")),
          card(full_screen = TRUE,
            card_head("graph-up", "Rarefaction — richness at equal sample size",
              info_pop("Rarefaction",
                p("Expected species in a random subsample of ", tags$b("n individuals"),
                  " (Hurlbert 1971), with a ±1 SD band. Comparing at equal n stops a site that simply caught more beetles from looking falsely richer."))),
            spin(plotlyOutput("rarePlot", height = "300px")))
        ),
        card(full_screen = TRUE,
          card_head("graph-up-arrow", "Species accumulation across bouts",
            info_pop("Accumulation",
              p("As pitfall ", tags$b("bouts"), " accumulate, how many species have appeared? A flattening curve means the site's fauna is well sampled (averaged over random bout orders; Gotelli & Colwell 2001)."))),
          spin(plotlyOutput("accumPlot", height = "360px")))
      ),

      nav_panel(
        title = tagList(bs_icon("calendar-heart"), " Seasonality"), value = "seasonality",
        div(class = "tab-intro", bs_icon("info-circle"),
            " Beetle activity isn't constant — pitfall catch tracks the warm season and each species' own peak."),
        card(full_screen = TRUE,
          card_head("activity", "Activity-density by month (catch per 100 trap-nights)",
            info_pop("Seasonality",
              p("Mean ", tags$b("catch per 100 trap-nights"), " by calendar month — the community activity curve. Toggle to split it by the top species."))),
          div(class = "season-toggle",
            checkboxInput("seasonBySpecies", "Split by species", value = FALSE)),
          spin(plotlyOutput("seasonPlot", height = "440px")))
      ),

      nav_panel(
        title = tagList(bs_icon("graph-up-arrow"), " Trends"), value = "trends",
        div(class = "tab-intro", bs_icon("info-circle"),
            " Are the beetles holding steady, booming, or declining? NEON's standardized effort makes year-to-year catch comparable — the backbone of insect-decline science."),
        uiOutput("trendVerdict"),
        card(full_screen = TRUE,
          card_head("graph-up-arrow", "Catch-per-effort by year, with fitted trend",
            info_pop("Inter-annual trend",
              p("Annual ", tags$b("catch per 100 trap-nights"), " (or raw counts when a bundle lacks effort), with an ordinary-least-squares trend line."),
              p("The slope and its p-value drive the verdict above. Short series (a few years) are noisy — read the direction, not the decimal."))),
          spin(plotlyOutput("trendPlot", height = "440px")))
      ),

      nav_panel(
        title = tagList(bs_icon("map-fill"), " Biogeography"), value = "biogeo",
        div(class = "tab-head",
          div(class = "tab-head-text",
            h4("Carabid richness across NEON"),
            p("Each marker is a NEON site with beetle data. With no species picked, markers size by total richness; choose a species to map its range, sized by local abundance. Tap a site to load it.")),
          div(class = "map-controls",
            selectInput("rangeSpecies", tagList(bs_icon("search"), " Map a species' range"),
                        choices = NULL, width = "280px"))),
        spin(leafletOutput("map", height = "540px")),
        card(full_screen = TRUE,
          card_head("diagram-2", "Community ordination — who resembles whom",
            info_pop("PCoA ordination",
              p("Every point is a ", tags$b("site × plot × year"), " beetle community, placed by ",
                tags$b("Bray–Curtis"), " dissimilarity (PCoA) so similar communities sit close together."),
              p("Points from the same site/biome cluster — the carabid biogeography signal. Computed across all bundled sites."))),
          spin(plotlyOutput("ordPlot", height = "440px"))),
        card(full_screen = TRUE,
          card_head("award", "Indicator species — each site's signature beetles",
            info_pop("Indicator value (IndVal)",
              p("The ", tags$b("Dufrêne–Legendre IndVal"), " (0–100) flags species that are both ",
                tags$b("concentrated in"), " and ", tags$b("reliably found at"), " one site."),
              tags$ul(
                tags$li(tags$b("Specificity"), " — share of the species' abundance that's at that site"),
                tags$li(tags$b("Fidelity"), " — share of that site's samples where it turns up")),
              p("High on both = a genuine signature of that place. Computed across all bundled sites; samples are plot × year."))),
          spin(DT::DTOutput("indicatorTable"))),
        card(card_head("table", "Site comparison"),
             spin(DT::DTOutput("siteTable")))
      ),

      nav_panel(
        title = tagList(bs_icon("info-circle"), " About"), value = "about",
        uiOutput("aboutPanel")
      )
    )
  ),

  # ---- footer ------------------------------------------------------------
  div(class = "ddl-footer",
    p(HTML("Built by <strong>Desert Data Labs</strong> · Tucson, AZ · custom data apps & analytics → "),
      tags$a(href = "mailto:desertdatalabs@gmail.com?subject=NEON%20Ground%20Beetle%20Tracker",
             "desertdatalabs@gmail.com")),
    p(style = "font-size:12px;opacity:.85",
      "Data: NEON Ground Beetles sampled from pitfall traps (DP1.10022.001). Not affiliated with NEON, Battelle, or the NSF. An educational data-exploration tool.")
  )
)
