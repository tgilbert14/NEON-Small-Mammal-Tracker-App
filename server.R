# ===========================================================================
# NEON Small Mammal Tracker — server.R
# ===========================================================================

server <- function(input, output, session) {

  # ---- shared plotly styling (light theme, Rubik, dark ink text) ---------
  plotly_theme <- function(p, legend = TRUE) {
    p %>% plotly::layout(
      paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
      font = list(color = "#1f2a30", family = "Rubik"),
      xaxis = list(gridcolor = "rgba(31,42,48,0.08)", zerolinecolor = "rgba(31,42,48,0.15)", linecolor = "#d6ddd4"),
      yaxis = list(gridcolor = "rgba(31,42,48,0.08)", zerolinecolor = "rgba(31,42,48,0.15)", linecolor = "#d6ddd4"),
      legend = list(bgcolor = "rgba(0,0,0,0)", orientation = "h", y = -0.2, font = list(color = "#344049")),
      margin = list(l = 50, r = 30, t = 30, b = 40)
    ) %>%
      plotly::config(displayModeBar = FALSE, responsive = TRUE)
  }

  # shown on individual-only views when nobody is selected yet
  PICK_MSG <- "Pick an individual first.<br>Open the <b>Hall of Fame</b> and tap a row,<br>or hit \U201CSurprise me\U201D in the sidebar."

  # Append the current site + year-range as a small top-right caption on a plot
  # (uses add_annotations so it never clobbers a plot's own annotations).
  ctx_anno <- function(p) {
    if (is.null(rv$ctx)) return(p)
    plotly::add_annotations(p, text = rv$ctx, x = 1, y = 1.07, xref = "paper", yref = "paper",
      xanchor = "right", yanchor = "bottom", showarrow = FALSE,
      font = list(color = "#6b7a89", size = 11, family = "Rubik"))
  }

  # A centered-message placeholder for plots that have nothing to show.
  note_plot <- function(msg, icon = "\U0001F50D") {
    plotly::plot_ly(type = "scatter", mode = "markers") %>%
      plotly::layout(
        paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
        xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
        annotations = list(list(text = paste0(icon, "<br>", msg), showarrow = FALSE,
          font = list(color = "#6b7a85", size = 15), align = "center"))) %>%
      plotly::config(displayModeBar = FALSE)
  }

  # ---- core reactive state ------------------------------------------------
  rv <- reactiveValues(
    data = NULL,       # cleaned mam table
    lb = NULL,         # leaderboard tibble
    lb_view = NULL,    # currently-displayed leaderboard slice (row order = DT rows)
    pal = NULL,        # stable species -> color map (shared across all views)
    label = NULL,      # human label for the active site/window
    tag = NULL         # selected full tagID
  )

  # state -> site cascading picker (Arizona default so the demo lines up)
  updateSelectInput(session, "stateSel", choices = state_choices(), selected = "NM")
  observeEvent(input$stateSel, {
    sites <- sites_in_state(input$stateSel)
    # if a picker-map click is mid-flight, keep its site selected through the
    # cascade instead of snapping to the first site in the state
    sel <- if (!is.null(rv$pendingSite) && rv$pendingSite %in% sites) rv$pendingSite
           else if (length(sites)) sites[[1]] else NULL
    rv$pendingSite <- NULL
    updateSelectInput(session, "site", choices = sites, selected = sel)
  }, ignoreNULL = TRUE)

  output$siteBio <- renderUI({
    req(input$site)
    b <- site_bio(input$site)
    if (is.null(b)) return(NULL)
    div(class = "site-bio", bs_icon("info-circle-fill"), span(b))
  })

  shinyjs::hide("mainTabsWrap")
  # provisional/live data is meaningless in a bundle-only build
  if (!LIVE_FETCH) shinyjs::hide(selector = ".prov-toggle")

  # ---- data ingestion -----------------------------------------------------
  ingest <- function(data.raw, label, is_demo = FALSE) {
    rv$is_demo <- is_demo
    d <- clean_mam(data.raw)
    if (is.null(d) || sum(d$is_capture) == 0) {
      session$sendCustomMessage("loadDone", list())   # hide the loading overlay
      showNotification("No small-mammal captures found for that site & window.",
                       type = "warning", duration = 6)
      return(invisible(NULL))
    }
    rv$data  <- d
    rv$lb    <- build_leaderboard(d)
    rv$pal   <- make_species_pal(d)
    rv$label <- label
    rv$tag   <- NULL
    # compact context shown on each plot, e.g. "JORN · 2022–2024"
    y1 <- format(safe_date_min(d$date), "%Y"); y2 <- format(safe_date_max(d$date), "%Y")
    rv$ctx <- paste0(mode_chr(d$siteID), " · ", if (is.na(y1)) "" else if (y1 == y2) y1 else paste0(y1, "–", y2))

    # reveal UI
    shinyjs::show("mainTabsWrap")
    shinyjs::show("indivPickerWrap")
    shinyjs::hide("splash")

    # individual picker choices
    lb <- rv$lb
    ch <- setNames(lb$tagID, sprintf("%s  %s · %s · %d caps",
                                     lb$emoji, lb$short, lb$scientificName, lb$captures))
    updateSelectizeInput(session, "indiv", choices = c("Pick a tagID…" = "", ch), server = TRUE)
    nav_select("tabs", "overview")
    session$sendCustomMessage("countUp", list())
    session$sendCustomMessage("loadDone", list())   # hide the loading overlay
    invisible(TRUE)
  }

  # session cache: re-loading a site + window you already fetched is instant
  fetch_cache <- new.env(parent = emptyenv())

  # THE single load path — used by the Load button AND the national site-picker map.
  # Takes the site explicitly (don't re-read input$site) so a map click can't race the
  # state->site cascade. ingest() dismisses the loading overlay; we dismiss it on every
  # early-return path too.
  load_site <- function(site, s0, e0, prov = FALSE) {
    if (is.null(site) || site == "") { session$sendCustomMessage("loadDone", list()); return(invisible()) }

    # 1) bundled site? read from disk instantly and filter to the window.
    #    (Skip the bundle when the user wants provisional data — the bundle is
    #    published-only, so provisional must come from a live fetch.)
    if (!prov) {
      bundle <- load_site_bundle(site)
      if (!is.null(bundle)) {
        d0 <- filter_window(bundle, s0, e0)
        if (sum(!is.na(d0$tagID)) > 0)
          return(ingest(d0, sprintf("%s · %s", site_label(site), fmt_range(s0, e0))))
        # window had no captures in the bundle -> fall through to live (if enabled)
      }
    }

    # 2) live fetch — OPTIONAL. In a bundle-only build, explain instead of failing.
    if (!LIVE_FETCH) {
      session$sendCustomMessage("loadDone", list())
      showNotification(
        if (prov) "Provisional/live data isn't available in this build — uncheck it to use the offline bundle."
        else "That site & window isn't in the offline bundle. Try a wider date window.",
        type = "warning", duration = 7)
      return(invisible())
    }
    key <- paste(site, s0, e0, prov, sep = "|")
    res <- if (!is.null(fetch_cache[[key]])) fetch_cache[[key]] else tryCatch(
      fetch_neon_mam(site, s0, e0, provisional = prov),
      error = function(e) { showNotification(paste("NEON fetch failed:", conditionMessage(e)),
                                             type = "error", duration = 8); NULL })
    if (is.null(res)) { session$sendCustomMessage("loadDone", list()); return(invisible()) }
    fetch_cache[[key]] <- res
    ingest(res, sprintf("%s · %s%s", site_label(site), fmt_range(s0, e0),
                        if (prov) " · incl. provisional" else ""))
  }

  observeEvent(input$loadBtn,
    load_site(input$site, input$dateRange[1], input$dateRange[2], isTRUE(input$provisional)))

  # ---- national site-picker map: click a site -> load its full record ------
  # Loads the whole bundled window for the chosen site (the friendliest default
  # from the landing map), raises the loading overlay from the server (the map
  # has no inline onclick), and syncs the sidebar selects through the cascade.
  load_site_full <- function(code) {
    if (is.null(code) || code == "") return(invisible())
    row <- if (!is.null(SITE_INDEX)) SITE_INDEX[SITE_INDEX$site == code, ] else NULL
    nm  <- if (!is.null(row) && nrow(row)) row$name[1] else code
    y1  <- if (!is.null(row) && nrow(row) && !is.na(row$year_min[1])) row$year_min[1] else 2013L
    s0  <- as.Date(sprintf("%d-01-01", y1)); e0 <- Sys.Date()
    st  <- if (!is.null(row) && nrow(row)) row$state[1] else NULL
    if (!is.null(st) && !is.na(st)) { rv$pendingSite <- code; updateSelectInput(session, "stateSel", selected = st) }
    updateDateRangeInput(session, "dateRange", start = s0, end = e0)
    session$sendCustomMessage("smtLoadStart", list(label = sprintf("%s — %s", code, nm)))
    load_site(code, s0, e0, FALSE)
  }
  observeEvent(input$pickerMap_marker_click, {
    click <- input$pickerMap_marker_click
    if (!is.null(click$id)) load_site_full(click$id)
  })
  observeEvent(input$pickFromList, load_site_full(input$pickFromList))

  # "Change site" (in the hero band) -> back to the picker-map landing
  observeEvent(input$changeSite, {
    rv$data <- NULL; rv$lb <- NULL; rv$lb_view <- NULL; rv$tag <- NULL; rv$label <- NULL
    shinyjs::hide("mainTabsWrap"); shinyjs::hide("indivPickerWrap"); shinyjs::show("splash")
  })

  observeEvent(input$demoBtn, {
    d <- load_demo()
    if (is.null(d)) { showNotification("Demo data not found.", type = "error"); return() }
    ingest(d, DEMO_META$label, is_demo = TRUE)
    showNotification(tagList(bs_icon("arrow-counterclockwise"), " Back to the Jornada demo."),
      type = "message", duration = 4)
  })

  # ---- selecting an individual -------------------------------------------
  pick_individual <- function(tag) {
    if (is.null(tag) || is.na(tag) || tag == "") return()
    rv$tag <- tag
    if (!identical(input$indiv, tag))
      updateSelectizeInput(session, "indiv", selected = tag)
    nav_select("tabs", "dossier")
    row <- rv$lb[rv$lb$tagID == tag, ]
    if (nrow(row) && row$rarity[1] %in% c("Epic", "Legendary")) {
      session$sendCustomMessage("confetti", list(big = row$rarity[1] == "Legendary"))
    }
  }

  observeEvent(input$indiv, {
    if (!is.null(input$indiv) && input$indiv != "" && !identical(input$indiv, rv$tag))
      pick_individual(input$indiv)
  }, ignoreInit = TRUE)

  observeEvent(input$leaderboard_rows_selected, {
    i <- input$leaderboard_rows_selected
    if (length(i) && !is.null(rv$lb_view)) pick_individual(rv$lb_view$tagID[i])
  })

  observeEvent(input$surpriseBtn, {
    lb <- rv$lb; req(lb)
    pool <- lb$tagID[lb$rarity %in% c("Legendary", "Epic")]
    if (length(pool) == 0) pool <- lb$tagID[seq_len(min(20, nrow(lb)))]
    pick_individual(sample(pool, 1))
  })

  # ensure an individual is selected (for tabs that need one) -> pick the star
  ensure_individual <- function() {
    if (!is.null(rv$tag)) return(invisible())
    lb <- rv$lb; if (is.null(lb) || nrow(lb) == 0) return(invisible())
    tag <- lb$tagID[1]
    rv$tag <- tag
    updateSelectizeInput(session, "indiv", selected = tag)
  }

  # ---- Overview home-nav buttons (Girth-style quick jumps) ---------------
  observeEvent(input$goMap,        nav_select("tabs", "map"))
  observeEvent(input$goCommunity,  nav_select("tabs", "community"))
  observeEvent(input$goPopulation, nav_select("tabs", "population"))
  observeEvent(input$goFame,       nav_select("tabs", "fame"))
  observeEvent(input$goRange, {    # heatmap/replay need an individual — pick the star
    ensure_individual(); nav_select("tabs", "homerange")
  })

  # ---- splash / landing: the national site-picker map --------------------
  # "Select your site" — a map of all bundled NEON sites. Tap a dot to load it.
  # Dot size = total captures (log-scaled); color = the ecological family of the
  # site's most-caught species. Falls back to a clickable list (a11y / no-JS).
  output$splash <- renderUI({
    if (!is.null(rv$data)) return(NULL)
    idx <- SITE_INDEX

    # graceful fallback to a simple prompt if the index wasn't precomputed
    if (is.null(idx) || nrow(idx) == 0) {
      return(div(class = "splash",
        div(class = "splash-icon", "\U0001F43E"),
        h3("Explore the small mammals of the NEON network"),
        p("Pick a ", tags$b("state"), " then a ", tags$b("site"), " in the sidebar, or jump into the demo."),
        actionButton("demoBtn2", tagList(bs_icon("stars"), " Explore the Jornada demo instantly"),
                     class = "btn-primary btn-lg", onclick = "smtLoadStart('Jornada — demo dataset')")))
    }

    # legend — only the groups actually present, in canonical family order
    g_order <- vapply(GENUS_GROUPS, function(g) g$key, character(1))
    grps <- unique(idx[, c("group_key", "group_label", "group_color")])
    grps <- grps[order(match(grps$group_key, g_order)), ]
    legend <- div(class = "picker-legend",
      tags$span(class = "pl-label", "Most-caught family:"),
      lapply(seq_len(nrow(grps)), function(i)
        tags$span(class = "pl-item",
          tags$span(class = "pl-dot", style = sprintf("background:%s", grps$group_color[i])),
          grps$group_label[i])))

    # a11y / no-JS fallback: every site as a clickable link (one shared input)
    ord <- idx[order(idx$name), ]
    fallback <- tags$details(class = "picker-list",
      tags$summary(tagList(bs_icon("list-ul"), " Browse all ", nrow(ord), " sites as a list")),
      div(class = "picker-list-grid",
        lapply(seq_len(nrow(ord)), function(i)
          tags$a(class = "picker-list-link", href = "#",
            onclick = sprintf("Shiny.setInputValue('pickFromList','%s',{priority:'event'});return false;", ord$site[i]),
            tags$b(ord$site[i]), sprintf(" — %s ", ord$name[i]),
            tags$span(class = "pll-meta", sprintf("%s · %s caps", ord$state[i], format(ord$captures[i], big.mark = ",")))))))

    has_species <- !is.null(SPECIES_RANGES) && nrow(SPECIES_RANGES) > 0
    div(class = "splash splash-map",
      div(class = "splash-icon", "\U0001F43E"),
      h3("Explore the NEON small-mammal network"),
      p("NEON live-traps small mammals at ", tags$b(nrow(idx)), " field sites across the U.S. and Puerto Rico. ",
        "Explore ", tags$b("by site"), " — tap a dot to dive in — or ", tags$b("by species"),
        ", to see where one animal turns up across the country."),

      # mode toggle: by-site picker  vs  by-species range map
      if (has_species) div(class = "picker-mode",
        radioButtons("pickMode", NULL, inline = TRUE,
          choiceNames = list(tagList(bs_icon("geo-alt-fill"), " By site"),
                             tagList(bs_icon("bezier2"), " By species")),
          choiceValues = c("site", "species"), selected = "site")),

      # by-site: the family-color legend
      conditionalPanel("input.pickMode != 'species'", legend),

      # by-species: a species picker + a live range summary
      if (has_species) conditionalPanel("input.pickMode == 'species'",
        div(class = "range-controls",
          selectizeInput("rangeSpecies", label = NULL, width = "100%",
            choices = species_choices(),
            options = list(placeholder = "Pick a species to map its range…")),
          uiOutput("rangeSummary"))),

      div(class = "picker-map-wrap",
        spin(leafletOutput("pickerMap", height = "560px"), img = "rat1.gif"),
        div(class = "picker-map-hint", bs_icon("hand-index-thumb"),
            " Drag to pan · scroll to zoom · Alaska & Puerto Rico are out there too")),
      div(class = "picker-actions",
        actionButton("demoBtn2", tagList(bs_icon("stars"), " Or jump straight into the Jornada demo"),
                     class = "btn-primary btn-lg", onclick = "smtLoadStart('Jornada — demo dataset')"),
        actionButton("compareBtn", tagList(bs_icon("bar-chart-steps"), " Compare two sites"),
                     class = "btn-outline-dark btn-lg ms-2")),
      fallback
    )
  })

  # radius helper: 6–24 px on a log scale, self-consistent within whichever set
  picker_radius <- function(v) {
    lc <- log1p(pmax(v, 0))
    6 + 18 * (lc - min(lc)) / (max(lc) - min(lc) + 1e-9)
  }
  picker_label_opts <- leaflet::labelOptions(direction = "auto", opacity = 0.97,
    style = list("border-color" = "rgba(12,35,75,.25)", "border-radius" = "8px",
                 "box-shadow" = "0 6px 22px rgba(12,35,75,.18)", "padding" = "8px 10px"))

  # add the all-sites markers (by-site mode): size = captures, color = family
  add_site_markers <- function(map) {
    idx <- SITE_INDEX
    labs <- lapply(seq_len(nrow(idx)), function(i) htmltools::HTML(sprintf(
      "<div class='pm-pop'><div class='pm-pop-t'>%s %s</div>
       <div class='pm-pop-s'>%s, %s · NEON %s</div>
       <div class='pm-pop-n'><b>%s</b> captures · <b>%s</b> individuals · <b>%s</b> species</div>
       <div class='pm-pop-sp'>Most caught: <i>%s</i></div>
       <div class='pm-pop-go'>Click to explore &rarr;</div></div>",
      idx$emoji[i], idx$site[i], idx$name[i], idx$state[i], idx$domain[i],
      format(idx$captures[i], big.mark = ","), format(idx$individuals[i], big.mark = ","),
      idx$species[i], idx$top_species[i])))
    leaflet::addCircleMarkers(map, data = idx, lng = ~lng, lat = ~lat, layerId = ~site,
      radius = picker_radius(idx$captures), stroke = TRUE, color = "#ffffff", weight = 1.5,
      opacity = 1, fillColor = ~group_color, fillOpacity = 0.85, label = labs,
      labelOptions = picker_label_opts, options = leaflet::markerOptions(riseOnHover = TRUE))
  }

  # add one species' range markers (by-species mode): size = that species' local
  # abundance, all one family color; clicking a site still loads it
  add_species_markers <- function(map, species) {
    r <- SPECIES_RANGES[SPECIES_RANGES$scientificName == species, , drop = FALSE]
    if (nrow(r) == 0) return(map)
    col <- r$group_color[1]
    labs <- lapply(seq_len(nrow(r)), function(i) htmltools::HTML(sprintf(
      "<div class='pm-pop'><div class='pm-pop-t'>%s %s</div>
       <div class='pm-pop-s'>%s, %s</div>
       <div class='pm-pop-n'><b>%s</b> individuals · <b>%s</b> captures here</div>
       <div class='pm-pop-go'>Click to open this site &rarr;</div></div>",
      r$emoji[i], r$site[i], r$name[i], r$state[i],
      format(r$individuals[i], big.mark = ","), format(r$captures[i], big.mark = ","))))
    leaflet::addCircleMarkers(map, data = r, lng = ~lng, lat = ~lat, layerId = ~site,
      radius = picker_radius(r$individuals), stroke = TRUE, color = "#ffffff", weight = 1.5,
      opacity = 1, fillColor = col, fillOpacity = 0.85, label = labs,
      labelOptions = picker_label_opts, options = leaflet::markerOptions(riseOnHover = TRUE))
  }

  # base map drawn once (tiles + view + initial by-site markers)
  output$pickerMap <- renderLeaflet({
    req(SITE_INDEX, nrow(SITE_INDEX) > 0)
    leaflet(options = leafletOptions(minZoom = 2, worldCopyJump = TRUE)) %>%
      addProviderTiles("CartoDB.Positron", options = providerTileOptions(noWrap = TRUE)) %>%
      setView(lng = -96, lat = 41, zoom = 4) %>%
      add_site_markers()
  })

  # swap markers when the user toggles mode or picks a species (proxy = no reflow)
  observeEvent(list(input$pickMode, input$rangeSpecies), {
    req(SITE_INDEX)
    map <- leaflet::leafletProxy("pickerMap") %>% leaflet::clearMarkers()
    if (identical(input$pickMode, "species") && !is.null(input$rangeSpecies) &&
        nzchar(input$rangeSpecies)) {
      add_species_markers(map, input$rangeSpecies)
    } else {
      add_site_markers(map)
    }
  }, ignoreInit = TRUE)

  # live range summary under the species picker
  output$rangeSummary <- renderUI({
    sp <- input$rangeSpecies
    if (is.null(sp) || !nzchar(sp) || is.null(SPECIES_RANGES)) return(NULL)
    r <- SPECIES_RANGES[SPECIES_RANGES$scientificName == sp, , drop = FALSE]
    if (nrow(r) == 0) return(NULL)
    r <- r[order(-r$individuals), ]
    n_sites_total <- if (!is.null(SITE_INDEX)) nrow(SITE_INDEX) else nrow(r)
    nick <- r$nickname[1]
    div(class = "range-summary", style = sprintf("--rc:%s", r$group_color[1]),
      span(class = "rs-emoji", r$emoji[1]),
      div(class = "rs-body",
        div(class = "rs-name", em(sp),
            if (!is.na(nick)) span(class = "rs-nick", paste0(" · ", nick))),
        div(class = "rs-stats",
          HTML(sprintf("found at <b>%d</b> of %d sites · <b>%s</b> individuals · most abundant at <b>%s</b> (%s, %s)",
            nrow(r), n_sites_total, format(sum(r$individuals), big.mark = ","),
            r$site[1], r$name[1], r$state[1])))))
  })

  observeEvent(input$demoBtn2, {
    d <- load_demo(); req(!is.null(d)); ingest(d, DEMO_META$label, is_demo = TRUE)
  })

  # ---- compare two sites (modal) -----------------------------------------
  compare_site_choices <- function() {
    idx <- SITE_INDEX
    if (is.null(idx)) return(setNames(neon_sites$site, neon_sites$site))
    o <- idx[order(idx$name), ]
    setNames(o$site, sprintf("%s — %s, %s", o$site, o$name, o$state))
  }
  observeEvent(input$compareBtn, {
    ch <- compare_site_choices()
    showModal(modalDialog(
      title = tagList(bs_icon("bar-chart-steps"), " Compare two sites"),
      easyClose = TRUE, size = "l", footer = modalButton("Close"),
      div(class = "compare-pickers",
        selectizeInput("cmpA", "Site A", choices = ch,
                       selected = if ("JORN" %in% ch) "JORN" else unname(ch)[1], width = "100%"),
        selectizeInput("cmpB", "Site B", choices = ch,
                       selected = if ("HARV" %in% ch) "HARV" else unname(ch)[2], width = "100%")),
      uiOutput("compareOut")
    ))
  })

  # build a one-site metric pack from its bundle (instant; bundles are tiny)
  compare_pack <- function(site) {
    b <- load_site_bundle(site)
    if (is.null(b)) return(NULL)
    d <- clean_mam(b)
    if (is.null(d) || sum(d$is_capture) == 0) return(NULL)
    cs <- community_stats(d)
    hn <- hill_numbers(d)
    sp <- utils::head(species_summary(d), 5)
    yrs <- range(d$year[is.finite(d$year)])
    list(site = site, label = site_label(site), cs = cs, hn = hn, sp = sp,
         years = if (all(is.finite(yrs))) yrs else c(NA, NA))
  }

  output$compareOut <- renderUI({
    a <- input$cmpA; b <- input$cmpB
    req(a, b)
    if (identical(a, b)) return(div(class = "compare-hint", bs_icon("info-circle"),
      " Pick two different sites to compare."))
    pa <- compare_pack(a); pb <- compare_pack(b)
    if (is.null(pa) || is.null(pb)) return(div(class = "compare-hint", bs_icon("exclamation-triangle"),
      " One of those sites isn't in the offline bundle."))

    # winner-aware metric row: higher value gets a subtle highlight
    row <- function(lab, va, vb, fmt = function(x) format(x, big.mark = ","), higher = TRUE, tip = NULL) {
      hl <- if (is.na(va) || is.na(vb) || va == vb) c("", "")
            else if ((va > vb) == higher) c("cmp-win", "") else c("", "cmp-win")
      tags$tr(
        tags$td(class = "cmp-lab", lab, if (!is.null(tip)) info_pop(lab, p(tip))),
        tags$td(class = paste("cmp-val", hl[1]), fmt(va)),
        tags$td(class = paste("cmp-val", hl[2]), fmt(vb)))
    }
    sp_list <- function(p) tags$div(class = "cmp-splist",
      lapply(seq_len(nrow(p$sp)), function(i)
        tags$div(class = "cmp-sp", span(p$sp$emoji[i]), em(p$sp$scientificName[i]),
                 span(class = "cmp-sp-n", paste0(format(p$sp$individuals[i], big.mark = ","), " ind")))))

    tagList(
      tags$table(class = "compare-table",
        tags$thead(tags$tr(tags$th(""),
          tags$th(div(class = "cmp-head", pa$site), div(class = "cmp-head-sub", pa$cs$plots, " plots")),
          tags$th(div(class = "cmp-head", pb$site), div(class = "cmp-head-sub", pb$cs$plots, " plots")))),
        tags$tbody(
          row("Captures", pa$cs$total_captures, pb$cs$total_captures),
          row("Individuals", pa$cs$individuals, pb$cs$individuals),
          row("Species (richness)", pa$cs$species, pb$cs$species),
          row("Effective common species", pa$hn$q1, pb$hn$q1, fmt = function(x) format(x, nsmall = 1),
              tip = "Hill q1 = exp(Shannon): the effective number of common species. Higher = more diverse."),
          row("Evenness (0–1)", pa$hn$even, pb$hn$even, fmt = function(x) format(x, nsmall = 2),
              tip = "How evenly captures spread across species. Near 1 = even; low = a few species dominate."),
          row("Recapture rate", pa$cs$recap_rate, pb$cs$recap_rate, fmt = function(x) paste0(x, "%")),
          row("Trap-nights (effort)", pa$cs$trap_nights, pb$cs$trap_nights))),
      div(class = "compare-species",
        div(class = "cmp-col", div(class = "cmp-col-h", "Top species — ", pa$site), sp_list(pa)),
        div(class = "cmp-col", div(class = "cmp-col-h", "Top species — ", pb$site), sp_list(pb))),
      div(class = "compare-foot", bs_icon("info-circle"),
        " Higher value highlighted per row. Diversity uses Hill numbers over distinct individuals; richness is the raw species count.")
    )
  })

  # ---- hero stat band -----------------------------------------------------
  output$heroStats <- renderUI({
    d <- rv$data; if (is.null(d)) return(NULL)
    cs <- community_stats(d, rv$lb)

    # every stat card is a clickable "door" -> opens a ranked-breakdown modal
    click <- function(key) sprintf("Shiny.setInputValue('statClick', '%s', {priority:'event'})", key)
    vb <- function(value, title, icon, accent, key, tip = NULL, pct = FALSE) {
      div(class = "stat-card", style = sprintf("--accent:%s;", accent), title = tip, onclick = click(key),
        div(class = "stat-icon", bs_icon(icon)),
        div(class = "stat-body",
          div(class = "stat-value",
            span(class = "count-up", `data-target` = value, "0"), if (pct) "%"),
          div(class = "stat-title", title, span(class = "stat-q", bs_icon("box-arrow-up-right")))
        ))
    }

    div(
      div(class = "hero-site",
        bs_icon("broadcast-pin"), span(class = "hero-site-label", rv$label),
        if (isTRUE(rv$is_demo)) span(class = "demo-pill", bs_icon("stars"), " DEMO"),
        span(class = "hero-site-range", fmt_range(cs$date_min, cs$date_max)),
        actionLink("changeSite", tagList(bs_icon("arrow-left-circle"), " change site"),
                   class = "hero-change"),
        tags$a(class = "hero-report", href = "#", onclick = "smtPrintReport();return false;",
               bs_icon("file-earmark-text"), " report card"),
        span(class = "hero-site-hint", bs_icon("hand-index"), " tap any stat for the full ranking")),
      div(class = "stat-grid",
        vb(cs$total_captures, "Captures",      "bullseye",        "#0C234B", "captures",
           tip = "Total times an animal was caught & handled — click to see captures by plot."),
        vb(cs$individuals,    "Individuals",   "fingerprint",     "#2f7fb5", "individuals",
           tip = "Distinct animals (unique ear-tag IDs) — click for the most-caught individuals."),
        vb(cs$species,        "Species",       "diagram-3-fill",  "#1a7f37", "species",
           tip = "Distinct species identified — click for species ranked by abundance."),
        vb(cs$recap_rate,     "Recapture rate","arrow-repeat",    "#138086", "recapture", pct = TRUE,
           tip = "Share of captures that were re-encounters — click for recapture rate by species."),
        vb(cs$trap_nights,    "Trap-nights",   "moon-stars-fill", "#5b3a8a", "trapnights",
           tip = "Total trapping effort — click for effort & catch-rate by plot."),
        vb(cs$legendary,      "Legends (10+)", "trophy-fill",     "#c9a300", "legends",
           tip = "Individuals caught 10+ times — click for the full list of legends.")
      )
    )
  })

  # ---- clickable stat -> ranked breakdown modal --------------------------
  observeEvent(input$statClick, {
    d <- rv$data; lb <- rv$lb; req(d, lb)
    bk <- stat_breakdown(d, lb, input$statClick)
    req(!is.null(bk))
    rows <- bk$rows
    has_tag <- "tag" %in% names(rows)
    rank_items <- lapply(seq_len(nrow(rows)), function(i) {
      clickable <- has_tag && !is.na(rows$tag[i])
      tags$li(class = paste("rank-row", if (clickable) "rank-click" else ""),
        onclick = if (clickable) sprintf(
          "Shiny.setInputValue('modalPick','%s',{priority:'event'})", rows$tag[i]),
        span(class = paste("rank-num", if (rows$rank[i] <= 3) "top" else ""),
             paste0("#", rows$rank[i])),
        span(class = "rank-name", HTML(rows$name[i]),
             if (clickable) span(class = "rank-go", bs_icon("chevron-right"))),
        span(class = "rank-metric", rows$metric[i]),
        span(class = "rank-sub", rows$sub[i]))
    })
    showModal(modalDialog(
      title = tagList(bk$icon, " ", bk$title),
      easyClose = TRUE, size = "l", footer = modalButton("Close"),
      div(class = "rank-modal-sub", bk$subtitle),
      if (nzchar(bk$insight %||% "")) div(class = "rank-modal-insight", bs_icon("lightbulb"), " ", HTML(bk$insight)),
      tags$ol(class = "rank-list", rank_items)
    ))
  })

  # click an animal inside a modal -> open its dossier
  observeEvent(input$modalPick, {
    removeModal()
    pick_individual(input$modalPick)
  })

  # ---- overview: narrative insights + "meet the locals" ------------------
  output$siteInsights <- renderUI({
    d <- rv$data; req(d)
    tags$ul(class = "insights-list",
            lapply(site_insights(d, rv$lb), function(s) tags$li(HTML(s))))
  })

  output$meetLocals <- renderUI({
    d <- rv$data; req(d)
    s <- utils::head(species_summary(d), 6)
    if (nrow(s) == 0) return(div(class = "empty-state", "No species identified yet."))
    div(class = "locals-grid",
      lapply(seq_len(nrow(s)), function(i) {
        sci <- s$scientificName[i]
        wiki <- paste0("https://en.wikipedia.org/wiki/", gsub(" ", "_", sub(" sp\\.$", "", sci)))
        tags$a(class = "local-card", href = wiki, target = "_blank", rel = "noopener",
          div(class = "local-emoji", s$emoji[i]),
          div(class = "local-body",
            div(class = "local-name", em(sci), span(class = "local-go", bs_icon("box-arrow-up-right"))),
            if (!is.na(s$nickname[i])) div(class = "local-nick", s$nickname[i]),
            div(class = "local-stats",
              span(class = "ls-strong", fmt_int(s$individuals[i])), " individuals · ",
              span(class = "ls-strong", fmt_int(s$captures[i])), " captures"),
            div(class = "local-blurb", species_blurb(sci))))
      }))
  })

  # ---- leaderboard --------------------------------------------------------
  observe({
    lb <- rv$lb; if (is.null(lb)) return()
    rv$lb_view <- leaderboard_by(lb, input$leaderCat %||% "captures")
  })

  output$leaderboard <- DT::renderDT({
    v <- rv$lb_view; req(v)
    cat_key <- input$leaderCat %||% "captures"

    medal <- function(rank) paste0("#", rank)
    rar_badge <- function(tier) {
      m <- lapply(tier, rarity_meta)
      vapply(seq_along(tier), function(i) sprintf(
        "<span class='tag-badge' style='background:%s;border-color:%s;color:#fff'>%s %s</span>",
        m[[i]]$color, m[[i]]$color, m[[i]]$icon, tier[i]), character(1))
    }
    chonk_badge <- function(tier, pct) {
      ifelse(is.na(pct) | tier == "—", "<span class='muted'>—</span>",
        sprintf("<span class='chonk-badge' style='--p:%s'>%s · %s</span>", pct, tier, pct))
    }

    df <- tibble::tibble(
      Rank = vapply(seq_len(nrow(v)), function(i) medal(i), character(1)),
      Individual = sprintf("<span class='ind-cell'><span class='ind-emoji'>%s</span><span class='ind-id'>%s</span><span class='ind-sp'>%s</span></span>",
                           v$emoji, v$short, v$scientificName),
      Captures = v$captures,
      Career = ifelse(v$career_days > 0, paste0(v$career_days, " d"), "—"),
      Traps = v$n_traps,
      Roam = ifelse(is.na(v$roam_m), "—", paste0(v$roam_m, " m")),
      `Max wt` = ifelse(is.na(v$max_weight), "—", paste0(v$max_weight, " g")),
      Chonk = chonk_badge(v$chonk_tier, v$chonk_pct),
      Rarity = rar_badge(v$rarity),
      Plot = v$home_plot
    )

    # subtle bar-in-cell for the active ranking metric
    active_col <- switch(cat_key, captures = "Captures", weight = "Max wt",
                         career = "Career", roam = "Roam", chonk = "Chonk")

    DT::datatable(df, escape = FALSE, rownames = FALSE, selection = "single",
      class = "compact stripe hover nowrap leader-dt",
      options = list(pageLength = 15, dom = "ftip", scrollX = TRUE,
                     columnDefs = list(list(className = "dt-center",
                                            targets = c(0, 2, 3, 4, 5, 6, 7, 8, 9))),
                     language = list(search = "", searchPlaceholder = "filter individuals…"))
    )
  })

  # ---- bio repository links ----------------------------------------------
  output$bioLinks <- renderUI({
    tag <- rv$tag; if (is.null(tag)) return(note_plot(PICK_MSG, "\U0001F50D"))  # sidebar links stay hidden until a pick
    sp <- rv$lb$scientificName[rv$lb$tagID == tag][1]
    if (is.na(sp)) return(NULL)
    parts <- strsplit(sp, " ")[[1]]
    genus <- parts[1]; epithet <- ifelse(length(parts) > 1, parts[2], "")
    q <- paste0(genus, "+", epithet)
    div(class = "bio-links",
      div(class = "bio-links-title", bs_icon("box-arrow-up-right"), " NEON BioRepository"),
      tags$a(href = paste0("https://biorepo.neonscience.org/portal/collections/list.php?usethes=1&taxa=", q),
             target = "_blank", bs_icon("archive"), " Specimen records"),
      tags$a(href = paste0("https://biorepo.neonscience.org/portal/imagelib/search.php?usethes=1&taxa=", q),
             target = "_blank", bs_icon("images"), " Image library")
    )
  })

  # ---- individual helpers -------------------------------------------------
  ind_rows <- reactive({
    tag <- rv$tag; d <- rv$data; req(tag, d)
    dplyr::filter(d, .data$tagID == tag) %>% dplyr::arrange(.data$date)
  })

  # ---- dossier hero card --------------------------------------------------
  output$dossierHero <- renderUI({
    tag <- rv$tag
    if (is.null(tag)) return(div(class = "empty-state",
      div(class = "empty-icon", "\U0001F50D"),
      h4("No individual selected"),
      p("Open the ", tags$b("Hall of Fame"), " and click any rodent — or hit ",
        tags$b("Surprise me"), " in the sidebar.")))
    row <- rv$lb[rv$lb$tagID == tag, ]; req(nrow(row) == 1)
    rm <- rarity_meta(row$rarity[1])
    nick <- if (!is.na(row$nickname[1])) row$nickname[1] else "small mammal"

    stat <- function(value, label) div(class = "ds-stat",
      div(class = "ds-stat-v", value), div(class = "ds-stat-l", label))

    fmt_date <- function(x) if (is.na(x)) "—" else format(x, "%b %Y")

    div(class = "dossier-card", style = sprintf("--rarity:%s; --rglow:%s;", rm$color, rm$glow),
      div(class = "ds-left",
        div(class = "ds-emoji", row$emoji[1]),
        glow_badge(paste(rm$icon, row$rarity[1]), rm$color, rm$glow),
        if (!is.na(row$chonk_tier[1]) && row$chonk_tier[1] != "—")
          div(class = "ds-chonk", "\U0001F9CA ", row$chonk_tier[1])
      ),
      div(class = "ds-main",
        div(class = "ds-id", row$short[1], span(class = "ds-nick", nick),
          if (isTRUE(row$tag_suspect[1]))
            span(class = "ds-warn", title = "Career exceeds plausible lifespan or has a long gap — possibly a reused ear tag",
                 bs_icon("exclamation-triangle-fill"), " verify tag"),
          if (isTRUE(row$id_uncertain[1]))
            span(class = "ds-warn", title = "This tag was recorded under more than one species",
                 bs_icon("question-circle-fill"), " ID uncertain")
        ),
        div(class = "ds-sci", em(row$scientificName[1])),
        div(class = "ds-stats",
          stat(row$captures[1], "captures"),
          stat(ifelse(row$career_days[1] > 0, paste0(row$career_days[1], "d"), "—"), "career span"),
          stat(row$n_traps[1], "traps used"),
          stat(ifelse(is.na(row$mdm_m[1]), "—", paste0(row$mdm_m[1], "m")), "max move"),
          stat(ifelse(is.na(row$avg_weight[1]), "—", paste0(row$avg_weight[1], "g")), "avg weight"),
          stat(row$home_plot[1], "home plot")
        ),
        div(class = "ds-meta",
          span(bs_icon("calendar-event"), " First seen ", tags$b(fmt_date(row$first_seen[1]))),
          span(bs_icon("calendar-check"), " Last seen ", tags$b(fmt_date(row$last_seen[1]))),
          span(bs_icon("compass"), " Roam ", tags$b(ifelse(is.na(row$roam_m[1]), "—", paste0(row$roam_m[1], "m")))),
          span(bs_icon("gender-ambiguous"), " ", ifelse(is.na(row$sex[1]), "?", row$sex[1])),
          span(bs_icon("bezier2"), " ", ifelse(is.na(row$lifeStage[1]), "?", row$lifeStage[1]))
        )
      )
    )
  })

  # ---- measurements through time -----------------------------------------
  output$measPlot <- renderPlotly({
    tag <- rv$tag; if (is.null(tag)) return(note_plot(PICK_MSG, "\U0001F50D"))
    df <- ind_rows()
    if (!any(is.finite(df$weight)) && !any(is.finite(df$hindfootLength)))
      return(note_plot("No weight or hind-foot<br>measurements recorded for this animal", "\U0001F4CF"))
    sp <- rv$lb$scientificName[rv$lb$tagID == tag][1]
    sp_w <- rv$data %>% dplyr::filter(.data$scientificName == sp, !is.na(.data$weight),
                                      .data$weight > 0) %>% dplyr::pull(.data$weight)
    qs <- if (length(sp_w) >= 8) stats::quantile(sp_w, c(.25, .5, .75), names = FALSE) else NULL
    xr <- range(df$date)

    p <- plot_ly()
    # shaded species weight IQR band -> the animal's track reads as above/below normal
    if (!is.null(qs)) {
      p <- p %>%
        add_trace(x = xr, y = rep(qs[3], 2), type = "scatter", mode = "lines",
          line = list(width = 0), showlegend = FALSE, hoverinfo = "skip", name = "q75") %>%
        add_trace(x = xr, y = rep(qs[1], 2), type = "scatter", mode = "lines", fill = "tonexty",
          fillcolor = "rgba(27,96,81,0.10)", line = list(width = 0),
          name = "species IQR", hoverinfo = "skip") %>%
        add_trace(x = xr, y = rep(qs[2], 2), type = "scatter", mode = "lines",
          line = list(color = "rgba(31,42,48,0.35)", width = 1, dash = "dash"),
          name = "species median wt", hoverinfo = "skip")
    }
    p <- p %>% add_trace(
      data = df, x = ~date, y = ~weight, name = "Weight (g)",
      type = "scatter", mode = "lines+markers",
      line = list(color = "#16386e", width = 2), marker = list(color = "#16386e", size = 8),
      hovertemplate = "%{x|%b %d, %Y}<br><span style='color:#16386e'>●</span> Weight: %{y} g<extra></extra>")
    p <- p %>% add_trace(
      data = df, x = ~date, y = ~hindfootLength, name = "Hind foot (mm)", yaxis = "y2",
      type = "scatter", mode = "lines+markers",
      line = list(color = "#AB0520", width = 2, dash = "dot"), marker = list(color = "#AB0520", size = 7),
      hovertemplate = "%{x|%b %d, %Y}<br><span style='color:#AB0520'>●</span> Hind foot: %{y} mm<extra></extra>")

    # call out the heaviest capture
    ann <- list()
    if (any(is.finite(df$weight))) {
      i <- which.max(df$weight)
      ann <- list(list(x = df$date[i], y = df$weight[i],
        text = sprintf("heaviest ♦ %sg", df$weight[i]), showarrow = TRUE, arrowcolor = "#c9a300",
        ax = 0, ay = -28, font = list(color = "#c9a300", size = 11)))
    }

    plotly_theme(p) %>% plotly::layout(
      yaxis  = list(title = "Weight (g)", color = "#16386e"),
      yaxis2 = list(title = "Hind foot (mm)", color = "#AB0520", overlaying = "y",
                    side = "right", gridcolor = "rgba(0,0,0,0)"),
      xaxis  = list(title = ""), hovermode = "x unified", annotations = ann)
  })

  # ---- chonk gauge --------------------------------------------------------
  output$chonkGauge <- renderPlotly({
    tag <- rv$tag; if (is.null(tag)) return(note_plot(PICK_MSG, "\U0001F50D"))
    row <- rv$lb[rv$lb$tagID == tag, ]
    pct <- row$chonk_pct[1]
    if (is.na(pct))
      return(note_plot("Not enough adult weights<br>for this species to rank", "\U0001F9CA"))
    plot_ly(type = "indicator", mode = "gauge+number+delta",
      value = pct,
      number = list(suffix = "", font = list(color = "#1f2a30", size = 46)),
      delta = list(reference = 50, suffix = " vs typical",
        increasing = list(color = "#1a7f37"), decreasing = list(color = "#2f7fb5"),
        font = list(size = 13)),
      title = list(text = sprintf("<b>%s</b><br><span style='font-size:12px;color:#6b7a85'>adult weight percentile vs %s</span>",
                                  row$chonk_tier[1], row$scientificName[1]),
                   font = list(color = "#1f2a30", size = 18)),
      gauge = list(
        axis = list(range = list(0, 100), tickcolor = "#6b7a85", tickfont = list(color = "#6b7a85")),
        bar = list(color = "#0C234B", thickness = 0.28),
        bgcolor = "rgba(0,0,0,0)", borderwidth = 0,
        steps = list(
          list(range = c(0, 20),  color = "#e3eef0"),
          list(range = c(20, 40), color = "#e6f1ea"),
          list(range = c(40, 60), color = "#f0f1ec"),
          list(range = c(60, 80), color = "#faedd6"),
          list(range = c(80, 100),color = "#f6ddd2")),
        threshold = list(line = list(color = "#AB0520", width = 3), thickness = 0.8, value = 50))
    ) %>% plotly::layout(paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
                         font = list(color = "#344049", family = "Rubik"),
                         margin = list(t = 70, b = 10, l = 30, r = 30)) %>%
      plotly::config(displayModeBar = FALSE)
  })

  # ---- body-size morphospace scatter (the honest "girth index") ----------
  output$morphoPlot <- renderPlotly({
    tag <- rv$tag; if (is.null(tag)) return(note_plot(PICK_MSG, "\U0001F50D"))
    d <- rv$data
    sp <- rv$lb$scientificName[rv$lb$tagID == tag][1]
    meas <- dplyr::filter(d, !is.na(.data$weight), !is.na(.data$hindfootLength),
                          .data$weight > 0, .data$hindfootLength > 0)
    if (nrow(meas) == 0 || is.na(sp))
      return(note_plot("No paired weight + hind-foot<br>measurements to map", "\U0001F4CF"))

    others <- dplyr::filter(meas, .data$scientificName != sp)
    focal  <- dplyr::filter(meas, .data$scientificName == sp)
    ind    <- dplyr::filter(meas, .data$tagID == tag)

    p <- plot_ly()
    # everyone else, faint, for whole-community context
    if (nrow(others) > 0)
      p <- p %>% add_trace(data = others, x = ~hindfootLength, y = ~weight,
        type = "scatter", mode = "markers", name = "other species",
        marker = list(color = "rgba(150,160,170,0.18)", size = 5),
        hovertemplate = "%{text}<br>%{x} mm · %{y} g<extra></extra>",
        text = ~scientificName)
    # the focal species cloud, colored by life stage
    if (nrow(focal) > 0) {
      stage_col <- c(adult = "#16386e", subadult = "#AB0520", juvenile = "#4bb87a", unknown = "#6c757d")
      focal$stg <- ifelse(focal$lifeStage %in% names(stage_col), focal$lifeStage, "unknown")
      for (st in intersect(c("adult","subadult","juvenile","unknown"), unique(focal$stg))) {
        sub <- focal[focal$stg == st, ]
        p <- p %>% add_trace(data = sub, x = ~hindfootLength, y = ~weight,
          type = "scatter", mode = "markers", name = paste0(sp, " · ", st),
          marker = list(color = stage_col[[st]], size = 8, opacity = 0.55,
                        line = list(color = "#ffffff", width = 0.5)),
          hovertemplate = paste0(st, "<br>%{x} mm · %{y} g<extra></extra>"))
      }
    }
    # species SMA reference line — only where the relationship is real
    ss <- species_scaling(d)
    srow <- ss[ss$scientificName == sp, ]
    if (nrow(srow) == 1 && !is.na(srow$b)) {
      fl <- focal[is.finite(focal$weight) & focal$hindfootLength > 0, ]
      a <- mean(log(fl$weight)) - srow$b * mean(log(fl$hindfootLength))
      lx <- seq(min(fl$hindfootLength), max(fl$hindfootLength), length.out = 40)
      ly <- exp(a + srow$b * log(lx))
      p <- p %>% add_trace(x = lx, y = ly, type = "scatter", mode = "lines",
        name = sprintf("size–mass fit (r=%.2f)", srow$r),
        line = list(color = "rgba(31,42,48,0.45)", width = 2, dash = "dash"),
        hoverinfo = "skip")
    }
    # the selected animal, gold, captures connected in time
    if (nrow(ind) > 0)
      p <- p %>% add_trace(data = ind[order(ind$date), ], x = ~hindfootLength, y = ~weight,
        type = "scatter", mode = "markers+lines", name = "★ this animal",
        marker = list(color = "#c9a300", size = 15, symbol = "diamond",
                      line = list(color = "#ffffff", width = 1.5)),
        line = list(color = "rgba(255,210,74,0.5)", width = 1.5),
        hovertemplate = "this animal<br>%{x} mm · %{y} g<extra></extra>")

    note <- if (!(nrow(srow) == 1 && !is.na(srow$b)))
      list(list(text = "↳ hind-foot barely predicts mass in this species — read position, not a line",
        x = 0, y = 1.08, xref = "paper", yref = "paper", showarrow = FALSE, xanchor = "left",
        font = list(color = "#6b7a85", size = 11))) else list()

    plotly_theme(p) %>% plotly::layout(
      xaxis = list(title = "Hind-foot length (mm)"),
      yaxis = list(title = "Weight (g)"),
      annotations = note, hovermode = "closest")
  })

  # ---- capture history table ---------------------------------------------
  output$capHistory <- DT::renderDT({
    tag <- rv$tag
    if (is.null(tag)) return(DT::datatable(
      data.frame(` ` = "Pick an individual first — open the Hall of Fame and tap a row.", check.names = FALSE),
      rownames = FALSE, options = list(dom = "t", ordering = FALSE)))
    df <- ind_rows() %>%
      dplyr::transmute(
        Date = format(.data$date, "%Y-%m-%d"),
        Plot = .data$plotID, Trap = .data$trapCoordinate,
        Recap = .data$recapture,
        `Weight (g)` = .data$weight, `Hind foot (mm)` = .data$hindfootLength,
        Sex = .data$sex, `Life stage` = .data$lifeStage,
        Fate = .data$fate, Remarks = .data$remarks)
    DT::datatable(df, rownames = FALSE, selection = "none",
      class = "compact stripe hover", options = list(pageLength = 10, dom = "tip", scrollX = TRUE))
  })

  # ---- trap-grid heatmap --------------------------------------------------
  output$trapHeat <- renderPlotly({
    tag <- rv$tag; if (is.null(tag)) return(note_plot(PICK_MSG, "\U0001F50D"))
    g <- trap_grid_long(rv$data, tag)
    if (sum(g$captures) == 0)
      return(note_plot("No mapped trap coordinates<br>for this animal", "\U0001F5FA️"))
    z <- matrix(0, 10, 10)
    for (k in seq_len(nrow(g))) z[g$ty[k], g$tx[k]] <- g$captures[k]
    if (isTRUE(input$blurMode)) z <- blur_grid(z)
    zmax <- max(z)

    cx <- mean(g$tx[g$captures > 0]); cy <- mean(g$ty[g$captures > 0])
    hit <- g[g$captures > 0, ]   # visited cells, drawn as points on top

    # singleton-safe colorbar: integer ticks when the animal was caught ≤ a few times
    cbar <- list(title = "caps", tickcolor = "#6b7a85", tickfont = list(color = "#6b7a85"))
    if (zmax <= 5) cbar$dtick <- 1

    p <- plot_ly(x = LETTERS[1:10], y = 1:10, z = z, type = "heatmap",
      zmin = 0, zmax = max(zmax, 1),
      colorscale = list(c(0, "#f0f3ee"), c(0.001, "#d6e8df"),
                        c(0.4, "#1a7f37"), c(0.75, "#c9a300"), c(1, "#AB0520")),
      hovertemplate = "Trap %{x}%{y}<br>Captures: %{z}<extra></extra>",
      showscale = TRUE, xgap = 2, ygap = 2, colorbar = cbar)
    # overlay actual capture points so single-capture animals still pop
    p <- p %>% add_trace(x = LETTERS[hit$tx], y = hit$ty, type = "scatter", mode = "markers",
      marker = list(size = ~pmax(7, sqrt(hit$captures) * 8), color = "#1f2a30",
                    line = list(color = "#ffffff", width = 1.5)),
      text = ~paste0("Trap ", LETTERS[hit$tx], hit$ty, " · ", hit$captures, " cap"),
      hovertemplate = "%{text}<extra></extra>", inherit = FALSE, showlegend = FALSE)
    if (is.finite(cx) && is.finite(cy))
      p <- p %>% add_trace(x = LETTERS[round(cx)], y = round(cy), type = "scatter",
        mode = "markers", marker = list(symbol = "x", size = 16, color = "#AB0520",
        line = list(color = "#ffffff", width = 2)), name = "centroid",
        hovertemplate = "home centroid<extra></extra>", inherit = FALSE)
    plotly_theme(p, legend = FALSE) %>% plotly::layout(
      xaxis = list(title = "", side = "top", showgrid = FALSE, tickfont = list(size = 11)),
      yaxis = list(title = "", autorange = "reversed", showgrid = FALSE, dtick = 1),
      showlegend = FALSE)
  })

  # ---- capture replay (animated path) ------------------------------------
  output$trapReplay <- renderPlotly({
    tag <- rv$tag; if (is.null(tag)) return(note_plot(PICK_MSG, "\U0001F50D"))
    df <- ind_rows() %>% dplyr::filter(!is.na(.data$tx), !is.na(.data$ty))
    if (nrow(df) == 0) return(note_plot("No mapped trap coordinates<br>to replay", "▶️"))
    df$step <- seq_len(nrow(df))
    # unique, ordered frame key (NOT the date string — two captures can share a
    # date and would collapse into one frame); date rides along in the label
    df$key <- sprintf("%02d · %s", df$step, format(df$date, "%b %d, %Y"))

    # cumulative frames: at step k, show captures 1..k
    frames <- do.call(rbind, lapply(df$step, function(k) {
      sub <- df[df$step <= k, ]; sub$frame <- df$key[k]; sub$age <- k - sub$step; sub
    }))
    frames$frame <- factor(frames$frame, levels = df$key)
    frames$lab <- format(frames$date, "%b %d, %Y")

    p <- frames %>%
      plot_ly() %>%
      add_trace(x = ~tx, y = ~ty, frame = ~frame, type = "scatter", mode = "lines+markers",
        line = list(color = "rgba(45,212,191,0.5)", width = 2),
        marker = list(size = ~pmax(8, 18 - age * 2), color = ~age,
          colorscale = list(c(0, "#c9a300"), c(1, "#16386e")), showscale = FALSE,
          line = list(color = "#ffffff", width = 1)),
        text = ~paste0("Trap ", LETTERS[tx], ty, "<br>", lab),
        hovertemplate = "%{text}<extra></extra>")
    plotly_theme(p, legend = FALSE) %>%
      plotly::layout(
        xaxis = list(title = "", range = c(0.5, 10.5), tickvals = 1:10, ticktext = LETTERS[1:10],
                     showgrid = TRUE, side = "top"),
        yaxis = list(title = "", range = c(10.5, 0.5), tickvals = 1:10, dtick = 1, showgrid = TRUE),
        showlegend = FALSE) %>%
      plotly::animation_opts(frame = 700, transition = 300, redraw = FALSE) %>%
      plotly::animation_slider(currentvalue = list(prefix = "Capture ",
        font = list(color = "#16386e"))) %>%
      plotly::animation_button(label = "▶ Play")
  })

  # ---- leaflet site map ---------------------------------------------------
  mapBase <- eventReactive(list(rv$data, input$reloadMapBtn, input$view), {
    d <- rv$data; if (is.null(d)) return(NULL)
    geo <- d %>% dplyr::filter(!is.na(.data$tagID), !is.na(.data$decimalLatitude)) %>%
      dplyr::group_by(.data$plotID, .data$scientificName) %>%
      dplyr::summarise(count = dplyr::n(),
                       lat = mean(.data$decimalLatitude, na.rm = TRUE),
                       lng = mean(.data$decimalLongitude, na.rm = TRUE), .groups = "drop") %>%
      dplyr::filter(!is.na(.data$lat))
    if (nrow(geo) == 0) return(NULL)

    # shared species palette so a species is the SAME color on map + charts
    sp_pal <- rv$pal %||% make_species_pal(d)
    pal <- colorFactor(unname(sp_pal[sort(unique(geo$scientificName))]),
                       domain = sort(unique(geo$scientificName)))
    r <- input$rad_size %||% 1

    txt <- sprintf("<b><i>%s</i></b><br/>Plot %s · <b>%d</b> captures",
                   geo$scientificName, geo$plotID, geo$count)
    # per-plot capture totals for a clean top-of-stack label
    plot_tot <- geo %>% dplyr::group_by(.data$plotID) %>%
      dplyr::summarise(lat = mean(.data$lat), lng = mean(.data$lng),
                       n = sum(.data$count), .groups = "drop")
    view <- input$view %||% "Esri.WorldImagery"

    leaflet(geo, options = leafletOptions(attributionControl = FALSE)) %>%
      addProviderTiles(view) %>%
      setView(lng = mean(geo$lng), lat = mean(geo$lat), zoom = 13) %>%
      addScaleBar("bottomleft", options = scaleBarOptions(imperial = FALSE)) %>%
      addCircleMarkers(~lng, ~lat, radius = ~pmax(5, sqrt(count) * 3.4 * r),
        fillColor = ~pal(scientificName), color = "#ffffff", weight = 1.2,
        fillOpacity = 0.78, opacity = 0.85,
        label = lapply(txt, htmltools::HTML),
        labelOptions = labelOptions(textsize = "13px", direction = "auto",
          style = list("background" = "#1f2937", "color" = "#eef3f8", "font-family" = "Inter",
                       "border-color" = "rgba(45,212,191,0.5)", "border-radius" = "8px"))) %>%
      # always-on plot name tags so the grid is legible at a glance
      addLabelOnlyMarkers(data = plot_tot, ~lng, ~lat, label = ~plotID,
        labelOptions = labelOptions(noHide = TRUE, direction = "top", textOnly = TRUE, offset = c(0, -14),
          style = list("color" = "#ffffff", "font-family" = "Jura", "font-weight" = "700",
                       "font-size" = "11px", "text-shadow" = "0 1px 4px #000"))) %>%
      addLegend("bottomright", pal = pal, values = ~scientificName,
        title = "Species", opacity = 0.9)
  }, ignoreNULL = FALSE)

  output$map <- renderLeaflet({
    m <- mapBase()
    if (is.null(m)) {
      return(leaflet(options = leafletOptions(attributionControl = FALSE)) %>%
        addProviderTiles("CartoDB.DarkMatter") %>% setView(-98, 39, 3) %>%
        addControl(htmltools::HTML(
          "<div style='background:#fff;color:#6b7a85;border:1px solid #e2e7df;padding:8px 12px;border-radius:8px;font-family:Rubik;box-shadow:0 2px 8px rgba(27,96,81,.12)'>\U0001F5FA️ No geographic coordinates for this site &amp; window.</div>"),
          position = "topright"))
    }
    m
  })

  # highlight selected individual's plots without redrawing the whole map
  observeEvent(list(rv$tag, mapBase()), {
    proxy <- leafletProxy("map")
    proxy %>% clearGroup("highlight")
    tag <- rv$tag; d <- rv$data
    if (is.null(tag) || is.null(d)) return()
    hl <- d %>% dplyr::filter(.data$tagID == tag, !is.na(.data$decimalLatitude)) %>%
      dplyr::group_by(.data$plotID) %>%
      dplyr::summarise(lat = mean(.data$decimalLatitude), lng = mean(.data$decimalLongitude),
                       n = dplyr::n(), .groups = "drop")
    if (nrow(hl) == 0) return()
    proxy %>% addCircleMarkers(data = hl, ~lng, ~lat, radius = 22, group = "highlight",
      fillColor = "transparent", color = "#c9a300", weight = 3, opacity = 0.9,
      label = lapply(sprintf("⭐ selected individual here (%d caps)", hl$n), htmltools::HTML))
  })

  # ---- community pulse ----------------------------------------------------
  output$speciesBar <- renderPlotly({
    d <- rv$data; req(d)
    s <- species_summary(d) %>% dplyr::slice_head(n = 14) %>% dplyr::arrange(.data$captures)
    if (nrow(s) == 0) return(note_plot("No identified captures", "\U0001F9EC"))
    s$label <- paste0(s$emoji, " ", s$scientificName)
    s$recaps_per <- round(s$captures / s$individuals, 1)  # trap-happiness
    p <- plot_ly(s, x = ~captures, y = ~factor(label, levels = label), type = "bar",
      orientation = "h",
      # color encodes captures-per-individual (something the bar length doesn't show)
      marker = list(color = ~recaps_per,
        colorscale = list(c(0, "#dcebe4"), c(0.5, "#1a7f37"), c(1, "#c9a300")), showscale = FALSE),
      customdata = ~scientificName, source = "speciesBar",
      text = ~paste0(individuals, " indiv"), textposition = "outside",
      textfont = list(color = "#6b7a85", size = 11),
      hovertemplate = "%{y}<br>%{x} captures · %{customdata}<br>%{marker.color} caps/individual<extra></extra>")
    plotly_theme(p, legend = FALSE) %>%
      plotly::layout(xaxis = list(title = "captures"), yaxis = list(title = ""),
                     showlegend = FALSE, margin = list(l = 180, t = 44)) %>%
      ctx_anno()
  })

  donut_center <- function(total, label) list(text = sprintf("<b>%s</b><br><span style='font-size:11px;color:#6b7a85'>%s</span>",
    format(total, big.mark = ","), label), showarrow = FALSE, font = list(color = "#1f2a30", size = 20))

  output$sexDonut <- renderPlotly({
    d <- rv$data; req(d)
    h <- dplyr::filter(d, !is.na(.data$tagID))
    tab <- as.data.frame(table(factor(h$sex, levels = c("F", "M", "U"))))
    names(tab) <- c("key", "n")
    # keep a fixed key->color->label mapping so slices never swap colors
    lab <- c(F = "Female", M = "Male", U = "Unknown")
    col <- c(F = "#c2255c", M = "#2f7fb5", U = "#6c757d")
    tab$label <- lab[as.character(tab$key)]
    plot_ly(tab, labels = ~label, values = ~n, type = "pie", hole = 0.62, sort = FALSE,
      marker = list(colors = unname(col[as.character(tab$key)]), line = list(color = "#ffffff", width = 2)),
      pull = c(0.03, 0, 0), textinfo = "percent", textposition = "inside",
      insidetextorientation = "horizontal", textfont = list(color = "#ffffff", size = 13),
      hovertemplate = "%{label}<br>%{value} animals · %{percent}<extra></extra>") %>%
      plotly::layout(title = list(text = "Sex", font = list(color = "#344049", size = 14)),
        paper_bgcolor = "rgba(0,0,0,0)", showlegend = TRUE,
        legend = list(orientation = "h", y = -0.05, x = 0.5, xanchor = "center", font = list(size = 11)),
        annotations = list(donut_center(sum(tab$n), "handled")),
        font = list(color = "#344049"), margin = list(t = 38, b = 30, l = 10, r = 10)) %>%
      plotly::config(displayModeBar = FALSE)
  })

  output$ageDonut <- renderPlotly({
    d <- rv$data; req(d)
    h <- dplyr::filter(d, !is.na(.data$tagID), !is.na(.data$lifeStage))
    # FIX: pin life-stage order + named colors so a stage always gets the same color
    lvls <- c("juvenile", "subadult", "adult", "unknown")
    col  <- c(juvenile = "#4bb87a", subadult = "#AB0520", adult = "#16386e", unknown = "#6c757d")
    h$stage <- factor(ifelse(h$lifeStage %in% lvls, h$lifeStage, "unknown"), levels = lvls)
    tab <- as.data.frame(table(h$stage)); names(tab) <- c("stage", "n")
    tab <- tab[tab$n > 0, , drop = FALSE]
    plot_ly(tab, labels = ~stage, values = ~n, type = "pie", hole = 0.62, sort = FALSE,
      marker = list(colors = unname(col[as.character(tab$stage)]), line = list(color = "#ffffff", width = 2)),
      textinfo = "percent", textposition = "inside", insidetextorientation = "horizontal",
      textfont = list(color = "#ffffff", size = 13),
      hovertemplate = "%{label}<br>%{value} animals · %{percent}<extra></extra>") %>%
      plotly::layout(title = list(text = "Life stage", font = list(color = "#344049", size = 14)),
        paper_bgcolor = "rgba(0,0,0,0)", showlegend = TRUE,
        legend = list(orientation = "h", y = -0.05, x = 0.5, xanchor = "center", font = list(size = 11)),
        annotations = list(donut_center(sum(tab$n), "aged")),
        font = list(color = "#344049"), margin = list(t = 38, b = 30, l = 10, r = 10)) %>%
      plotly::config(displayModeBar = FALSE)
  })

  # ---- Hill numbers: the diversity profile -------------------------------
  output$hillPlot <- renderPlotly({
    d <- rv$data; req(d)
    hn <- hill_numbers(d)
    if (hn$n_sp == 0) return(note_plot("No identified species to profile", "\U0001F9EE"))
    df <- data.frame(
      lab = factor(c("q=2 · dominant", "q=1 · common", "q=0 · richness"),
                   levels = c("q=2 · dominant", "q=1 · common", "q=0 · richness")),
      val = c(hn$q2, hn$q1, hn$q0),
      col = c("#1a7f37", "#2f7fb5", "#0C234B"))
    plot_ly(df, x = ~val, y = ~lab, type = "bar", orientation = "h",
      marker = list(color = ~col, line = list(color = "#ffffff", width = 1)),
      text = ~sprintf("%.1f", val), textposition = "outside",
      textfont = list(color = "#1f2a30", size = 13),
      hovertemplate = "%{y}: <b>%{x:.1f}</b> effective species<extra></extra>") %>%
      plotly::layout(
        xaxis = list(title = "effective number of species", rangemode = "tozero"),
        yaxis = list(title = ""), showlegend = FALSE,
        margin = list(l = 110, r = 40, t = 20, b = 40)) %>%
      plotly_theme(legend = FALSE) %>% ctx_anno()
  })

  output$hillNote <- renderUI({
    d <- rv$data; req(d)
    hn <- hill_numbers(d)
    if (hn$n_sp == 0) return(NULL)
    even_word <- if (is.na(hn$even)) "—"
      else if (hn$even >= 0.75) "very even — captures are spread across many species"
      else if (hn$even >= 0.5)  "moderately even"
      else if (hn$even >= 0.3)  "uneven — a few species dominate the catch"
      else "highly skewed — one or two species dominate"
    tile <- function(v, lab, sub, col) div(class = "hill-tile", style = sprintf("--hc:%s", col),
      div(class = "hill-v", v), div(class = "hill-l", lab), div(class = "hill-s", sub))
    div(class = "hill-note",
      div(class = "hill-tiles",
        tile(hn$q0, "richness", "all species", "#0C234B"),
        tile(hn$q1, "common", "exp(Shannon)", "#2f7fb5"),
        tile(hn$q2, "dominant", "inv. Simpson", "#1a7f37")),
      div(class = "hill-even",
        bs_icon("bar-chart-steps"),
        HTML(sprintf(" Evenness <b>%s</b> — %s.",
                     ifelse(is.na(hn$even), "—", format(hn$even, nsmall = 2)), even_word))),
      div(class = "hill-foot",
        sprintf("From %s individuals across %s species.",
                format(hn$n_ind, big.mark = ","), hn$n_sp)))
  })

  output$plotTrend <- renderPlotly({
    d <- rv$data; req(d)
    ds <- d %>% dplyr::filter(!is.na(.data$tagID), !is.na(.data$scientificName), !is.na(.data$ym)) %>%
      dplyr::group_by(.data$plotID, .data$scientificName, .data$ym) %>%
      dplyr::summarise(count = dplyr::n(), .groups = "drop")
    if (nrow(ds) == 0) return(note_plot("No dated captures to chart", "\U0001F4C8"))
    ds$date <- as.Date(paste0(ds$ym, "-01"))
    # declutter: keep only the species with a meaningful number of captures so
    # the small-multiples + legend stay readable (rare one-offs add noise)
    keep_sp <- ds %>% dplyr::group_by(.data$scientificName) %>%
      dplyr::summarise(tot = sum(.data$count), .groups = "drop") %>%
      dplyr::filter(.data$tot >= 5) %>% dplyr::pull(.data$scientificName)
    if (length(keep_sp) == 0) keep_sp <- unique(ds$scientificName)
    ds <- ds[ds$scientificName %in% keep_sp, ]
    pal <- rv$pal %||% make_species_pal(d)
    allsp <- sort(unique(ds$scientificName))
    plots <- sort(unique(ds$plotID))

    # facet-like layout, one mini time-series per plot. The legend is built ONCE
    # from invisible "legend-only" traces covering EVERY species, so it's always
    # complete and stable regardless of which species each plot happens to hold.
    mk <- function(pl, first) {
      dd <- ds[ds$plotID == pl, ]
      p <- plot_ly()
      if (first) for (s in allsp)
        p <- p %>% add_trace(x = NA, y = NA, type = "scatter", mode = "lines",
          name = s, legendgroup = s, line = list(color = pal[[s]]),
          showlegend = TRUE, hoverinfo = "skip")
      for (s in unique(dd$scientificName)) {
        sd <- dd[dd$scientificName == s, ]
        p <- p %>% add_trace(data = sd, x = ~date, y = ~count, type = "scatter",
          mode = "lines+markers", name = s, legendgroup = s, showlegend = FALSE,
          marker = list(size = 5, color = pal[[s]]), line = list(width = 1.5, color = pal[[s]]),
          hovertemplate = paste0(pl, "<br>%{x|%b %Y}: %{y}<extra></extra>"))
      }
      p %>% plotly::layout(
        annotations = list(list(text = pl, x = 0.02, y = 1.0, xref = "x domain",
          yref = "y domain", xanchor = "left", yanchor = "bottom", showarrow = FALSE,
          font = list(color = "#16386e", size = 11), bgcolor = "rgba(255,255,255,0.78)",
          borderpad = 2)),
        xaxis = list(gridcolor = "rgba(31,42,48,0.06)"),
        yaxis = list(gridcolor = "rgba(31,42,48,0.06)"))
    }
    sub <- lapply(seq_along(plots), function(i) mk(plots[i], i == 1))
    plotly::subplot(sub, nrows = ceiling(length(plots) / 2), shareX = TRUE, shareY = FALSE,
                    titleX = FALSE, margin = 0.05) %>%
      plotly::layout(paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
                     font = list(color = "#344049", family = "Rubik"), margin = list(t = 44),
                     legend = list(font = list(size = 10), bgcolor = "rgba(0,0,0,0)")) %>%
      ctx_anno() %>%
      plotly::config(displayModeBar = FALSE)
  })

  # ---- breeding phenology -------------------------------------------------
  output$phenoPlot <- renderPlotly({
    d <- rv$data; req(d)
    ad <- flag_repro(dplyr::filter(d, !is.na(.data$tagID), .data$lifeStage == "adult", !is.na(.data$date)))
    if (nrow(ad) == 0) return(note_plot("No adult reproductive data", "\U0001F423"))
    ad$mon <- as.integer(format(ad$date, "%m"))
    by_m <- ad %>% dplyr::group_by(.data$mon) %>% dplyr::summarise(
      males = sum(.data$sex == "M", na.rm = TRUE),
      females = sum(.data$sex == "F", na.rm = TRUE),
      breeding_m = sum(.data$repro == "breeding male", na.rm = TRUE),
      repro_f = sum(.data$repro %in% c("pregnant female", "lactating/receptive female"), na.rm = TRUE),
      .groups = "drop")
    by_m$pm <- ifelse(by_m$males > 0, round(100 * by_m$breeding_m / by_m$males), NA)
    by_m$pf <- ifelse(by_m$females > 0, round(100 * by_m$repro_f / by_m$females), NA)
    mlab <- month.abb[by_m$mon]
    p <- plot_ly(x = mlab) %>%
      add_trace(y = by_m$pm, type = "scatter", mode = "lines+markers", name = "breeding males",
        line = list(color = "#2f7fb5", width = 3), marker = list(size = 8, color = "#2f7fb5"),
        hovertemplate = "%{x}<br>%{y}% of adult males scrotal<extra></extra>") %>%
      add_trace(y = by_m$pf, type = "scatter", mode = "lines+markers", name = "reproductive females",
        line = list(color = "#c2255c", width = 3), marker = list(size = 8, color = "#c2255c"),
        hovertemplate = "%{x}<br>%{y}% of adult females pregnant/lactating<extra></extra>")
    plotly_theme(p) %>% plotly::layout(
      xaxis = list(title = "", categoryorder = "array", categoryarray = month.abb),
      yaxis = list(title = "% reproductively active", range = c(0, 100)),
      hovermode = "x unified", margin = list(t = 44)) %>% ctx_anno()
  })

  # ---- body-size profile (violin per species, "Position DNA") ------------
  output$sizeViolin <- renderPlotly({
    d <- rv$data; req(d)
    w <- dplyr::filter(d, !is.na(.data$tagID), !is.na(.data$weight), .data$weight > 0,
                       !is.na(.data$scientificName))
    keep <- w %>% dplyr::count(.data$scientificName) %>% dplyr::filter(.data$n >= 8) %>%
      dplyr::pull(.data$scientificName)
    w <- w[w$scientificName %in% keep, ]
    if (nrow(w) == 0) return(note_plot("Not enough weighed animals<br>for a size profile", "⚖️"))
    ord <- w %>% dplyr::group_by(.data$scientificName) %>%
      dplyr::summarise(m = stats::median(.data$weight), .groups = "drop") %>%
      dplyr::arrange(.data$m) %>% dplyr::pull(.data$scientificName)
    pal <- rv$pal %||% make_species_pal(d)

    p <- plot_ly()
    for (s in ord) {
      sub <- w[w$scientificName == s, ]
      col <- pal[[s]] %||% "#16386e"
      p <- p %>% plotly::add_trace(
        y = sub$weight, x = rep(s, nrow(sub)), type = "violin", name = s,
        scalemode = "width", spanmode = "hard", points = FALSE,
        line = list(color = col), fillcolor = paste0(col, "44"),
        meanline = list(visible = TRUE, color = col),
        hovertemplate = paste0("<b>", s, "</b><br>%{y} g<extra></extra>"))
    }
    # mark the selected individual on its species' violin
    tag <- rv$tag
    if (!is.null(tag)) {
      ir <- dplyr::filter(w, .data$tagID == tag)
      if (nrow(ir) > 0) {
        sp <- mode_chr(ir$scientificName)
        if (sp %in% ord) p <- p %>% plotly::add_trace(
          x = sp, y = round(mean(ir$weight), 1), type = "scatter", mode = "markers",
          marker = list(symbol = "diamond", size = 15, color = "#c9a300",
                        line = list(color = "#ffffff", width = 1.5)),
          name = "this animal", hovertemplate = paste0("this animal<br>%{y} g<extra></extra>"),
          showlegend = FALSE)
      }
    }
    plotly_theme(p, legend = FALSE) %>% plotly::layout(
      showlegend = FALSE,
      xaxis = list(title = "", categoryorder = "array", categoryarray = ord, tickangle = -35),
      yaxis = list(title = "Weight (g)", type = "log"),
      margin = list(b = 120, t = 44)) %>% ctx_anno()
  })

  # ---- MNKA + catch-per-effort -------------------------------------------
  output$mnkaPlot <- renderPlotly({
    d <- rv$data; req(d)
    mn <- mnka_series(d)
    if (is.null(mn) || nrow(mn) == 0) return(note_plot("Not enough data for MNKA", "\U0001F465"))
    pal <- rv$pal
    plots <- sort(unique(mn$plotID))
    plot_cols <- colorRampPalette(brewer.pal(8, "Set2"))(max(length(plots), 3))
    p <- plot_ly()
    for (i in seq_along(plots)) {
      pl <- plots[i]; dd <- mn[mn$plotID == pl, ]
      p <- p %>% add_trace(data = dd, x = ~date, y = ~mnka, type = "scatter", mode = "lines+markers",
        name = pl, legendgroup = pl, line = list(color = plot_cols[i], width = 2),
        marker = list(size = 5, color = plot_cols[i]),
        hovertemplate = paste0(pl, "<br>%{x|%b %Y}<br>MNKA: %{y}<extra></extra>"))
    }
    # site-total CPUE companion line on a secondary axis
    site <- mn %>% dplyr::group_by(.data$date) %>%
      dplyr::summarise(cap = sum(.data$captures), tn = sum(.data$trap_nights), .groups = "drop") %>%
      dplyr::mutate(cpue = round(100 * .data$cap / .data$tn, 1))
    p <- p %>% add_trace(data = site, x = ~date, y = ~cpue, yaxis = "y2", type = "scatter",
      mode = "lines", name = "site CPUE", line = list(color = "rgba(31,42,48,0.55)", width = 2, dash = "dot"),
      hovertemplate = "%{x|%b %Y}<br>%{y} captures / 100 trap-nights<extra></extra>")
    plotly_theme(p) %>% plotly::layout(
      yaxis  = list(title = "MNKA (individuals known alive)", color = "#16386e"),
      yaxis2 = list(title = "captures / 100 TN", color = "#7a8896", overlaying = "y", side = "right",
                    gridcolor = "rgba(0,0,0,0)"),
      xaxis  = list(title = ""), hovermode = "closest", margin = list(t = 44)) %>% ctx_anno()
  })

  # ---- species accumulation ----------------------------------------------
  output$accumPlot <- renderPlotly({
    d <- rv$data; req(d)
    sa <- species_accum(d)
    if (is.null(sa)) return(note_plot("Not enough data for accumulation", "\U0001F4C8"))
    cv <- sa$curve
    p <- plot_ly() %>%
      add_trace(x = cv$bouts, y = cv$hi, type = "scatter", mode = "lines",
        line = list(width = 0), showlegend = FALSE, hoverinfo = "skip") %>%
      add_trace(x = cv$bouts, y = cv$lo, type = "scatter", mode = "lines", fill = "tonexty",
        fillcolor = "rgba(22,56,110,0.14)", line = list(width = 0),
        name = "±1 SD", hoverinfo = "skip") %>%
      add_trace(x = cv$bouts, y = cv$richness, type = "scatter", mode = "lines+markers",
        name = "species found", line = list(color = "#16386e", width = 3),
        marker = list(size = 6, color = "#16386e"),
        hovertemplate = "after %{x} bouts<br>%{y:.1f} species<extra></extra>") %>%
      add_trace(x = range(cv$bouts), y = rep(sa$chao1, 2), type = "scatter", mode = "lines",
        name = sprintf("Chao1 ≈ %s", sa$chao1),
        line = list(color = "#AB0520", width = 1.5, dash = "dash"), hoverinfo = "skip")
    plotly_theme(p) %>% plotly::layout(
      xaxis = list(title = "trapping bouts (months)"),
      yaxis = list(title = "cumulative species"), margin = list(t = 44),
      annotations = list(list(text = sprintf("observed %d · estimated ≈ %s species", sa$sobs, sa$chao1),
        x = 0.98, y = 0.05, xref = "paper", yref = "paper", xanchor = "right", showarrow = FALSE,
        font = list(color = "#6b7a85", size = 12)))) %>% ctx_anno()
  })

  # ---- detection-corrected abundance (closed-capture per bout) ------------
  # Memoize per loaded dataset so detectHead/Plot/Note share one computation.
  detect_cc <- reactive({
    d <- rv$data; req(d)
    bouts <- bout_closed_capture(d)
    closed_capture_series(d, bouts)
  })

  output$detectHead <- renderUI({
    cc <- detect_cc()
    if (is.null(cc) || is.null(cc$series) || nrow(cc$series) == 0) return(NULL)
    pct <- function(x) if (is.na(x)) "—" else paste0(round(100 * x), "%")
    chip <- function(v, lab, col) div(class = "detect-chip", style = sprintf("--dc:%s", col),
      div(class = "detect-v", v), div(class = "detect-l", lab))
    div(class = "detect-head",
      chip(pct(cc$mean_p),      "per-night detection (p̂)", "#0C234B"),
      chip(pct(cc$mean_detect), "of population caught / bout", "#2f7fb5"),
      chip(cc$n_estimable,      sprintf("estimable bouts (of %d)", cc$n_bouts), "#1a7f37"))
  })

  output$detectPlot <- renderPlotly({
    cc <- detect_cc()
    if (is.null(cc) || is.null(cc$series) || nrow(cc$series) == 0)
      return(note_plot(paste0("No multi-night recapture data to estimate detection here.<br>",
                              "<span style='font-size:13px'>This site's grids are single-night, or had too few within-bout recaptures.<br>",
                              "MNKA & CPUE above are the right index for these.</span>"), "\U0001F50E"))
    s <- cc$series
    # cap any infinite upper bound for plotting (shouldn't occur post-roll-up, but be safe)
    s$hi[!is.finite(s$hi)] <- s$N[!is.finite(s$hi)] * 2
    p <- plot_ly() %>%
      add_trace(data = s, x = ~date, y = ~hi, type = "scatter", mode = "lines",
        line = list(width = 0), showlegend = FALSE, hoverinfo = "skip") %>%
      add_trace(data = s, x = ~date, y = ~lo, type = "scatter", mode = "lines", fill = "tonexty",
        fillcolor = "rgba(12,35,75,0.13)", line = list(width = 0),
        name = "95% interval", hoverinfo = "skip") %>%
      add_trace(data = s, x = ~date, y = ~mnka, type = "scatter", mode = "lines+markers",
        name = "MNKA (known alive)", line = list(color = "#8a97a8", width = 2),
        marker = list(size = 5, color = "#8a97a8"),
        hovertemplate = "%{x|%b %Y}<br>MNKA %{y}<extra></extra>") %>%
      add_trace(data = s, x = ~date, y = ~N, type = "scatter", mode = "lines+markers",
        name = "estimated abundance (N̂)", line = list(color = "#0C234B", width = 3),
        marker = list(size = 7, color = "#0C234B"),
        customdata = ~round(100 * p),
        hovertemplate = "%{x|%b %Y}<br>N̂ %{y} · p̂ %{customdata}%<extra></extra>")
    plotly_theme(p) %>% plotly::layout(
      xaxis = list(title = ""), yaxis = list(title = "animals on the grid(s)", rangemode = "tozero"),
      margin = list(t = 30)) %>% ctx_anno()
  })

  output$detectNote <- renderUI({
    cc <- detect_cc()
    if (is.null(cc) || is.null(cc$series) || nrow(cc$series) == 0) return(NULL)
    s <- cc$series
    lift <- if (any(s$mnka > 0)) round(100 * (sum(s$N) / sum(s$mnka) - 1)) else NA
    div(class = "detect-note", bs_icon("info-circle"),
      HTML(sprintf(" Across estimable bouts, the corrected estimate runs about <b>%s%%</b> above the raw known-alive count — the animals the traps missed. Estimates are summed across grids per month; months with too few recaptures are omitted.",
                   ifelse(is.na(lift), "—", lift))))
  })

  # ---- printable site report card ----------------------------------------
  # A clean one-pager summarizing the loaded site; the "Report card" button
  # triggers the browser's print dialog (-> Save as PDF). Hidden on screen,
  # shown only when printing (see styles.css @media print).
  output$reportCard <- renderUI({
    d <- rv$data; req(d)
    cs <- community_stats(d, rv$lb)
    hn <- hill_numbers(d)
    cc <- tryCatch(detect_cc(), error = function(e) NULL)
    sp <- utils::head(species_summary(d), 8)
    even_word <- if (is.na(hn$even)) "—"
      else if (hn$even >= 0.6) "an even community" else if (hn$even >= 0.35) "a moderately uneven community"
      else "a community dominated by a few species"
    stat <- function(v, lab) div(class = "rc-stat", div(class = "rc-stat-v", v), div(class = "rc-stat-l", lab))
    p_txt <- if (!is.null(cc) && !is.null(cc$series) && nrow(cc$series) > 0 && !is.na(cc$mean_p))
               sprintf("%.0f%% per night (≈%.0f%% of the population caught per bout, across %d estimable bouts)",
                       100 * cc$mean_p, 100 * cc$mean_detect, cc$n_estimable)
             else "not estimable here (single-night grids / too few recaptures)"
    div(class = "report-card",
      div(class = "rc-head",
        div(class = "rc-brand", "\U0001F43E NEON Small Mammal Report Card"),
        div(class = "rc-site", rv$label),
        div(class = "rc-range", fmt_range(cs$date_min, cs$date_max),
            if (isTRUE(rv$is_demo)) " · demo dataset")),
      div(class = "rc-stats",
        stat(format(cs$total_captures, big.mark = ","), "captures"),
        stat(format(cs$individuals, big.mark = ","), "individuals"),
        stat(cs$species, "species"),
        stat(paste0(cs$recap_rate, "%"), "recapture rate"),
        stat(format(cs$trap_nights, big.mark = ","), "trap-nights"),
        stat(cs$legendary, "10+ caught")),
      div(class = "rc-section",
        tags$h4("Diversity"),
        tags$p(sprintf("Species richness %d · effective common species (Hill q1) %.1f · effective dominant (q2) %.1f · evenness %s — %s.",
          hn$q0, hn$q1, hn$q2, ifelse(is.na(hn$even), "—", format(hn$even, nsmall = 2)), even_word))),
      div(class = "rc-section",
        tags$h4("Detection-corrected abundance"),
        tags$p(sprintf("Estimated per-night detection probability: %s.", p_txt))),
      div(class = "rc-section",
        tags$h4("Most-caught species"),
        tags$table(class = "rc-table",
          tags$thead(tags$tr(tags$th("Species"), tags$th("Individuals"), tags$th("Captures"))),
          tags$tbody(lapply(seq_len(nrow(sp)), function(i) tags$tr(
            tags$td(tagList(sp$emoji[i], " ", tags$em(sp$scientificName[i]),
                            if (!is.na(sp$nickname[i])) tags$span(class = "rc-nick", paste0(" (", sp$nickname[i], ")")))),
            tags$td(format(sp$individuals[i], big.mark = ",")),
            tags$td(format(sp$captures[i], big.mark = ","))))))),
      div(class = "rc-foot",
        sprintf("Data: NEON Small Mammal Box Trapping (DP1.10072.001). Generated by the NEON Small Mammal Tracker — Desert Data Labs. An unofficial educational summary; not affiliated with NEON, Battelle, or the NSF.")))
  })
  # the report card lives in a display:none wrapper (shown only when printing);
  # render it anyway so it's ready in the DOM the instant the user prints.
  outputOptions(output, "reportCard", suspendWhenHidden = FALSE)

  # ---- about --------------------------------------------------------------
  output$aboutPanel <- renderUI({
    div(class = "about-wrap",
      div(class = "about-card",
        h4("\U0001F43E What this is"),
        p("An (unofficial) explorer for NEON's small-mammal box-trapping data product ",
          tags$code("DP1.10072.001"), ". Pick a site and date window and the app pulls every published capture, then reconstructs each individual's ", tags$b("capture career"), " from its ear-tag ID.")),
      div(class = "about-card",
        h4(bs_icon("arrow-repeat"), " How fresh is the data?"),
        p("Each site ships as a pre-built, compressed bundle, so it loads instantly. An automated job re-pulls the latest published NEON records and redeploys the app ", tags$b("late on the first Saturday night of each month"), " (~11 pm Arizona time) — an off-peak window chosen so the brief redeploy never interrupts anyone mid-session."),
        p("Want the very newest records right now? Tick ", tags$b("Include provisional"), " in the sidebar for a live fetch of NEON's latest (still-unpublished) data.")),
      div(class = "about-card",
        h4(bs_icon("calculator"), " The Chonk Index — honest version"),
        p("It would be tempting to dress this up as a Scaled Mass Index (Peig & Green 2009), but in these desert rodents hind-foot length barely scales with mass (r ≈ 0.15 for kangaroo & pocket mice) and NEON almost never records total body length — so a standardized condition index would just rank measurement noise."),
        p("Instead, the Chonk score is an honest ", tags$b("adult weight percentile within species"), " — a true statement (\"heavy for its kind\"). The ", tags$b("body-size map"), " on the dossier shows the actual weight × hind-foot cloud, and draws a size–mass fit line ", tags$em("only"), " for species where the relationship is real (n ≥ 15, |r| ≥ 0.3)."),
        p(class = "caveat", bs_icon("exclamation-triangle"), " Computed on adults only; species with < 4 measured adults show ", tags$b("—"), ".")),
      div(class = "about-card",
        h4(bs_icon("clipboard-data"), " Metrics"),
        tags$ul(
          tags$li(tags$b("Career span"), " — days between first and last capture. Spans > 520 d or with a > 200 d gap are flagged ", tags$b("verify tag"), " (likely a reused ear tag = two animals)."),
          tags$li(tags$b("Roam radius"), " — mean displacement of captures from the trap-grid centroid (a grid-bounded dispersion index, not a true home-range area)."),
          tags$li(tags$b("Max move"), " — the largest distance between any two captures (MDM)."),
          tags$li(tags$b("MNKA"), " — Minimum Number Known Alive (Krebs 1966): a transparent abundance ", tags$em("index"), ", shown with captures / 100 trap-nights (CPUE)."),
          tags$li(tags$b("Species accumulation"), " — richness vs trapping effort (Gotelli & Colwell 2001) with a Chao1 estimate of total richness."),
          tags$li(tags$b("Rarity"), " — a playful tier from total captures; it tracks trappability & residency, not ecological rarity."))),
      div(class = "about-card",
        h4(bs_icon("exclamation-diamond"), " Caveats"),
        p("NEON ear-tag numbers can be reused across years (we flag the obvious cases). A trap that caught nothing means \"not detected,\" not \"absent.\" This is a data-exploration toy, not an authoritative population analysis — but the metrics are built to be defensible."),
        p("Reviewed for scientific soundness with input from a wildlife-monitoring methods audit (Peig & Green 2009; Krebs 1966; Gotelli & Colwell 2001; NEON DP1.10072.001 User Guide)."),
        p(bs_icon("envelope"), " ", tags$a(href = "mailto:tsgilbert@arizona.edu", "tsgilbert@arizona.edu"),
          " · ", tags$a(href = "https://data.neonscience.org/data-products/DP1.10072.001",
                        target = "_blank", "NEON data product"))))
  })

  # ---- help dialog (also wired in confirm.js) ----------------------------
}
