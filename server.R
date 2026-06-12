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
    updateSelectInput(session, "site", choices = sites,
                      selected = if (length(sites)) sites[[1]] else NULL)
  }, ignoreNULL = TRUE)

  output$siteBio <- renderUI({
    req(input$site)
    b <- site_bio(input$site)
    if (is.null(b)) return(NULL)
    div(class = "site-bio", bs_icon("info-circle-fill"), span(b))
  })

  shinyjs::hide("mainTabsWrap")

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

  observeEvent(input$loadBtn, {
    if (is.null(input$site) || input$site == "") {
      session$sendCustomMessage("loadDone", list()); return()
    }
    site <- input$site; s0 <- input$dateRange[1]; e0 <- input$dateRange[2]
    prov <- isTRUE(input$provisional)
    # ingest() sends "loadDone" when it finishes; we send it on the failure
    # paths below so the loading overlay is ALWAYS dismissed.

    # 1) bundled site? read from disk instantly and filter to the window.
    #    (Skip the bundle when the user wants provisional data — the bundle is
    #    published-only, so provisional must come from a live fetch.)
    if (!prov) {
      bundle <- load_site_bundle(site)
      if (!is.null(bundle)) {
        d0 <- filter_window(bundle, s0, e0)
        if (sum(!is.na(d0$tagID)) > 0) {
          return(ingest(d0, sprintf("%s · %s", site_label(site), fmt_range(s0, e0))))
        }
        # window had no captures in the bundle -> fall through to a live fetch
      }
    }

    # 2) live fetch (with a session cache so repeats are instant)
    key <- paste(site, s0, e0, prov, sep = "|")
    res <- if (!is.null(fetch_cache[[key]])) fetch_cache[[key]] else tryCatch(
      fetch_neon_mam(site, s0, e0, provisional = prov),
      error = function(e) { showNotification(paste("NEON fetch failed:", conditionMessage(e)),
                                             type = "error", duration = 8); NULL })
    if (is.null(res)) { session$sendCustomMessage("loadDone", list()); return() }
    fetch_cache[[key]] <- res
    ingest(res, sprintf("%s · %s%s", site_label(site), fmt_range(s0, e0),
                        if (prov) " · incl. provisional" else ""))
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

  # ---- splash / landing (before any site is loaded) ----------------------
  output$splash <- renderUI({
    if (!is.null(rv$data)) return(NULL)
    feat <- function(icon, title, text) div(class = "land-feat",
      div(class = "land-feat-ico", bs_icon(icon)),
      div(div(class = "land-feat-t", title), div(class = "land-feat-d", text)))
    div(class = "splash",
      div(class = "splash-icon", "\U0001F43E"),
      h3("Explore the small mammals of the NEON network"),
      p("The National Ecological Observatory Network live-traps small mammals at 47 field sites across the U.S. and Puerto Rico. This app turns those captures into maps, charts, and individual ", tags$em("life stories"), " — built for the curious and for new field techs learning their site."),
      div(class = "splash-steps",
        div(class = "step", span(class = "step-n", "1"), tagList("Pick a ", tags$b("state"), ", then a ", tags$b("site"), " in the sidebar \U2190")),
        div(class = "step", span(class = "step-n", "2"), tagList("Hit ", tags$b("Load this site"), " (real NEON data downloads live)")),
        div(class = "step", span(class = "step-n", "3"), "Explore the maps, charts & critters")
      ),
      actionButton("demoBtn2", tagList(bs_icon("stars"), " Explore the Jornada demo instantly"),
                   class = "btn-primary btn-lg", onclick = "smtLoadStart('Jornada — demo dataset')"),
      div(class = "land-feats",
        feat("map-fill", "Where they live", "Species mapped across each site's trapping grids."),
        feat("fire", "Home ranges", "Heatmaps & replays of where one animal kept turning up."),
        feat("graph-up-arrow", "Real science", "Abundance indices, body condition & breeding phenology."))
    )
  })
  observeEvent(input$demoBtn2, {
    d <- load_demo(); req(!is.null(d)); ingest(d, DEMO_META$label, is_demo = TRUE)
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
