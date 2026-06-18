# ===========================================================================
# NEON Small Mammal Tracker — server.R
# ===========================================================================

server <- function(input, output, session) {

  # is the dark theme active? Driven by the sidebar input_dark_mode("colorMode").
  # Reading it inside the shared plot helpers makes every chart that calls them
  # take a reactive dependency on the toggle, so they re-render on theme switch.
  is_dark <- function() identical(input$colorMode, "dark")

  # ---- shared plotly styling (Rubik; light or dark per the toggle) -------
  plotly_theme <- function(p, legend = TRUE) {
    dark <- is_dark()
    ink  <- if (dark) "#e8eef2" else "#1f2a30"
    grid <- if (dark) "rgba(220,230,240,0.10)" else "rgba(31,42,48,0.08)"
    zero <- if (dark) "rgba(220,230,240,0.22)" else "rgba(31,42,48,0.15)"
    lin  <- if (dark) "#3a4759" else "#d6ddd4"
    legc <- if (dark) "#c3cedd" else "#344049"
    p %>% plotly::layout(
      paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
      font = list(color = ink, family = "Rubik"),
      xaxis = list(gridcolor = grid, zerolinecolor = zero, linecolor = lin),
      yaxis = list(gridcolor = grid, zerolinecolor = zero, linecolor = lin),
      legend = list(bgcolor = "rgba(0,0,0,0)", orientation = "h", y = -0.2, font = list(color = legc)),
      margin = list(l = 50, r = 30, t = 48, b = 40),   # t roomy enough for the ctx caption
      # navy card + gold edge tooltips, on-theme across every plot
      hoverlabel = list(bgcolor = "rgba(12,35,75,0.96)", bordercolor = "#FFD200",
        font = list(color = "#ffffff", family = "Rubik", size = 13))
    ) %>%
      plotly::config(displayModeBar = FALSE, responsive = TRUE)
  }

  # shown on individual-only views when nobody is selected yet
  PICK_MSG <- "Pick an individual first.<br>Open the <b>Hall of Fame</b> and tap a row,<br>or hit \U201CSurprise me\U201D in the sidebar."

  # Append the current site + year-range as a small top-right caption on a plot
  # (uses add_annotations so it never clobbers a plot's own annotations).
  ctx_anno <- function(p) {
    if (is.null(rv$ctx)) return(p)
    # y just above the plot (1.03) so it clears the panel without needing a huge
    # top margin; plotly_theme's t=48 gives every plot enough headroom.
    plotly::add_annotations(p, text = rv$ctx, x = 1, y = 1.03, xref = "paper", yref = "paper",
      xanchor = "right", yanchor = "bottom", showarrow = FALSE,
      font = list(color = if (is_dark()) "#9fb0c4" else "#6b7a89", size = 11, family = "Rubik"))
  }

  # A centered-message placeholder for plots that have nothing to show.
  note_plot <- function(msg, icon = "\U0001F50D") {
    plotly::plot_ly(type = "scatter", mode = "markers") %>%
      plotly::layout(
        paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
        xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
        annotations = list(list(text = paste0(icon, "<br>", msg), showarrow = FALSE,
          font = list(color = if (is_dark()) "#9fb0c4" else "#6b7a85", size = 15), align = "center"))) %>%
      plotly::config(displayModeBar = FALSE)
  }

  # ---- core reactive state ------------------------------------------------
  rv <- reactiveValues(
    data = NULL,       # cleaned mam table
    lb = NULL,         # leaderboard tibble
    lb_view = NULL,    # currently-displayed leaderboard slice (row order = DT rows)
    pal = NULL,        # stable species -> color map (shared across all views)
    label = NULL,      # human label for the active site/window
    tag = NULL,        # selected full tagID
    env = NULL         # monthly co-located environmental overlay for the site
  )

  # Currently-selected overlay layer ("none" or an ENV_LAYERS key) + its lag.
  # Centralised so every plot reads the same selection.
  env_sel <- reactive({
    layer <- input$envLayer %||% "none"
    if (is.null(rv$env) || layer == "none" || !layer %in% names(ENV_LAYERS)) return(NULL)
    list(layer = layer, lag = as.integer(input$envLag %||% 0),
         env = rv$env, demo = identical(attr(rv$env, "source"), "demo"))
  })

  # show the lag slider once a layer is chosen, AND default it to that layer's
  # MOST-correlated lag (from the deseasonalized scan) so picking a layer shows
  # its best match, not lag 0; the slider still lets you switch to any other lag.
  observeEvent(input$envLayer, {
    on <- !is.null(input$envLayer) && input$envLayer != "none"
    shinyjs::toggle("envLagWrap", condition = on)
    if (on && !is.null(rv$data) && !is.null(rv$env)) {
      sc <- tryCatch(env_corr_scan(rv$data, rv$env, input$envLayer), error = function(e) NULL)
      best <- if (!is.null(sc) && !is.na(sc$lag)) as.integer(sc$lag) else 0L
      updateSliderInput(session, "envLag", value = best)
      # The lag slider's wrapper was just un-hidden; ionRangeSlider can compute a
      # 0-width track when updated inside a hidden box and then not repaint the
      # handle. A resize tick once it's visible makes it recompute width and
      # redraw the handle at the value we just set (`best`).
      shinyjs::runjs("setTimeout(function(){window.dispatchEvent(new Event('resize'));},150);")
    }
  }, ignoreNULL = FALSE)

  # provenance badge under the picker: real NEON vs the illustrative demo series
  output$envSourceNote <- renderUI({
    if (is.null(rv$env)) return(NULL)
    if (identical(attr(rv$env, "source"), "demo"))
      div(class = "env-source env-demo", bs_icon("info-circle-fill"),
          tags$span(HTML(" <b>Demo overlay</b> — an illustrative monthly series, <b>not</b> NEON data. Run <code>scripts/refresh_env_data.R</code> to bundle the real product.")))
    else
      div(class = "env-source env-real", bs_icon("patch-check-fill"),
          tags$span(" Live from co-located NEON sensors at this site."))
  })

  # Quantify the population<->environment link: best lag + correlation between
  # this site's monthly catch-per-effort and the selected (lagged) driver.
  output$envCorrNote <- renderUI({
    es <- env_sel(); d <- rv$data
    if (is.null(d)) return(NULL)
    # On initial load no layer is picked (env_sel() is NULL), but the ranking
    # card + auto-overlay below already default to the BEST-correlated driver.
    # Mirror that here so the narrative renders immediately and describes the
    # same driver — instead of staying blank until the user taps a bar.
    if (is.null(es)) {
      if (is.null(rv$env)) return(NULL)
      ca <- env_corr_all(d, rv$env)
      if (is.null(ca) || !nrow(ca)) return(NULL)
      w  <- ca[1, ]
      es <- list(layer = w$layer, lag = as.integer(w$lag), env = rv$env,
                 demo = identical(attr(rv$env, "source"), "demo"))
    }
    sc <- env_corr_scan(d, es$env, es$layer)
    if (is.null(sc)) return(NULL)
    # The redesign reads as one designed "answer" to the card below it:
    #   eyebrow · hero sentence + hero r-value · supporting metadata.
    # Strength drives the left rail + the bold verdict word; SIGN drives the
    # r-value color + arrow glyph + the "more/fewer" word — kept on separate
    # visual channels so a strong-but-inverse link never reads as "weak".
    strength <- abs(sc$r)
    pos    <- sc$r >= 0
    dir    <- if (pos) "more" else "fewer"
    rail   <- if (strength >= 0.6) "rail-strong" else if (strength >= 0.35) "rail-mod" else "rail-weak"
    slabel <- if (strength >= 0.6) "Strong" else if (strength >= 0.35) "Moderate"
              else if (strength >= 0.2) "Weak" else "Negligible"
    glyph  <- if (pos) "arrow-up-right" else "arrow-down-right"
    div(class = paste("ec", rail),
      # tint the driver-name underline to that driver's IDENTITY colour (not its
      # sign/strength) so the banner echoes the warm/blue/green family below.
      style = sprintf("--ec-driver-hue:%s;", ENV_LAYERS[[es$layer]]$color %||% "#8a97a8"),
      div(class = "ec-eyebrow",
        bs_icon("graph-up-arrow"), tags$span("environmental tracking"),
        if (es$demo) tags$span(class = "ec-demo", "demo overlay") else NULL),
      div(class = "ec-hero",
        div(class = "ec-hero-text",
          tags$span(class = "ec-strength", slabel), " link with ",
          tags$span(class = "ec-driver", tolower(sc$label))),
        div(class = paste("ec-rvalue", if (pos) "ec-sgn-pos" else "ec-sgn-neg"),
          title = "correlation coefficient, -1 to +1 — tap the (i) above for what it means",
          bs_icon(glyph), HTML(sprintf("r&nbsp;%+.2f", sc$r)))),
      div(class = "ec-foot",
        tags$span(class = "ec-meta", bs_icon("clock-history"),
          if (sc$lag == 0) "same-month signal"
          else HTML(sprintf("<b>%d-mo</b> lead", sc$lag))),
        tags$span(class = "ec-meta-dot"),
        tags$span(class = "ec-meta", bs_icon("calendar3"),
          HTML(sprintf("<b>%d</b> months matched", sc$n))),
        tags$span(class = paste("ec-meta ec-dir", if (pos) "ec-sgn-pos" else "ec-sgn-neg"),
          HTML(sprintf("higher \U2192 <b>%s</b> animals", dir)))))
  })

  # Response scatter: monthly catch-per-effort vs the (lagged) driver, with fit.
  output$envScatter <- renderPlotly({
    es <- env_sel(); d <- rv$data
    if (is.null(es) || is.null(d)) return(note_plot("Pick an environmental overlay to compare", "\U0001F3AF"))
    meta <- ENV_LAYERS[[es$layer]]
    sc   <- tryCatch(env_corr_scan(d, es$env, es$layer), error = function(e) NULL)
    pts <- env_response_points(d, es$env, es$layer, es$lag)
    if (is.null(pts) || nrow(pts) < 3)
      return(note_plot("Not enough month-matched data for this overlay", "\U0001F3AF"))
    p <- plot_ly(data = pts, x = ~value, y = ~cpue, type = "scatter", mode = "markers",
      color = ~year, colors = "YlGnBu",
      marker = list(size = 10, line = list(color = "#fff", width = 1)),
      text = ~format(date, "%b %Y"),
      hovertemplate = paste0("%{text}<br>", meta$label, ": %{x:.", (meta$dig %||% 0), "f} ", meta$unit,
                             "<br>CPUE: %{y:.1f}/100TN<extra></extra>")) %>%
      plotly::colorbar(title = "year")
    if (nrow(pts) >= 3 && stats::sd(pts$value) > 0) {
      fit <- stats::lm(cpue ~ value, data = pts)
      xs <- range(pts$value, na.rm = TRUE)
      yh <- stats::predict(fit, newdata = data.frame(value = xs))
      fitcol <- if (!is.null(sc)) ec_corr_color(es$layer, sc$r, is_dark()) else meta$color
      p <- p %>% plotly::add_trace(x = xs, y = yh, type = "scatter", mode = "lines",
        inherit = FALSE, showlegend = FALSE, hoverinfo = "skip",
        line = list(color = fitcol, width = 2, dash = "dash"))
    }
    p %>% plotly_theme(legend = FALSE) %>% plotly::layout(
      xaxis = list(title = sprintf("%s (%s)%s", meta$label, meta$unit,
                   if (es$lag) sprintf(" · lag %d mo", as.integer(es$lag)) else "")),
      yaxis = list(title = "catch per 100 trap-nights", rangemode = "tozero"))
  })

  # is any environmental overlay available for this site? (drives the driver card)
  output$hasEnv <- reactive(!is.null(rv$env) && nrow(rv$env) > 0)
  outputOptions(output, "hasEnv", suspendWhenHidden = FALSE)

  # multi-driver comparison: best-lag |correlation| for every available driver
  output$envDriverRank <- renderPlotly({
    d <- rv$data; env <- rv$env
    if (is.null(d) || is.null(env)) return(note_plot("No environmental data for this site", "\U0001F326"))
    ca <- env_corr_all(d, env)
    if (is.null(ca) || !nrow(ca)) return(note_plot("Not enough overlap to compare drivers", "\U0001F326"))
    ca <- ca[order(abs(ca$r)), ]   # plotly horizontal bars draw bottom-up
    ca$lab <- ifelse(ca$lag == 0, "same mo", sprintf("lag %d mo", ca$lag))
    ca$dir <- ifelse(ca$r < 0, "inverse", "positive")
    # Driver-semantic fill: hue = which driver + sign of r (warm/cool, green/brown,
    # faded for weak links). Safe here ONLY because the bar's SIDE of x=0 (+ the
    # "left of 0 = inverse" caption) is the authoritative direction cue — colour
    # never carries the direction call alone, so it survives colour-blindness.
    ca$ccol <- mapply(ec_corr_color, ca$layer, ca$r, MoreArgs = list(dark = is_dark()))
    demo <- identical(attr(env, "source"), "demo")
    n_drv <- nrow(ca)
    # Bars are clickable (source = "driverRank", customdata = the layer key): a
    # click overlays that driver on the abundance plot below — the ranking IS the
    # driver switcher. Sorted by strength |r| regardless of sign; the sign/dir
    # tells you whether more driver means more or fewer animals.
    plot_ly(ca, x = ~r, y = ~factor(label, levels = label), type = "bar",
      orientation = "h", source = "driverRank", customdata = ~layer,
      marker = list(color = ~ccol),
      text = ~sprintf("r %+.2f · %s · n=%d", r, lab, n), textposition = "auto",
      hovertemplate = ~paste0("<b>", label, "</b><br>r %{x:+.2f} (", dir, ") at ", lab,
                              " · n=", n, "<br><i>tap to overlay below</i><extra></extra>")) %>%
      plotly::event_register("plotly_click") %>%
      plotly_theme(legend = FALSE) %>%
      plotly::layout(
        title = list(text = if (demo) "Demo overlays — illustrative" else "",
                     font = list(size = 11, color = "#9a7a00"), x = 0.02),
        xaxis = list(title = "deseasonalized correlation with catch-per-effort (r) — left of 0 = inverse",
                     range = c(-1, 1), zeroline = TRUE, zerolinecolor = "rgba(31,42,48,0.35)"),
        yaxis = list(title = ""),
        margin = list(b = 84),
        # cohesion + honesty: the bars ARE the switcher, sorted by strength; and
        # they're collinear stages of one seasonal cascade, not independent evidence
        annotations = list(list(
          text = sprintf("tap a bar to overlay that driver below · sorted by strength · best of %d driver%s × ≤13 lags<br>left of 0 = inverse (more driver → fewer animals) · bars aren't independent evidence",
                         n_drv, if (n_drv == 1) "" else "s"),
          x = 0, y = -0.36, xref = "paper", yref = "paper", xanchor = "left", yanchor = "top",
          align = "left", showarrow = FALSE, font = list(size = 10, color = "#8a97a8"))))
  })

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
    if (is_demo) rv$loaded_key <- NULL   # demo isn't a site+window; don't let the guard match it
    d <- clean_mam(data.raw)
    if (is.null(d) || sum(d$is_capture) == 0) {
      session$sendCustomMessage("loadDone", list())   # hide the loading overlay
      rv$loaded_key <- NULL                            # let a retry re-run this site
      showNotification("No small-mammal captures found for that site & window.",
                       type = "warning", duration = 6)
      return(invisible(NULL))
    }
    rv$data  <- d
    rv$lb    <- build_leaderboard(d)
    rv$pal   <- make_species_pal(d)
    rv$label <- label
    rv$tag   <- NULL
    # co-located environmental overlay for THIS site (precip/temp/soil/phenology)
    rv$env   <- load_site_env(mode_chr(d$siteID))
    # compact context shown on each plot, e.g. "JORN · 2022–2024"
    y1 <- format(safe_date_min(d$date), "%Y"); y2 <- format(safe_date_max(d$date), "%Y")
    rv$ctx <- paste0(mode_chr(d$siteID), " · ", if (is.na(y1)) "" else if (y1 == y2) y1 else paste0(y1, "–", y2))

    # reveal UI
    shinyjs::show("mainTabsWrap")
    shinyjs::show("indivPickerWrap")
    shinyjs::hide("splash")

    # environmental-overlay picker: only offer it when this site has env data,
    # and only the layers that actually have values for it.
    env_ch <- env_layer_choices(rv$env)
    if (length(env_ch) > 1) {
      updateSelectInput(session, "envLayer", choices = env_ch, selected = "none")
      shinyjs::show("envPickerWrap")
    } else {
      shinyjs::hide("envPickerWrap")
    }

    # individual picker choices
    lb <- rv$lb
    ch <- setNames(lb$tagID, sprintf("%s  %s · %s · %d caps",
                                     lb$emoji, lb$short, lb$scientificName, lb$captures))
    updateSelectizeInput(session, "indiv", choices = c("Pick a tagID…" = "", ch), server = TRUE)
    updateSelectizeInput(session, "indivHR", choices = c("Pick an individual…" = "", ch), server = TRUE)
    nav_select("tabs", "overview")
    session$sendCustomMessage("countUp", list())
    session$sendCustomMessage("loadDone", list())   # hide the loading overlay
    # positive confirmation it worked (the demo path shows its own toast)
    if (!is_demo)
      showNotification(tagList(bs_icon("check-circle-fill"),
        HTML(paste0(" Loaded <b>", htmltools::htmlEscape(label), "</b>"))),
        type = "message", duration = 4)
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

    # double-tap / re-select guard: if this exact site+window+prov is already
    # the loaded dataset, don't re-run the multi-second ingest — just dismiss
    # the overlay. (Cleared on demo loads and failed loads so retries still run.)
    key <- paste(site, as.character(s0), as.character(e0), prov, sep = "|")
    if (!is.null(rv$data) && identical(rv$loaded_key, key)) {
      session$sendCustomMessage("loadDone", list()); return(invisible())
    }
    rv$loaded_key <- key

    # 1) bundled site? read from disk instantly and filter to the window.
    #    (Skip the bundle when the user wants provisional data — the bundle is
    #    published-only, so provisional must come from a live fetch.)
    if (!prov) {
      bundle <- load_site_bundle(site)
      if (!is.null(bundle)) {
        d0 <- filter_window(bundle, s0, e0)
        if (sum(!is.na(d0$tagID)) > 0)
          return(ingest(d0, sprintf("%s · %s", site_label(site), fmt_range(s0, e0))))
        # Window had no captures, but the site IS bundled — its records just fall
        # outside this window (e.g. GUAN/LAJA predate the default range). Show the
        # full bundled record INSTANTLY rather than dropping to a slow live fetch.
        if (sum(!is.na(bundle$tagID)) > 0) {
          showNotification(sprintf(
            "No captures at %s in %s–%s — showing its full bundled record instead.",
            site_label(site), format(as.Date(s0), "%Y"), format(as.Date(e0), "%Y")),
            type = "message", duration = 6)
          return(ingest(bundle, sprintf("%s · full record", site_label(site))))
        }
        # bundle truly empty -> fall through to live (if enabled)
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
  # ---- the site-choice popup + "About this site" card --------------------
  # Tapping a dot no longer auto-loads. It opens a small popup anchored on the
  # dot offering a CLEAR choice: "Explore this site" (loads the record) or
  # "About this site" (an instant info card). Both built from SITE_INDEX by the
  # clicked code, so they're identical in by-site and by-species mode.
  site_popup_html <- function(row) {
    code <- row$site[1]
    where <- paste(stats::na.omit(c(
      as.character(row$state[1]),
      if (!is.na(row$domain[1])) paste("NEON", row$domain[1]) else NA,
      as.character(row$bio[1]))), collapse = " · ")
    sp_line <- if (!is.na(row$top_species[1]))
      sprintf("<div class='pm-pop-sp'>Most caught: <i>%s</i>%s</div>", row$top_species[1],
              if (!is.na(row$nickname[1])) sprintf(" (%s)", row$nickname[1]) else "") else ""
    yrs <- if (!is.na(row$year_min[1]) && !is.na(row$year_max[1]))
      sprintf("<div class='sp-years'>Sampled %d&ndash;%d</div>", row$year_min[1], row$year_max[1]) else ""
    htmltools::HTML(sprintf(
      "<div class='pm-pop site-pop'>
         <div class='pm-pop-t'>%s %s <span class='sp-code'>(%s)</span></div>
         <div class='pm-pop-s'>%s</div>
         <div class='pm-pop-n'><b>%s</b> captures &middot; <b>%s</b> individuals &middot; <b>%s</b> species</div>
         %s%s
         <div class='sp-actions'>
           <button type='button' class='sp-btn sp-go' onclick=\"smtLoadStart('%s \\u2014 loading\\u2026');Shiny.setInputValue('siteExplore','%s',{priority:'event'});\">Explore this site &rarr;</button>
           <button type='button' class='sp-btn sp-info' onclick=\"Shiny.setInputValue('siteInfo','%s',{priority:'event'});\">About this site</button>
         </div>
       </div>",
      row$emoji[1], row$name[1], code, where,
      format(row$captures[1], big.mark = ","), format(row$individuals[1], big.mark = ","),
      row$species[1], sp_line, yrs, row$name[1], code, code))
  }

  site_info_modal <- function(code) {
    row <- if (!is.null(SITE_INDEX)) SITE_INDEX[SITE_INDEX$site == code, ] else NULL
    if (is.null(row) || !nrow(row))
      return(modalDialog(title = "Site info", easyClose = TRUE, footer = modalButton("Close"),
                         p("No details are available for this site.")))
    dash  <- function(x) if (length(x) == 0 || is.na(x) || !nzchar(as.character(x))) "—" else as.character(x)
    coords <- if (!is.na(row$lat[1]) && !is.na(row$lng[1]))
      sprintf("%.3f, %.3f", row$lat[1], row$lng[1]) else "—"
    yrs <- if (!is.na(row$year_min[1]) && !is.na(row$year_max[1]))
      sprintf("%d–%d", row$year_min[1], row$year_max[1]) else "—"
    star <- if (!is.na(row$top_species[1]))
      HTML(sprintf("<i>%s</i>%s%s", row$top_species[1],
        if (!is.na(row$nickname[1])) sprintf(" (%s)", row$nickname[1]) else "",
        if (!is.na(row$top_caps[1])) sprintf(" — %s captures", format(row$top_caps[1], big.mark = ",")) else ""))
      else "—"
    stat <- function(v, lab) div(class = "si-stat",
      div(class = "si-stat-n", if (is.na(v)) "—" else format(v, big.mark = ",")),
      div(class = "si-stat-l", lab))
    modalDialog(
      title = HTML(sprintf("%s %s <span class='si-code'>(%s)</span>", row$emoji[1], row$name[1], code)),
      easyClose = TRUE, size = "m",
      footer = tagList(
        modalButton("Close"),
        tags$button(type = "button", class = "btn btn-primary",
          onclick = sprintf("smtLoadStart('%s \\u2014 loading\\u2026');Shiny.setInputValue('siteExplore','%s',{priority:'event'});",
                            gsub("'", "\\\\'", row$name[1]), code),
          HTML("Explore this site&rsquo;s data &rarr;"))),
      div(class = "site-info",
        div(class = "si-sec",
          div(class = "si-h", "Where"),
          div(class = "si-row", dash(row$state[1]), " · NEON ", dash(row$domain[1])),
          if (!is.na(row$bio[1])) div(class = "si-row si-bio", row$bio[1]),
          div(class = "si-coords", bs_icon("geo-alt"), " ", coords)),
        div(class = "si-sec",
          div(class = "si-h", "When"),
          div(class = "si-row", "Sampled ", yrs)),
        div(class = "si-sec",
          div(class = "si-h", "What’s been caught"),
          div(class = "si-stats",
            stat(row$captures[1], "captures"),
            stat(row$individuals[1], "individuals"),
            stat(row$species[1], "species")),
          div(class = "si-row si-star", "Most caught: ", star)),
        div(class = "si-sec",
          div(class = "si-h", "Ecological family"),
          div(class = "si-row si-fam",
            tags$span(class = "si-dot", style = sprintf("background:%s", dash(row$group_color[1]))),
            dash(row$group_label[1])))))
  }

  # The choice popup is bound directly to each marker (add_site_markers /
  # add_species_markers), so a dot tap opens it client-side — no marker_click
  # observer + leafletProxy("pickerMap") addPopups, which silently failed after
  # the picker map had been hidden and re-shown ("change site").

  # "Explore this site" (popup button OR About-modal footer button) -> load it.
  observeEvent(input$siteExplore, {
    removeModal()
    load_site_full(input$siteExplore)   # the native popup closes when the splash hides
  })
  # "About this site" -> instant info card (no bundle load)
  observeEvent(input$siteInfo, showModal(site_info_modal(input$siteInfo)))

  observeEvent(input$pickFromList, load_site_full(input$pickFromList))

  # "Change site" (in the hero band) -> back to the picker-map landing
  observeEvent(input$changeSite, {
    rv$data <- NULL; rv$lb <- NULL; rv$lb_view <- NULL; rv$tag <- NULL; rv$label <- NULL; rv$env <- NULL
    shinyjs::hide("mainTabsWrap"); shinyjs::hide("indivPickerWrap"); shinyjs::hide("envPickerWrap"); shinyjs::show("splash")
    # the picker map was hidden while a site was loaded; nudge it to recompute
    # size now that it's visible again, so it never paints blank/grey on return
    session$sendCustomMessage("kickMaps", list())
  })

  observeEvent(input$demoBtn, {
    d <- load_demo()
    if (is.null(d)) { showNotification("Demo data not found.", type = "error"); return() }
    ingest(d, DEMO_META$label, is_demo = TRUE)
    showNotification(tagList(bs_icon("arrow-counterclockwise"), " Back to the Jornada demo."),
      type = "message", duration = 4)
  })

  # ---- selecting an individual -------------------------------------------
  pick_individual <- function(tag, navigate = TRUE) {
    if (is.null(tag) || is.na(tag) || tag == "") return()
    rv$tag <- tag
    # keep BOTH pickers (sidebar + the inline Home-Range one) in lockstep
    if (!identical(input$indiv, tag))   updateSelectizeInput(session, "indiv",   selected = tag)
    if (!identical(input$indivHR, tag)) updateSelectizeInput(session, "indivHR", selected = tag)
    if (navigate) nav_select("tabs", "dossier")
    row <- rv$lb[rv$lb$tagID == tag, ]
    if (nrow(row) && row$rarity[1] %in% c("Epic", "Legendary")) {
      session$sendCustomMessage("confetti", list(big = row$rarity[1] == "Legendary"))
    }
  }

  observeEvent(input$indiv, {
    if (!is.null(input$indiv) && input$indiv != "" && !identical(input$indiv, rv$tag))
      pick_individual(input$indiv)
  }, ignoreInit = TRUE)

  # inline trap-grid picker: change who's tracked WITHOUT leaving Home Range
  observeEvent(input$indivHR, {
    if (!is.null(input$indivHR) && input$indivHR != "" && !identical(input$indivHR, rv$tag))
      pick_individual(input$indivHR, navigate = FALSE)
  }, ignoreInit = TRUE)

  observeEvent(input$leaderboard_rows_selected, {
    i <- input$leaderboard_rows_selected
    if (length(i) && !is.null(rv$lb_view)) pick_individual(rv$lb_view$tagID[i])
  })

  # pick a random standout individual (shared by the sidebar + dossier buttons)
  surprise_pick <- function() {
    lb <- rv$lb; req(lb)
    pool <- lb$tagID[lb$rarity %in% c("Legendary", "Epic")]
    if (length(pool) == 0) pool <- lb$tagID[seq_len(min(20, nrow(lb)))]
    pick_individual(sample(pool, 1))
  }
  observeEvent(input$surpriseBtn, surprise_pick())

  # ensure an individual is selected (for tabs that need one) -> pick the star
  ensure_individual <- function() {
    if (!is.null(rv$tag)) return(invisible())
    lb <- rv$lb; if (is.null(lb) || nrow(lb) == 0) return(invisible())
    tag <- lb$tagID[1]
    rv$tag <- tag
    updateSelectizeInput(session, "indiv", selected = tag)
    updateSelectizeInput(session, "indivHR", selected = tag)
  }

  # ---- Overview home-nav buttons (Girth-style quick jumps) ---------------
  observeEvent(input$goMap,        nav_select("tabs", "map"))
  observeEvent(input$goCommunity,  nav_select("tabs", "community"))
  observeEvent(input$goPopulation, nav_select("tabs", "population"))
  observeEvent(input$goFame,       nav_select("tabs", "fame"))
  observeEvent(input$goRange, {    # heatmap/replay need an individual — pick the star
    ensure_individual(); nav_select("tabs", "homerange")
  })
  observeEvent(input$goDossier, {  # "Track an animal" door — open the picker (Hall of Fame)
    nav_select("tabs", "fame")
  })
  # dossier empty-state buttons (surface the picker where users land)
  observeEvent(input$goFameFromDossier,   nav_select("tabs", "fame"))
  observeEvent(input$surpriseFromDossier, surprise_pick())

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
    # hover label = a quick name-tag only; the CLICK opens the choice popup
    labs <- lapply(seq_len(nrow(idx)), function(i) htmltools::HTML(sprintf(
      "<div class='pm-pop'><div class='pm-pop-t'>%s %s</div>
       <div class='pm-pop-s'>%s · %s</div>
       <div class='pm-pop-hint'>Tap for site options</div></div>",
      idx$emoji[i], idx$site[i], idx$name[i], idx$state[i])))
    # Bind the choice popup to each marker so a CLICK opens it client-side — no
    # leafletProxy round-trip (which fails once the picker map has been hidden and
    # re-shown via "change site"). This is what makes site selection work again.
    pops <- vapply(seq_len(nrow(idx)),
      function(i) as.character(site_popup_html(idx[i, , drop = FALSE])), character(1))
    leaflet::addCircleMarkers(map, data = idx, lng = ~lng, lat = ~lat, layerId = ~site,
      radius = picker_radius(idx$captures), stroke = TRUE, color = "#ffffff", weight = 1.5,
      opacity = 1, fillColor = ~group_color, fillOpacity = 0.85, label = labs,
      popup = pops, popupOptions = leaflet::popupOptions(maxWidth = 300, minWidth = 230,
        autoPan = TRUE, autoPanPadding = c(40, 55), keepInView = TRUE,
        closeButton = TRUE, closeOnClick = FALSE, className = "pm-pop-card"),
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
       <div class='pm-pop-hint'>Tap for site options</div></div>",
      r$emoji[i], r$site[i], r$name[i], r$state[i],
      format(r$individuals[i], big.mark = ","), format(r$captures[i], big.mark = ","))))
    # native popups (built from the full SITE_INDEX row) so a click opens the
    # choice card client-side — same robustness as the by-site markers
    pops <- vapply(seq_len(nrow(r)), function(i) {
      srow <- if (!is.null(SITE_INDEX)) SITE_INDEX[SITE_INDEX$site == r$site[i], , drop = FALSE] else NULL
      if (is.null(srow) || !nrow(srow)) "" else as.character(site_popup_html(srow))
    }, character(1))
    leaflet::addCircleMarkers(map, data = r, lng = ~lng, lat = ~lat, layerId = ~site,
      radius = picker_radius(r$individuals), stroke = TRUE, color = "#ffffff", weight = 1.5,
      opacity = 1, fillColor = col, fillOpacity = 0.85, label = labs,
      popup = pops, popupOptions = leaflet::popupOptions(maxWidth = 300, minWidth = 230,
        autoPan = TRUE, autoPanPadding = c(40, 55), keepInView = TRUE,
        closeButton = TRUE, closeOnClick = FALSE, className = "pm-pop-card"),
      labelOptions = picker_label_opts, options = leaflet::markerOptions(riseOnHover = TRUE))
  }

  # base map drawn once (tiles + view + initial by-site markers)
  output$pickerMap <- renderLeaflet({
    # Show a clear notice (not an endless spinner) if the site index didn't load.
    # `req()` here would silently halt and spin forever; `validate()` surfaces the
    # cause in-place so a missing/unreadable data/site_index.rds is diagnosable.
    validate(need(
      !is.null(SITE_INDEX) && nrow(SITE_INDEX) > 0,
      "The national site map couldn't load its data (data/site_index.rds is missing or unreadable in this deployment). The rest of the app still works — pick a site, or try the demo."
    ))
    leaflet(options = leafletOptions(minZoom = 2, worldCopyJump = TRUE)) %>%
      addProviderTiles("CartoDB.Positron", options = providerTileOptions(noWrap = TRUE)) %>%
      setView(lng = -96, lat = 41, zoom = 4) %>%
      add_site_markers()
  })

  # Keep the splash + its picker map rendered ONCE and alive while hidden, so the
  # leaflet proxy stays valid across "change site" (otherwise the map is recreated
  # and leafletProxy("pickerMap") can't find it -> dot popups silently fail).
  # pickerMap is a STATIC leafletOutput in ui.R now (not a server renderUI) so it
  # binds reliably on Connect Cloud. Keep it alive while the splash is hidden so
  # leafletProxy("pickerMap") stays valid across "change site".
  outputOptions(output, "pickerMap", suspendWhenHidden = FALSE)

  # swap markers when the user toggles mode or picks a species (proxy = no reflow)
  observeEvent(list(input$pickMode, input$rangeSpecies), {
    req(SITE_INDEX)
    map <- leaflet::leafletProxy("pickerMap") %>% leaflet::clearMarkers() %>% leaflet::clearPopups()
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
      actionButton("runCompare", tagList(bs_icon("bar-chart-steps"), " Compare these sites"),
                   class = "btn-primary w-100 cmp-run"),
      spin(uiOutput("compareOut"), img = "rat1.gif")
    ))
  })

  # build a one-site metric pack from its bundle (memoized so swapping a site
  # back in is instant and a repeat compare never re-crunches the heavy sites)
  cmp_cache <- new.env(parent = emptyenv())
  compare_pack <- function(site) {
    if (!is.null(cmp_cache[[site]])) return(cmp_cache[[site]])
    b <- load_site_bundle(site)
    if (is.null(b)) return(NULL)
    d <- clean_mam(b)
    if (is.null(d) || sum(d$is_capture) == 0) return(NULL)
    cs <- community_stats(d)
    hn <- hill_numbers(d)
    sp <- utils::head(species_summary(d), 5)
    yrs <- range(d$year[is.finite(d$year)])
    res <- list(site = site, label = site_label(site), cs = cs, hn = hn, sp = sp,
                years = if (all(is.finite(yrs))) yrs else c(NA, NA))
    cmp_cache[[site]] <- res
    res
  }

  # Gated on the Compare button (not live-on-keystroke) so it never silently
  # crunches two heavy sites on modal-open or mid-typing — the user presses
  # Compare and gets a deliberate, confirmed result.
  compare_built <- eventReactive(input$runCompare, {
    a <- input$cmpA; b <- input$cmpB
    if (is.null(a) || is.null(b)) return(div(class = "compare-hint", bs_icon("info-circle"),
      " Pick two sites to compare."))
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

  output$compareOut <- renderUI({
    if (is.null(input$runCompare) || input$runCompare == 0)
      return(div(class = "compare-hint", bs_icon("hand-index-thumb"),
        " Pick two sites above, then tap ", tags$b("Compare these sites"), "."))
    compare_built()
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
        downloadLink("reportPdf", tagList(bs_icon("file-earmark-arrow-down"), " report card"),
                     class = "hero-report"),
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
  open_stat_modal <- function(key) {
    d <- rv$data; lb <- rv$lb; req(d, lb)
    bk <- stat_breakdown(d, lb, key)
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
  }
  observeEvent(input$statClick, open_stat_modal(input$statClick))
  # the species bar advertises a click (source = "speciesBar") — wire it to the
  # same ranked species breakdown so the affordance isn't a dead end
  observeEvent(event_data("plotly_click", source = "speciesBar"), open_stat_modal("species"))

  # click a driver-rank bar -> overlay that driver on the abundance plot below
  # (the envLayer observer then sets its best lag); the ranking IS the switcher
  observeEvent(event_data("plotly_click", source = "driverRank"), {
    cd <- event_data("plotly_click", source = "driverRank")$customdata
    if (!is.null(cd) && length(cd) && cd[1] %in% names(ENV_LAYERS))
      updateSelectInput(session, "envLayer", selected = cd[1])
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

  # ---- "answer up front" banners for the data-heavy charts ----------------
  # Each leads its chart card with one plain-English finding, the way the
  # population driver card does. They read the same helpers the charts do.
  output$speciesBarInsight <- renderUI({
    d <- rv$data; req(d)
    sp <- species_summary(d); if (is.null(sp) || nrow(sp) == 0) return(NULL)
    tot <- sum(sp$individuals, na.rm = TRUE)
    share <- if (tot > 0) round(100 * sp$individuals[1] / tot) else 0
    insight_banner("collection", tone = "navy",
      HTML(sprintf("The <b><i>%s</i></b> dominates this site — <span class='ci-hero'>%s%%</span> of individuals (%s of %s).",
        sp$scientificName[1], share, fmt_int(sp$individuals[1]), fmt_int(tot))))
  })

  output$hillInsight <- renderUI({
    d <- rv$data; req(d)
    hn <- hill_numbers(d); if (hn$n_sp == 0) return(NULL)
    if (is.na(hn$even)) return(insight_banner("diagram-3-fill", tone = "muted",
      HTML(sprintf("<span class='ci-hero'>%s</span> species recorded at this site.", hn$n_sp))))
    verdict <- if (hn$even >= 0.75) "an even community"
      else if (hn$even >= 0.5) "a moderately even community"
      else if (hn$even >= 0.3) "an uneven community — a few species dominate"
      else "a highly skewed community — one or two species dominate"
    insight_banner("diagram-3-fill", tone = if (hn$even >= 0.5) "pine" else "gold",
      HTML(sprintf("This is <b>%s</b>: <span class='ci-hero'>%s</span> species seen, but only about <b>%s</b> are common.",
        verdict, hn$n_sp, format(hn$q1, nsmall = 1))))
  })

  output$accumInsight <- renderUI({
    d <- rv$data; req(d)
    sa <- tryCatch(species_accum(d), error = function(e) NULL); if (is.null(sa)) return(NULL)
    complete <- sa$sobs >= 0.85 * sa$chao1
    est <- if (isTRUE(sa$unstable)) sprintf("at least <b>%s</b>", sa$chao1)
           else sprintf("<span class='ci-hero'>%s</span> (95%% CI %s–%s)", sa$chao1, sa$chao_lo, sa$chao_hi)
    insight_banner("graph-up", tone = if (complete) "pine" else "gold",
      HTML(sprintf("Found <span class='ci-hero'>%s</span> species; Chao1 estimates %s are really here — sampling looks %s.",
        sa$sobs, est, if (complete) "close to complete" else "like it's still missing a rare species or two")))
  })

  output$phenoInsight <- renderUI({
    d <- rv$data; req(d)
    bm <- repro_by_month(d); if (is.null(bm)) return(NULL)
    pkm <- if (any(!is.na(bm$pm))) bm$mon[which.max(replace(bm$pm, is.na(bm$pm), -1))] else NA
    pkf <- if (any(!is.na(bm$pf))) bm$mon[which.max(replace(bm$pf, is.na(bm$pf), -1))] else NA
    if (is.na(pkm) && is.na(pkf)) return(insight_banner("calendar-heart", tone = "muted",
      "Too few sexed adults per month to read a clear breeding season here."))
    segs <- character(0)
    if (!is.na(pkm)) segs <- c(segs, sprintf("males in <span class='ci-hero'>%s</span> (%d%%)",
      month.abb[pkm], as.integer(bm$pm[bm$mon == pkm])))
    if (!is.na(pkf)) segs <- c(segs, sprintf("reproductive females in <span class='ci-hero'>%s</span> (%d%%)",
      month.abb[pkf], as.integer(bm$pf[bm$mon == pkf])))
    insight_banner("calendar-heart", tone = "navy",
      HTML(paste0("Breeding peaks — ", paste(segs, collapse = ", "), ".")))
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
      h4("Pick an animal to open its dossier"),
      p("Every individual NEON tagged at this site has a full profile — measurements, a trap-grid home range, capture history, and a shareable card."),
      div(class = "empty-actions",
        actionButton("goFameFromDossier", tagList(bs_icon("trophy-fill"), " Browse the Hall of Fame"),
                     class = "btn-primary"),
        actionButton("surpriseFromDossier", tagList(bs_icon("dice-5-fill"), " Surprise me"),
                     class = "btn-outline-dark"))))
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

  # ---- shareable trading card (html-to-image export) ---------------------
  output$tradingCardWrap <- renderUI({
    tag <- rv$tag
    if (is.null(tag)) return(NULL)
    row <- rv$lb[rv$lb$tagID == tag, ]; req(nrow(row) == 1)
    rm <- rarity_meta(row$rarity[1])
    nick <- if (!is.na(row$nickname[1])) row$nickname[1] else "small mammal"
    chonk <- if (!is.na(row$chonk_pct[1])) paste0(round(row$chonk_pct[1]), "%") else "—"
    yr <- function(x) if (is.na(x)) "" else format(x, "%Y")
    span_yr <- paste(na.omit(unique(c(yr(row$first_seen[1]), yr(row$last_seen[1])))), collapse = "–")
    tcstat <- function(v, l) div(class = "tc-stat", div(class = "tc-stat-v", v), div(class = "tc-stat-l", l))

    div(class = "tradingcard-wrap",
      div(class = "tc-toolbar",
        tags$button(class = "tc-save-btn", onclick = "smtSaveCard()",
                    bsicons::bs_icon("download"), " Save trading card"),
        span(class = "tc-hint", "a shareable card for this individual")),
      # the exportable node
      div(id = "smtCardNode", class = "trade-card", style = sprintf("--rc:%s;", rm$color),
        div(class = "tc-holo"),
        div(class = "tc-top",
          span(class = "tc-tier", paste(rm$icon, row$rarity[1])),
          span(class = "tc-brand", "NEON \U0001F43E")),
        div(class = "tc-emoji-wrap", div(class = "tc-emoji", row$emoji[1])),
        div(class = "tc-id", row$short[1]),
        div(class = "tc-sci", em(row$scientificName[1])),
        div(class = "tc-nick", nick),
        div(class = "tc-stats",
          tcstat(row$captures[1], "captures"),
          tcstat(if (row$career_days[1] > 0) paste0(row$career_days[1], "d") else "—", "career"),
          tcstat(chonk, "chonk %ile"),
          tcstat(if (is.na(row$max_weight[1])) "—" else paste0(round(row$max_weight[1]), "g"), "heaviest")),
        div(class = "tc-foot",
          span(mode_chr(rv$data$siteID), if (nzchar(span_yr)) paste0(" · ", span_yr)),
          span(class = "tc-foot-app", "Small Mammal Tracker"))))
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
      number = list(suffix = "", font = list(color = "#1f2a30", size = 40)),
      delta = list(reference = 50, suffix = " vs typical",
        increasing = list(color = "#1a7f37"), decreasing = list(color = "#2f7fb5"),
        font = list(size = 13)),
      title = list(text = sprintf("<b>%s</b><br><span style='font-size:12px;color:#6b7a85'>adult weight percentile vs %s</span>",
                                  row$chonk_tier[1], row$scientificName[1]),
                   font = list(color = "#1f2a30", size = 16)),
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
                         margin = list(t = 48, b = 26, l = 30, r = 30)) %>%
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
    blurred <- isTRUE(input$blurMode)
    if (blurred) z <- blur_grid(z)
    zmax <- max(z)

    # capture-WEIGHTED centre of activity (a trap caught 20x should pull the
    # centre toward it) — not the unweighted mean of merely-visited cells.
    wsum <- sum(g$captures)
    cx <- if (wsum > 0) sum(g$tx * g$captures) / wsum else NA_real_
    cy <- if (wsum > 0) sum(g$ty * g$captures) / wsum else NA_real_
    hit <- g[g$captures > 0, ]   # visited cells, drawn as points on top

    dark <- is_dark(); barfont <- if (dark) "#9fb0c4" else "#6b7a85"
    # blur smooths counts into a density surface — don't label it "captures".
    cbar <- list(title = if (blurred) "density" else "caps",
                 tickcolor = barfont, tickfont = list(color = barfont))
    if (!blurred && zmax <= 5) cbar$dtick <- 1
    # SINGLE-HUE sequential ramp (theme-aware): the old green->gold->cardinal
    # rainbow implied ordered categories on what is just a count, and failed CVD.
    empty <- if (dark) "#1b2942" else "#f0f3ee"
    ramp  <- if (dark) list(c(0, empty), c(0.001, "#244468"), c(0.5, "#3f7fc4"), c(1, "#7dc0f0"))
             else      list(c(0, empty), c(0.001, "#cfddf0"), c(0.5, "#4f8fc4"), c(1, "#0C234B"))
    zword <- if (blurred) "Density" else "Captures"

    p <- plot_ly(x = LETTERS[1:10], y = 1:10, z = z, type = "heatmap",
      zmin = 0, zmax = max(zmax, 1), colorscale = ramp,
      hovertemplate = paste0("Trap %{x}%{y}<br>", zword, ": %{z}<extra></extra>"),
      showscale = TRUE, xgap = 2, ygap = 2, colorbar = cbar)
    # overlay actual capture points so single-capture animals still pop
    p <- p %>% add_trace(x = LETTERS[hit$tx], y = hit$ty, type = "scatter", mode = "markers",
      marker = list(size = pmax(7, sqrt(hit$captures) * 8), color = if (dark) "#e6edf5" else "#1f2a30",
                    line = list(color = if (dark) "#0e1726" else "#ffffff", width = 1.5)),
      text = paste0("Trap ", LETTERS[hit$tx], hit$ty, " · ", hit$captures, " cap"),
      hovertemplate = "%{text}<extra></extra>", inherit = FALSE, showlegend = FALSE)
    if (is.finite(cx) && is.finite(cy))
      p <- p %>% add_trace(x = LETTERS[round(cx)], y = round(cy), type = "scatter",
        mode = "markers", marker = list(symbol = "x", size = 16, color = "#AB0520",
        line = list(color = "#ffffff", width = 2)), name = "centre",
        hovertemplate = "capture-weighted centre of activity<extra></extra>", inherit = FALSE)
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
      text = ~paste0(format(individuals, big.mark = ","), " indiv"), textposition = "outside",
      textfont = list(color = "#6b7a85", size = 11),
      hovertemplate = "<b>%{y}</b><br>%{x:,} captures · %{text}<br><span style='color:#cfe0f5'>tap for the full species breakdown</span><extra></extra>")
    plotly_theme(p, legend = FALSE) %>%
      plotly::layout(xaxis = list(title = "captures"), yaxis = list(title = ""),
                     showlegend = FALSE, margin = list(l = 180, t = 44)) %>%
      ctx_anno()
  })

  donut_center <- function(total, label) list(
    text = sprintf("<b>%s</b><br><span style='font-size:11px;color:%s'>%s</span>",
      format(total, big.mark = ","), if (is_dark()) "#9fb0c4" else "#6b7a85", label),
    showarrow = FALSE, font = list(color = if (is_dark()) "#e8eef2" else "#1f2a30", size = 20))

  output$sexDonut <- renderPlotly({
    d <- rv$data; req(d)
    # one row per INDIVIDUAL (sex is stable per animal) so a much-recaptured
    # animal doesn't count many times — the "% of handled" denominator is then
    # distinct individuals, matching the deduped diversity card on this tab.
    per <- d %>% dplyr::filter(!is.na(.data$tagID)) %>%
      dplyr::group_by(.data$tagID) %>%
      dplyr::summarise(sex = mode_chr(.data$sex), .groups = "drop")
    if (nrow(per) == 0) return(note_plot("No handled animals to profile", "\U00002640\UFE0F"))
    key <- factor(ifelse(per$sex %in% c("F", "M"), per$sex, "U"), levels = c("F", "M", "U"))
    tab <- as.data.frame(table(key)); names(tab) <- c("key", "n")
    # keep a fixed key->color->label mapping so slices never swap colors
    lab <- c(F = "Female", M = "Male", U = "Unknown")
    col <- c(F = "#c2255c", M = "#2f7fb5", U = "#6c757d")
    tab$label <- lab[as.character(tab$key)]
    plot_ly(tab, labels = ~label, values = ~n, type = "pie", hole = 0.62, sort = FALSE,
      marker = list(colors = unname(col[as.character(tab$key)]), line = list(color = "#ffffff", width = 2)),
      pull = c(0.03, 0, 0), textinfo = "percent", textposition = "inside",
      insidetextorientation = "horizontal", textfont = list(color = "#ffffff", size = 13),
      hovertemplate = "<b>%{label}</b><br>%{value:,} individuals · %{percent:.0%} of handled<extra></extra>") %>%
      plotly::layout(title = list(text = "Sex", font = list(color = if (is_dark()) "#c3cedd" else "#344049", size = 14)),
        paper_bgcolor = "rgba(0,0,0,0)", showlegend = TRUE,
        legend = list(orientation = "h", y = -0.05, x = 0.5, xanchor = "center", font = list(size = 11)),
        annotations = list(donut_center(sum(tab$n), "individuals")),
        hoverlabel = list(bgcolor = "rgba(12,35,75,0.96)", bordercolor = "#FFD200",
          font = list(color = "#ffffff", family = "Rubik", size = 13)),
        font = list(color = if (is_dark()) "#c3cedd" else "#344049"), margin = list(t = 38, b = 30, l = 10, r = 10)) %>%
      plotly::config(displayModeBar = FALSE)
  })

  output$ageDonut <- renderPlotly({
    d <- rv$data; req(d)
    # one row per INDIVIDUAL (its modal life-stage). NA stage is kept as
    # "unknown" so the slice IS the honest staging-coverage disclosure, and the
    # denominator is distinct individuals (not capture-rows) — so this can't
    # contradict the deduped diversity card six inches away.
    per <- d %>% dplyr::filter(!is.na(.data$tagID)) %>%
      dplyr::group_by(.data$tagID) %>%
      dplyr::summarise(lifeStage = mode_chr(.data$lifeStage), .groups = "drop")
    if (nrow(per) == 0) return(note_plot("No handled animals to profile", "\U0001F423"))
    # FIX: pin life-stage order + named colors so a stage always gets the same color
    lvls <- c("juvenile", "subadult", "adult", "unknown")
    col  <- c(juvenile = "#4bb87a", subadult = "#AB0520", adult = "#16386e", unknown = "#6c757d")
    per$stage <- factor(ifelse(!is.na(per$lifeStage) & per$lifeStage %in% lvls, per$lifeStage, "unknown"), levels = lvls)
    tab <- as.data.frame(table(per$stage)); names(tab) <- c("stage", "n")
    tab <- tab[tab$n > 0, , drop = FALSE]
    plot_ly(tab, labels = ~stage, values = ~n, type = "pie", hole = 0.62, sort = FALSE,
      marker = list(colors = unname(col[as.character(tab$stage)]), line = list(color = "#ffffff", width = 2)),
      textinfo = "percent", textposition = "inside", insidetextorientation = "horizontal",
      textfont = list(color = "#ffffff", size = 13),
      hovertemplate = "<b>%{label}</b><br>%{value:,} individuals · %{percent:.0%} of handled<extra></extra>") %>%
      plotly::layout(title = list(text = "Life stage", font = list(color = if (is_dark()) "#c3cedd" else "#344049", size = 14)),
        paper_bgcolor = "rgba(0,0,0,0)", showlegend = TRUE,
        legend = list(orientation = "h", y = -0.05, x = 0.5, xanchor = "center", font = list(size = 11)),
        annotations = list(donut_center(sum(tab$n), "individuals")),
        hoverlabel = list(bgcolor = "rgba(12,35,75,0.96)", bordercolor = "#FFD200",
          font = list(color = "#ffffff", family = "Rubik", size = 13)),
        font = list(color = if (is_dark()) "#c3cedd" else "#344049"), margin = list(t = 38, b = 30, l = 10, r = 10)) %>%
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
    # ONE shared y-scale across panels: a 3-capture plot and a 40-capture plot
    # must NOT peak at the same height (free axes manufactured a false "all plots
    # alike" read). gymax drives both the shared range and the label headroom.
    gymax <- suppressWarnings(max(ds$count, na.rm = TRUE))
    if (!is.finite(gymax) || gymax <= 0) gymax <- 1

    # facet-like layout, one mini time-series per plot. The legend is built from
    # the REAL data traces: each species' FIRST trace (in any panel) carries
    # showlegend = TRUE, the rest share its legendgroup. (Phantom all-NA legend
    # traces get silently dropped by plotly/subplot, which is why this is robust.)
    seen <- new.env(parent = emptyenv())
    labcol <- if (is_dark()) "#cfe0f5" else "#16386e"   # readable in both themes
    mk <- function(pl, first) {
      dd <- ds[ds$plotID == pl, ]
      ymax <- gymax                              # shared scale across all panels
      xleft <- suppressWarnings(min(dd$date, na.rm = TRUE))
      p <- plot_ly()
      for (s in unique(dd$scientificName)) {
        sd <- dd[dd$scientificName == s, ]
        show <- is.null(seen[[s]]); if (show) assign(s, TRUE, envir = seen)
        p <- p %>% add_trace(data = sd, x = ~date, y = ~count, type = "scatter",
          mode = "lines+markers", name = s, legendgroup = s, showlegend = show,
          marker = list(size = 5, color = pal[[s]]), line = list(width = 1.5, color = pal[[s]]),
          hovertemplate = paste0(pl, "<br>%{x|%b %Y}: %{y}<extra></extra>"))
      }
      # Per-panel plotID label as a TEXT TRACE in the panel's own data space, sat
      # in headroom above the data. A layout annotation with xref="x domain" is
      # NOT remapped per-panel by subplot() — every label collapses onto the first
      # panel (you see only one) — but a trace stays bound to its own panel. The
      # widened y-range gives the label clear space above the lines.
      p <- p %>% add_trace(x = xleft, y = ymax * 1.12, type = "scatter", mode = "text",
        text = pl, textposition = "middle right", cliponaxis = FALSE,
        textfont = list(color = labcol, size = 10), showlegend = FALSE, hoverinfo = "skip")
      p %>% plotly::layout(
        xaxis = list(gridcolor = "rgba(31,42,48,0.06)"),
        yaxis = list(gridcolor = "rgba(31,42,48,0.06)", range = c(0, ymax * 1.25)))
    }
    sub <- lapply(seq_along(plots), function(i) mk(plots[i], i == 1))
    plotly::subplot(sub, nrows = ceiling(length(plots) / 2), shareX = TRUE, shareY = TRUE,
                    titleX = FALSE, margin = 0.05) %>%
      plotly::layout(paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
                     font = list(color = "#344049", family = "Rubik"),
                     margin = list(t = 44, b = 72), showlegend = TRUE,
                     hoverlabel = list(bgcolor = "rgba(12,35,75,0.96)", bordercolor = "#FFD200",
                       font = list(color = "#ffffff", family = "Rubik", size = 13)),
                     legend = list(orientation = "h", x = 0.5, xanchor = "center",
                                   y = -0.08, yanchor = "top", font = list(size = 10),
                                   itemsizing = "constant", bgcolor = "rgba(255,255,255,0.6)")) %>%
      ctx_anno() %>%
      plotly::config(displayModeBar = FALSE)
  })

  # ---- breeding phenology -------------------------------------------------
  output$phenoPlot <- renderPlotly({
    d <- rv$data; req(d)
    # deduped to one row per (individual, calendar month), completed to 12 months,
    # <5-sexed-adult months suppressed — shared with the phenology answer-banner.
    by_m <- repro_by_month(d)
    if (is.null(by_m)) return(note_plot("No adult reproductive data", "\U0001F423"))
    mlab <- month.abb[by_m$mon]
    p <- plot_ly()
    # seasonal climatology of the chosen env layer, behind the breeding curves
    es <- env_sel()
    if (!is.null(es)) {
      clim <- env_climatology(es$env, es$layer, es$lag)
      if (!is.null(clim) && nrow(clim)) {
        meta <- ENV_LAYERS[[es$layer]]
        nm <- meta$label; if (es$lag) nm <- sprintf("%s · lag %d mo", nm, es$lag)
        if (es$demo) nm <- paste0(nm, " (demo)")
        p <- p %>% add_trace(x = month.abb[clim$mon], y = clim$value, yaxis = "y2",
          type = "scatter", mode = "lines", fill = "tozeroy", name = nm, legendgroup = "env",
          line = list(color = meta$color, width = 1.6, shape = "spline"),
          fillcolor = paste0(meta$color, "1f"),
          hovertemplate = paste0(meta$label, " (typical): %{y} ", meta$unit, "<extra></extra>"))
      }
    }
    p <- p %>%
      add_trace(x = mlab, y = by_m$pm, type = "scatter", mode = "lines+markers", name = "breeding males",
        line = list(color = "#2f7fb5", width = 3), marker = list(size = 8, color = "#2f7fb5"),
        connectgaps = FALSE, customdata = by_m$males,
        hovertemplate = "%{x}<br>%{y}% of adult males scrotal<br>(n = %{customdata} adult males)<extra></extra>") %>%
      add_trace(x = mlab, y = by_m$pf, type = "scatter", mode = "lines+markers", name = "reproductive females",
        line = list(color = "#c2255c", width = 3), marker = list(size = 8, color = "#c2255c"),
        connectgaps = FALSE, customdata = by_m$females,
        hovertemplate = "%{x}<br>%{y}% of adult females pregnant/lactating<br>(n = %{customdata} adult females)<extra></extra>")
    p <- plotly_theme(p) %>% plotly::layout(
      xaxis = list(title = "", categoryorder = "array", categoryarray = month.abb),
      yaxis = list(title = "% reproductively active", range = c(0, 100)),
      hovermode = "x unified", margin = list(t = 44)) %>% ctx_anno()
    if (!is.null(es)) p <- p %>% plotly::layout(yaxis2 = env_axis_spec(es$layer, show = TRUE))
    p
  })

  # ---- body-size profile (violin per species, "Position DNA") ------------
  output$sizeViolin <- renderPlotly({
    d <- rv$data; req(d)
    # ADULTS only (juveniles/subadults make a mixture that reads as a fat/bimodal
    # violin), and ONE weight per INDIVIDUAL (its mean) so a much-recaptured
    # animal isn't counted as many "weights" — matches the adults + per-individual
    # convention the leaderboard already uses.
    w <- dplyr::filter(d, !is.na(.data$tagID), !is.na(.data$weight), .data$weight > 0,
                       !is.na(.data$scientificName), .data$lifeStage == "adult") %>%
      dplyr::group_by(.data$tagID) %>%
      dplyr::summarise(weight = mean(.data$weight),
                       scientificName = mode_chr(.data$scientificName), .groups = "drop")
    keep <- w %>% dplyr::count(.data$scientificName) %>% dplyr::filter(.data$n >= 8) %>%
      dplyr::pull(.data$scientificName)
    w <- w[w$scientificName %in% keep, ]
    if (nrow(w) == 0) return(note_plot("Not enough weighed adults<br>for a size profile", "⚖️"))
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
        hovertemplate = paste0("<b>", s, "</b> · n=", nrow(sub),
                               " adults<br>%{y} g (per-individual mean)<extra></extra>"))
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
    # environmental overlay FIRST so it reads as soft context behind the lines
    es <- env_sel()
    if (!is.null(es))
      p <- add_env_overlay(p, es$env, es$layer, es$lag, yaxis = "y3",
                           xlim = range(mn$date, na.rm = TRUE), demo = es$demo)
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
      mode = "lines", name = "site total, per 100 trap-nights",
      line = list(color = "rgba(31,42,48,0.55)", width = 2, dash = "dot"),
      hovertemplate = "%{x|%b %Y}<br>%{y} captures per 100 trap-nights<extra></extra>")
    p <- plotly_theme(p) %>% plotly::layout(
      yaxis  = list(title = "MNKA (individuals known alive)", color = "#16386e"),
      yaxis2 = list(title = "captures per 100 trap-nights (site total)", color = "#7a8896",
                    overlaying = "y", side = "right", gridcolor = "rgba(0,0,0,0)"),
      xaxis  = list(title = "", showspikes = TRUE, spikemode = "across",
                    spikethickness = 1, spikecolor = "#7a8896", spikedash = "dot"),
      hovermode = "x", margin = list(t = 44),
      annotations = list(list(text = "⋯ dotted = catch-per-effort (right axis)",
        x = 0, y = 1.08, xref = "paper", yref = "paper", xanchor = "left", yanchor = "bottom",
        showarrow = FALSE, font = list(color = "#7a8896", size = 11, family = "Rubik")))) %>% ctx_anno()
    # hidden y3 for the env area: pure background context (y2 already holds CPUE)
    if (!is.null(es))
      p <- p %>% plotly::layout(yaxis3 = c(env_axis_spec(es$layer, show = FALSE),
                                           list(anchor = "free", position = 1)))
    p
  })

  # ---- species accumulation ----------------------------------------------
  output$accumPlot <- renderPlotly({
    d <- rv$data; req(d)
    sa <- species_accum(d)
    if (is.null(sa)) return(note_plot("Not enough data for accumulation", "\U0001F4C8"))
    cv <- sa$curve
    # The Chao1 CI upper bound explodes when doubletons are scarce (e.g. 33 from a
    # single doubleton), which used to draw a giant unlabeled pink rectangle that
    # dwarfed the curve. Cap the band for legibility, LABEL it, pin the y-axis, and
    # state the true range in the caption so it stays honest.
    cap <- min(sa$chao_hi, max(2 * sa$chao1, sa$sobs + 5))
    band_name <- if (isTRUE(sa$unstable)) "Chao1 interval (wide — unstable)"
                 else sprintf("Chao1 95%% CI (%d–%d)", sa$chao_lo, sa$chao_hi)
    p <- plot_ly() %>%
      add_trace(x = cv$bouts, y = cv$hi, type = "scatter", mode = "lines",
        line = list(width = 0), showlegend = FALSE, hoverinfo = "skip") %>%
      add_trace(x = cv$bouts, y = cv$lo, type = "scatter", mode = "lines", fill = "tonexty",
        fillcolor = "rgba(22,56,110,0.14)", line = list(width = 0),
        name = "±1 SD (resampling)", hoverinfo = "skip") %>%
      add_trace(x = cv$bouts, y = cv$richness, type = "scatter", mode = "lines+markers",
        name = "species found", line = list(color = "#16386e", width = 3),
        marker = list(size = 6, color = "#16386e"),
        hovertemplate = "after %{x} bouts<br>%{y:.1f} species<extra></extra>") %>%
      # Chao1 CI band — capped at `cap` for display, now NAMED in the legend
      add_trace(x = c(range(cv$bouts), rev(range(cv$bouts))),
        y = c(cap, cap, sa$chao_lo, sa$chao_lo),
        type = "scatter", mode = "lines", fill = "toself",
        fillcolor = "rgba(171,5,32,0.10)", line = list(width = 0),
        name = band_name, hoverinfo = "skip") %>%
      add_trace(x = range(cv$bouts), y = rep(sa$chao1, 2), type = "scatter", mode = "lines",
        name = if (isTRUE(sa$unstable)) sprintf("Chao1 ≥ %d (lower bound)", sa$chao1)
               else sprintf("Chao1 ≈ %d", sa$chao1),
        line = list(color = "#AB0520", width = 1.5, dash = "dash"), hoverinfo = "skip")
    # Short, left-anchored, AND on its own line ABOVE the right-anchored
    # "SITE · years" corner note (ctx sits at y=1.03; this rides higher at y=1.13)
    # so the two never collide even on a narrow phone-width card. Just the headline
    # count — the Chao1 estimate, its interval, and the (wide/unstable) flag all
    # live in the legend (band_name + the dashed-line name), and the doubleton
    # caveat is in the info popover, so the subtitle stays short enough never to clip.
    anno_txt <- sprintf("observed %d species", sa$sobs)
    plotly_theme(p) %>% plotly::layout(
      xaxis = list(title = "trapping bouts (months)"),
      yaxis = list(title = "cumulative species", range = c(0, cap + 1), rangemode = "tozero"),
      margin = list(l = 50, r = 30, t = 84, b = 40),
      annotations = list(list(text = anno_txt,
        x = 0, y = 1.13, xref = "paper", yref = "paper", xanchor = "left", yanchor = "bottom",
        showarrow = FALSE, font = list(color = "#6b7a85", size = 11)))) %>% ctx_anno()
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
    lead <- if (!is.na(cc$mean_detect))
      insight_banner("incognito", tone = "navy",
        HTML(sprintf("Traps caught about <span class='ci-hero'>%s</span> of the animals present per bout — the gap between the navy estimate and the grey known-alive line is everything they missed.",
          paste0(round(100 * cc$mean_detect), "%")))) else NULL
    tagList(lead,
      div(class = "detect-head",
        chip(pct(cc$mean_p),      "per-night detection (p̂)", "#0C234B"),
        chip(pct(cc$mean_detect), "of population caught / bout", "#2f7fb5"),
        chip(cc$n_estimable,      sprintf("estimable bouts (of %d)", cc$n_bouts), "#1a7f37")))
  })

  # The driver overlaid on the detection plot: the sidebar pick if any, else the
  # auto best-match (top of the ranking, |r| >= 0.2). Shared so the °F/°C toggle's
  # visibility and the plot agree on what's currently shown.
  detect_overlay <- reactive({
    es <- env_sel()
    if (!is.null(es)) return(c(es, list(auto = FALSE)))
    if (!is.null(rv$env)) {
      ca <- env_corr_all(rv$data, rv$env)
      if (!is.null(ca) && nrow(ca) && !is.na(ca$r[1]) && abs(ca$r[1]) >= 0.2) {
        w <- ca[1, ]
        return(list(layer = w$layer, lag = as.integer(w$lag), env = rv$env,
                    demo = identical(attr(rv$env, "source"), "demo"), auto = TRUE))
      }
    }
    NULL
  })
  # drives the conditional °F/°C toggle in the card header — only when temp is overlaid
  output$detectTempActive <- reactive({
    ov <- detect_overlay(); !is.null(ov) && identical(ov$layer, "temp")
  })
  outputOptions(output, "detectTempActive", suspendWhenHidden = FALSE)

  output$detectPlot <- renderPlotly({
    cc <- detect_cc()
    if (is.null(cc) || is.null(cc$series) || nrow(cc$series) == 0)
      return(note_plot(paste0("No multi-night recapture data to estimate detection here.<br>",
                              "<span style='font-size:13px'>This site's grids are single-night, or had too few within-bout recaptures.<br>",
                              "MNKA & CPUE above are the right index for these.</span>"), "\U0001F50E"))
    s <- cc$series
    # cap any infinite upper bound for plotting (shouldn't occur post-roll-up, but be safe)
    s$hi[!is.finite(s$hi)] <- s$N[!is.finite(s$hi)] * 2
    # Overlay an environmental driver. Honour the sidebar pick if the user made
    # one; otherwise AUTO-overlay the BEST-correlated driver at its best lag (the
    # winner from the ranking above) — but only when the link is real (|r| >= 0.2)
    # — so the "which driver does this track?" answer plays out in action here.
    es <- detect_overlay(); auto <- isTRUE(es$auto); best_label <- NULL
    # temperature unit: default °F; the card-header toggle (input$tempUnit) flips to °C
    tempF <- !is.null(es) && identical(es$layer, "temp") && (input$tempUnit %||% "F") == "F"
    conv  <- if (tempF) function(x) x * 9 / 5 + 32 else NULL
    ulab  <- if (tempF) "°F" else NULL
    # Label the overlaid driver with its DIRECTION (so a negative winner reads as
    # an inverse relationship, not a weak one) + lag. "best match" = the auto top
    # driver; "showing" = one you picked (sidebar or by tapping a ranking bar).
    if (!is.null(es)) {
      meta <- ENV_LAYERS[[es$layer]]
      sc <- tryCatch(env_corr_scan(rv$data, rv$env, es$layer), error = function(e) NULL)
      dirtxt <- if (!is.null(sc) && !is.na(sc$r)) sprintf(" (%s)", if (sc$r < 0) "inverse" else "positive") else ""
      lagtxt <- if (es$lag == 0) "same month" else sprintf("lag %d mo", es$lag)
      best_label <- sprintf("%s: %s%s · %s", if (auto) "best match" else "showing",
                            meta$label, dirtxt, lagtxt)
    }
    p <- plot_ly()
    if (!is.null(es))   # env area first → soft context behind the abundance band
      p <- add_env_overlay(p, es$env, es$layer, es$lag, yaxis = "y2",
                           xlim = range(s$date, na.rm = TRUE), demo = es$demo,
                           conv = conv, unit_label = ulab)
    p <- p %>%
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
    # the auto-best label rides its OWN line (y=1.12) so it can't collide with the
    # right-anchored ctx note; coloured to match the shaded driver + its y2 axis
    annos <- if (!is.null(best_label)) list(list(text = best_label,
        x = 0, y = 1.12, xref = "paper", yref = "paper", xanchor = "left", yanchor = "bottom",
        showarrow = FALSE, font = list(color = ENV_LAYERS[[es$layer]]$color, size = 11))) else list()
    p <- plotly_theme(p) %>% plotly::layout(
      xaxis = list(title = ""), yaxis = list(title = "animals on the grid(s)", rangemode = "tozero"),
      margin = list(l = 50, r = 30, t = if (!is.null(best_label)) 72 else 48, b = 40),
      annotations = annos) %>% ctx_anno()   # t roomy for ctx caption / best-match label
    if (!is.null(es)) p <- p %>% plotly::layout(yaxis2 = env_axis_spec(es$layer, show = TRUE, unit_label = ulab))
    p
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

  # ---- site report card (server-side PDF via downloadHandler) -------------
  # The hero "report card" link is a downloadLink that streams this real,
  # paginated PDF, generated in R/report_pdf.R (base grDevices + grid + ggplot2,
  # no LaTeX/Chrome). Replaces the old window.print() of hidden HTML, which cut
  # off / mis-paginated, especially on mobile.
  output$reportPdf <- downloadHandler(
    filename = function() {
      slug <- gsub("(^-|-$)", "", gsub("[^A-Za-z0-9]+", "-", rv$label %||% "site"))
      sprintf("NEON-SmallMammal-ReportCard_%s_%s.pdf", slug, format(Sys.Date(), "%Y%m%d"))
    },
    content = function(file) {
      d <- rv$data; req(d)
      cc <- tryCatch(detect_cc(), error = function(e) NULL)
      render_report_pdf(file, d, rv$label %||% mode_chr(d$siteID), isTRUE(rv$is_demo), cc)
    },
    contentType = "application/pdf"
  )

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
