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
      # desert-night tooltips: teal-edged dark card on dark, deep-teal on light
      hoverlabel = list(bgcolor = if (dark) "rgba(14,29,64,0.96)" else "rgba(20,46,72,0.96)",
        bordercolor = if (dark) "#38a8e8" else "#1f78c4",
        font = list(color = "#ffffff", family = "Rubik", size = 13))
    ) %>%
      plotly::config(displayModeBar = FALSE, responsive = TRUE)
  }

  # ---- mode-aware data/accent colors -------------------------------------
  # The app was built dark-first; the desert-night trace/accent colors below are
  # tuned for the dark navy background and several wash out on a white panel
  # (azure ~2.6:1, salmon ~2.3:1, lime ~1.8:1). These helpers keep the EXACT
  # dark color when is_dark(), and swap to a darker/more-saturated equivalent
  # that clears ~>=3:1 (most >=4:1) on white in light mode. Reading is_dark()
  # here takes the same reactive dependency on the toggle the plots already do,
  # so charts re-theme on switch. DARK values are byte-for-byte the originals.
  # NOTE: only used for traces/markers/axis-title colors drawn on the
  # transparent plot bg, and for stat NUMBERS that sit on a light --bg surface
  # (.hill-v / .detect-v). Accents that sit on an always-dark CSS surface (the
  # stat value-boxes, the hover card) keep their dark color in BOTH modes.
  acc <- function() if (is_dark()) "#38a8e8" else "#1565b0"   # primary azure
  # map any dark-tuned data hue to its light-readable twin (dark identity = leave)
  dcol <- function(hex) {
    if (is_dark()) return(hex)
    switch(tolower(hex),
      "#38a8e8" = "#1565b0", "#43b8e8" = "#1565b0", "#5cc6f5" = "#1f8fbf",
      "#fb8a7e" = "#c4452f",
      "#9bd24a" = "#5a8a1e", "#b7d84c" = "#5a8a1e",
      "#5fb56a" = "#2f7d3a",
      "#e0b43a" = "#9a6b00", "#ffd24a" = "#9a6b00",
      "#8a97a8" = "#5a6a82", "#9aa7b4" = "#5a6a82",
      hex)   # already-fine hues (#138086 teal, #5b3a8a purple) pass through
  }

  # shown on individual-only views when nobody is selected yet
  PICK_MSG <- "Pick an individual first.<br>Open the <b>Hall of Fame</b> and tap a row,<br>or hit \U201CSurprise me\U201D in the Track-an-individual picker."

  # Append the current site + year-range as a small top-right caption on a plot
  # (uses add_annotations so it never clobbers a plot's own annotations).
  ctx_anno <- function(p) {
    if (is.null(rv$ctx)) return(p)
    # y just above the plot (1.03) so it clears the panel without needing a huge
    # top margin; plotly_theme's t=48 gives every plot enough headroom.
    plotly::add_annotations(p, text = rv$ctx, x = 1, y = 1.03, xref = "paper", yref = "paper",
      xanchor = "right", yanchor = "bottom", showarrow = FALSE,
      # light caption nudged #6b7a89 (4.4:1) -> #647585 (4.75:1) so scope-stamp text
      # clears the WCAG 4.5:1 text gate on the white panel; still a soft muted slate.
      font = list(color = if (is_dark()) "#9fb0c4" else "#647585", size = 11, family = "Rubik"))
  }

  # Mode-aware muted plot text (annotations / secondary-axis labels). A single
  # hardcoded slate like #6b7a85 is 4.42:1 on white (under the 4.5:1 text gate) AND
  # only 3.45:1 on the dark navy panel — it can't win on both backgrounds. This
  # returns the light twin #63727d (4.96:1 on white) / #9fb0c4 (6.2:1 on navy).
  muted_ink <- function() if (is_dark()) "#9fb0c4" else "#63727d"

  # A centered-message placeholder for plots that have nothing to show.
  note_plot <- function(msg, icon = "\U0001F50D") {
    plotly::plot_ly(type = "scatter", mode = "markers") %>%
      plotly::layout(
        paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
        xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
        annotations = list(list(text = paste0(icon, "<br>", msg), showarrow = FALSE,
          # light muted nudged #6b7a85 (4.42:1) -> #63727d (4.96:1) for the 4.5:1 gate
          font = list(color = if (is_dark()) "#9fb0c4" else "#63727d", size = 15), align = "center"))) %>%
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

  # Mode-aware species -> color map, shared by every categorical plot. Reading
  # is_dark() here makes the palette a reactive dependency on the toggle, so the
  # map / trend / morphospace recolor on light<->dark switch. Species are sorted
  # inside make_species_pal(), so a given species keeps the SAME ramp slot in a
  # given mode -> colour stays consistent across all plots.
  species_pal <- reactive({
    if (is.null(rv$data)) return(character(0))
    make_species_pal(rv$data, dark = is_dark())
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
          tags$span(HTML(" <b>Demo overlay</b>, an illustrative monthly series, <b>not</b> NEON data. Run <code>scripts/refresh_env_data.R</code> to bundle the real product.")))
    else
      div(class = "env-source env-real", bs_icon("patch-check-fill"),
          tags$span(" Live from co-located NEON sensors at this site."))
  })

  # Shared honesty p-value for the env dredge (circular-shift null over the full
  # driver x lag scan). ONE reactive so envCorrNote and envDriverRank don't each
  # pay the permutation cost; recomputes only when the site's data or env changes
  # (not on driver-bar selection — the p is about the best-of-all-drivers dredge).
  env_pval <- reactive({
    d <- rv$data; env <- rv$env
    if (is.null(d) || is.null(env)) return(NULL)
    tryCatch(env_corr_pvalue(d, env), error = function(e) NULL)
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
    # HONESTY GATE: |r| alone is best-of-65 (5 drivers x 13 lags) — an upward-
    # biased dredge. Downgrade the verdict word to "Apparent link" whenever the
    # circular-shift null can't reject chance (p >= 0.05), however large |r|.
    pv     <- env_pval()
    p_val  <- if (!is.null(pv)) pv$p else NA_real_
    sig    <- is.finite(p_val) && p_val < 0.05
    rail   <- if (sig && strength >= 0.6) "rail-strong"
              else if (sig && strength >= 0.35) "rail-mod" else "rail-weak"
    slabel <- if (!sig) "Apparent"
              else if (strength >= 0.6) "Strong"
              else if (strength >= 0.35) "Moderate"
              else if (strength >= 0.2) "Weak"
              else "Negligible"
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
          title = "correlation coefficient, -1 to +1; tap the (i) above for what it means",
          bs_icon(glyph), HTML(sprintf("r&nbsp;%+.2f", sc$r)))),
      div(class = "ec-foot",
        tags$span(class = "ec-meta", bs_icon("clock-history"),
          if (sc$lag == 0) "same-month signal"
          else HTML(sprintf("<b>%d-mo</b> lead", sc$lag))),
        tags$span(class = "ec-meta-dot"),
        tags$span(class = "ec-meta", bs_icon("calendar3"),
          HTML(sprintf("<b>%d</b> months matched", sc$n))),
        tags$span(class = "ec-meta-dot"),
        tags$span(class = "ec-meta",
          title = sprintf("shuffle-null p from re-scanning the best of %s driver\U00D7lag combos",
                          if (!is.null(pv)) as.character(pv$n_search) else "many"),
          if (is.finite(p_val)) HTML(sprintf("shuffle-null <b>p = %.2f</b>%s", p_val,
              if (!is.null(pv)) sprintf(" (best of %d)", pv$n_search) else ""))
          else "series too short for a p"),
        tags$span(class = paste("ec-meta ec-dir", if (pos) "ec-sgn-pos" else "ec-sgn-neg"),
          HTML(sprintf("higher \U2192 <b>%s</b> animals", dir)))))
  })

  # ---- Seasonal climate: the cascade read (the desert fix) -----------------
  # The monthly scan above deseasonalizes monthly catch vs monthly precip, so it
  # CANNOT see "a big summer monsoon -> next year's seed-eater boom" — the
  # monsoon is averaged out of the annual cycle it subtracts. This card uses the
  # Driver Cascade's method: aggregate the driver by SEASON (monsoon Jul-Sep,
  # winter Oct-Mar, spring temp) into one value per year and correlate at the
  # prior's year-lag. Same rules as ann_env_seasonal so the app + cascade agree.
  # Per-site annual n is tiny (6-10 yrs) — SUGGESTIVE, not a verdict; the pooled
  # cross-site test lives in the Driver Cascade app.
  output$seasonalDriver <- renderUI({
    d <- rv$data; if (is.null(d) || is.null(rv$env) || !"ym" %in% names(d)) return(NULL)
    ac <- d %>% dplyr::filter(!is.na(.data$ym)) %>%
      dplyr::mutate(yr = suppressWarnings(as.integer(substr(.data$ym, 1, 4)))) %>%
      dplyr::group_by(.data$yr) %>%
      dplyr::summarise(cap = sum(.data$is_capture),
                       tn  = sum(.data$trap_effort, na.rm = TRUE), .groups = "drop")
    ac <- ac[is.finite(ac$yr) & is.finite(ac$tn) & ac$tn > 0, , drop = FALSE]
    if (nrow(ac) < 3) return(NULL)
    resp  <- data.frame(year = ac$yr, value = 100 * ac$cap / ac$tn)
    site_code <- if ("siteID" %in% names(d)) mode_chr(d$siteID) else NULL
    biome <- seasonal_biome(site_code)
    links <- tryCatch(seasonal_driver_links(rv$env, resp, biome = biome), error = function(e) NULL)
    eyebrow <- div(class = "ec-eyebrow", bs_icon("calendar-range"),
                   tags$span("seasonal climate \U00B7 the cascade read"),
                   info_pop("The seasonal read",
                     p("A single annual rain total blends two seasons that feed different things: the ", tags$b("summer monsoon"), " (Jul-Sep) grows the C4-grass seed crop desert granivores cash in the ", tags$b("next"), " year, while ", tags$b("winter rain"), " (Oct-Mar) feeds spring forbs. The month-to-month overlay above mixes them into one number."),
                     p("Here we aggregate the driver by season and correlate it against this site's yearly catch rate at the prior's lag, the way the Driver Cascade does. ", tags$b("Per-site n is only a handful of years"), ", so this is suggestive, not significant. The honest test pools many sites in the Driver Cascade app.")))
    if (is.null(links) || !nrow(links)) {
      return(div(class = "ec ec-seasonal rail-weak", eyebrow,
        div(class = "ec-hero", div(class = "ec-hero-text",
          "No co-located seasonal rain record at this site, so the monsoon and winter seasons can't be tested here. That is missing climate data, not a missing signal."))))
    }
    lead <- links[links$expected, , drop = FALSE]; if (!nrow(lead)) lead <- links
    L <- lead[1, ]; pos <- L$r >= 0; dir <- if (pos) "more" else "fewer"
    strength <- abs(L$r)
    rail <- if (strength >= 0.6) "rail-strong" else if (strength >= 0.35) "rail-mod" else "rail-weak"
    sub  <- tolower(L$label)
    lead_txt <- sprintf("%s %s tracks %s animals%s",
      if (grepl("precip", L$driver)) "A wetter" else "A warmer", sub, dir,
      if (L$lag >= 1) " the next year" else " the same year")
    # env_corr_scan() returns NULL when a site lacks an 8+ month precip overlap
    # (e.g. JORN), so $r is NULL — coerce to a length-1 NA so the is.finite(mr)
    # guard below never sees logical(0) ("argument is of length zero").
    mr <- tryCatch(env_corr_scan(d, rv$env, "precip")$r, error = function(e) NA_real_)
    if (is.null(mr) || length(mr) != 1L) mr <- NA_real_
    pstr <- if (is.finite(L$p)) sprintf("p = %.2f", L$p) else sprintf("%d yrs, too few for a p", L$n)
    div(class = paste("ec ec-seasonal", rail),
      style = "margin-top:14px;",
      eyebrow,
      div(class = "ec-hero",
        div(class = "ec-hero-text", lead_txt, "."),
        div(class = paste("ec-rvalue", if (pos) "ec-sgn-pos" else "ec-sgn-neg"),
          bs_icon(if (pos) "arrow-up-right" else "arrow-down-right"),
          HTML(sprintf("r&nbsp;%+.2f", L$r)))),
      div(class = "ec-foot",
        tags$span(class = "ec-meta", bs_icon("calendar3"), HTML(sprintf("<b>%d</b> years", L$n))),
        tags$span(class = "ec-meta-dot"), tags$span(class = "ec-meta", pstr),
        if (is.finite(L$p_adj)) tagList(tags$span(class = "ec-meta-dot"),
          tags$span(class = "ec-meta", title = "p after accounting for testing several seasons",
                    HTML(sprintf("season-corrected p = %.2f", L$p_adj))))),
      div(class = "ec-seasonal-note",
        if (is.finite(mr))
          HTML(sprintf("The month-to-month overlay shows only about <b>r %+.2f</b> for plain rain. Splitting the year by season recovers the desert driver a single annual total blends away.", mr))
        else "Splitting the year by season recovers the driver a single annual total blends away."),
      div(class = "ec-seasonal-caveat", bs_icon("info-circle"),
        HTML(" One site, a handful of years, so suggestive not settled. The cross-site test that pools many sites is in the "),
        tags$a(href = "https://tgilbert14.github.io/NEON-Driver-Cascade/", target = "_blank", "Driver Cascade app"), "."))
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
    # Only draw a fit line where it's defensible: the panel's own overlap floor
    # (n >= 8 month-matched points) AND a non-trivial relationship (|r| >= 0.3),
    # matching the Size Lab size-mass standard. Below that, a fitted line over a
    # handful of points would imply a relationship the data can't support, so we
    # say so plainly instead.
    rr <- if (!is.null(sc) && !is.null(sc$r) && is.finite(sc$r)) sc$r else
      suppressWarnings(stats::cor(pts$value, pts$cpue, use = "complete.obs"))
    if (nrow(pts) >= 8 && is.finite(rr) && abs(rr) >= 0.3 && stats::sd(pts$value) > 0) {
      fit <- stats::lm(cpue ~ value, data = pts)
      xs <- range(pts$value, na.rm = TRUE)
      yh <- stats::predict(fit, newdata = data.frame(value = xs))
      fitcol <- if (!is.null(sc)) ec_corr_color(es$layer, sc$r, is_dark()) else meta$color
      p <- p %>% plotly::add_trace(x = xs, y = yh, type = "scatter", mode = "lines",
        inherit = FALSE, showlegend = FALSE, hoverinfo = "skip",
        line = list(color = fitcol, width = 2, dash = "dash"))
    } else {
      p <- p %>% plotly::add_annotations(
        text = if (nrow(pts) < 8) "too few month-matched points to fit a line"
               else "no strong enough link to fit a line",
        x = 0.5, y = 1.02, xref = "paper", yref = "paper", xanchor = "center",
        showarrow = FALSE, font = list(color = DDL$muted, size = 12))
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
    # honesty p for the winning (best-|r|) bar: shared circular-shift null (see
    # env_pval). Printed in the caption so the ranking never reads as a verdict
    # when the top bar can't beat chance.
    pv <- env_pval()
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
      plotly_theme(legend = FALSE) %>%
      plotly::layout(
        title = list(text = if (demo) "Demo overlays · illustrative" else "",
                     font = list(size = 11, color = "#9a7a00"), x = 0.02),
        xaxis = list(title = "deseasonalized correlation with catch-per-effort (r) · left of 0 = inverse",
                     range = c(-1, 1), zeroline = TRUE, zerolinecolor = "rgba(31,42,48,0.35)"),
        yaxis = list(title = ""),
        margin = list(b = 84),
        # cohesion + honesty: the bars ARE the switcher, sorted by strength; and
        # they're collinear stages of one seasonal cascade, not independent evidence
        annotations = list(list(
          text = sprintf("tap a bar to overlay that driver below · sorted by strength · best of %d driver%s × ≤13 lags%s<br>left of 0 = inverse (more driver → fewer animals) · bars aren't independent evidence",
                         n_drv, if (n_drv == 1) "" else "s",
                         if (!is.null(pv) && is.finite(pv$p))
                           sprintf(" · top bar shuffle-null p = %.2f%s", pv$p,
                                   if (pv$p >= 0.05) " (apparent — can't beat chance)" else "")
                         else if (!is.null(pv)) " · series too short for a p" else ""),
          x = 0, y = -0.36, xref = "paper", yref = "paper", xanchor = "left", yanchor = "top",
          align = "left", showarrow = FALSE, font = list(size = 10, color = dcol("#8a97a8"))))) %>%
      plotly::event_register("plotly_click")
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
    # plots now read the mode-aware species_pal() reactive; keep rv$pal populated
    # (mode-aware at build) for any non-reactive consumer / back-compat.
    rv$pal   <- make_species_pal(d, dark = is_dark())
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
    # Size Lab scatter filters: species (default all) + plot (default all).
    # Build the choices from the MEASURED subset (individuals with both a weight
    # and a hind-foot) — the same set the scatter plots — so picking any species
    # or plot can never land on an empty chart.
    meas <- lb[is.finite(lb$avg_hf) & is.finite(lb$avg_weight) &
               lb$avg_hf > 0 & lb$avg_weight > 0 & !is.na(lb$scientificName), , drop = FALSE]
    sp_u <- sort(unique(meas$scientificName))
    updateSelectInput(session, "scatterSpecies",
                      choices = c(list("All species" = "all"), as.list(stats::setNames(sp_u, sp_u))),
                      selected = "all")
    pl_u <- sort(unique(meas$home_plot[!is.na(meas$home_plot)]))
    updateSelectInput(session, "scatterPlot",
                      choices = c(list("All plots" = "all"), as.list(stats::setNames(pl_u, pl_u))),
                      selected = "all")
    # Captures-over-time plot picker: every plot with captures (not just home plots),
    # default to the site total.
    tp_u <- sort(unique(rv$data$plotID[!is.na(rv$data$plotID)]))
    updateSelectInput(session, "plotTrendPlot",
                      choices = c(list("All plots (site total)" = "all"), as.list(stats::setNames(tp_u, tp_u))),
                      selected = "all")
    # Carry-state: land on the tab the user came from (stashed by "change site")
    # IF it's a real tab the new site supports; otherwise the Overview default.
    # Dossier is excluded — it needs a selected individual the new site doesn't
    # have yet — so a carried dossier falls back to overview.
    site_code <- mode_chr(d$siteID)
    land <- rv$pendingTab
    rv$pendingTab <- NULL
    valid_tabs <- c("overview", "population", "community", "sizelab",
                    "homerange", "map", "fame", "search", "about")
    if (is.null(land) || !land %in% valid_tabs) land <- "overview"
    nav_select("tabs", land)
    # restore the leaderboard category + env layer the user had, where the new
    # site can honor them (env layer only if this site offers it).
    if (!is.null(rv$pendingLeaderCat) && !is_demo) {
      updateRadioButtons(session, "leaderCat", selected = rv$pendingLeaderCat)
    }
    rv$pendingLeaderCat <- NULL
    if (!is.null(rv$pendingEnvLayer) && rv$pendingEnvLayer != "none" &&
        rv$pendingEnvLayer %in% env_layer_choices(rv$env)) {
      updateSelectInput(session, "envLayer", selected = rv$pendingEnvLayer)
    }
    rv$pendingEnvLayer <- NULL
    session$sendCustomMessage("countUp", list())
    session$sendCustomMessage("loadDone", list())   # hide the loading overlay
    # delighters: deep-linkable URL + persist last-site / recents. Only here, on a
    # real successful load, and only for a true site (never the demo path which
    # carries no site+window key). updateQueryString fires on load, not per tick.
    # delighters: deep-linkable URL + persist last-site / recents. Only here, on a
    # real successful load, and only for a true site (never the demo path). The
    # localStorage write goes through shinyjs::runjs (a direct client-side eval)
    # rather than a custom-message handler, so the ring update + the live input
    # refresh happen in one reliable client call. updateQueryString fires on load,
    # not per reactive tick.
    if (!is_demo && !is.null(site_code) && nzchar(site_code)) {
      updateQueryString(paste0("?site=", site_code), mode = "replace")
      shinyjs::runjs(sprintf("if(window.smtSaveSite) smtSaveSite(%s);",
                             jsonlite::toJSON(site_code, auto_unbox = TRUE)))
    }
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
            "No captures at %s in %s–%s, so showing its full bundled record instead.",
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
        if (prov) "Provisional/live data isn't available in this build. Uncheck it to use the offline bundle."
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

  # ---- national site-picker map: click a site -> load its record -----------
  # Raises the loading overlay from the server (the map has no inline onclick),
  # and syncs the landing select panel (stateSel/site/dateRange) through the
  # cascade. The date window comes from the landing dateRangeInput when the user
  # has set a valid one; otherwise we fall back to the site's full record
  # (year_min -> today), the friendliest default from a fresh map tap.
  load_site_full <- function(code) {
    if (is.null(code) || code == "") return(invisible())
    row <- if (!is.null(SITE_INDEX)) SITE_INDEX[SITE_INDEX$site == code, ] else NULL
    nm  <- if (!is.null(row) && nrow(row)) row$name[1] else code
    y1  <- if (!is.null(row) && nrow(row) && !is.na(row$year_min[1])) row$year_min[1] else 2013L
    # honour the landing date range when both ends are valid Dates; else use the
    # site's full bundled span so a first map tap loads everything.
    dr <- input$dateRange
    have_dr <- !is.null(dr) && length(dr) == 2 &&
      inherits(dr[1], "Date") && inherits(dr[2], "Date") &&
      !is.na(dr[1]) && !is.na(dr[2]) && dr[1] <= dr[2]
    if (have_dr) { s0 <- dr[1]; e0 <- dr[2] }
    else { s0 <- as.Date(sprintf("%d-01-01", y1)); e0 <- Sys.Date() }
    st  <- if (!is.null(row) && nrow(row)) row$state[1] else NULL
    if (!is.null(st) && !is.na(st)) { rv$pendingSite <- code; updateSelectInput(session, "stateSel", selected = st) }
    # keep the landing control in sync with the window we actually loaded
    updateDateRangeInput(session, "dateRange", start = s0, end = e0)
    session$sendCustomMessage("smtLoadStart", list(label = sprintf("%s · %s", code, nm)))
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
    # detection completeness: one understated labelled number, present only where
    # this site has an estimable closed-capture bout (mean_p), so a raw cross-site
    # count is read against how well the traps actually detect here (~0.57 closed-
    # canopy temperate vs ~0.95 deserts). Absent — not "—" — where unavailable.
    det_line <- if (!is.na(row$mean_p[1]))
      sprintf("<div class='pm-pop-det' title='Mean per-night detection probability across this site&rsquo;s estimable closed-capture bouts. See the Population tab&rsquo;s detection card.'>detection ~%d%%/night</div>",
              round(100 * row$mean_p[1])) else ""
    yrs <- if (!is.na(row$year_min[1]) && !is.na(row$year_max[1]))
      sprintf("<div class='sp-years'>Sampled %d&ndash;%d</div>", row$year_min[1], row$year_max[1]) else ""
    htmltools::HTML(sprintf(
      "<div class='pm-pop site-pop'>
         <div class='pm-pop-t'>%s %s <span class='sp-code'>(%s)</span></div>
         <div class='pm-pop-s'>%s</div>
         <div class='pm-pop-n'><b>%s</b> captures &middot; <b>%s</b> individuals &middot; <b>%s</b> species</div>
         %s%s%s
         <div class='sp-actions'>
           <button type='button' class='sp-btn sp-go' onclick=\"smtLoadStart('%s \\u2014 loading\\u2026');Shiny.setInputValue('siteExplore','%s',{priority:'event'});\">Explore this site &rarr;</button>
           <button type='button' class='sp-btn sp-info' onclick=\"Shiny.setInputValue('siteInfo','%s',{priority:'event'});\">About this site</button>
         </div>
       </div>",
      row$emoji[1], row$name[1], code, where,
      format(row$captures[1], big.mark = ","), format(row$individuals[1], big.mark = ","),
      row$species[1], sp_line, det_line, yrs, row$name[1], code, code))
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
        if (!is.na(row$top_caps[1])) sprintf(" · %s captures", format(row$top_caps[1], big.mark = ",")) else ""))
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
          onclick = sprintf("smtLoadStart('%s \\u00b7 loading\\u2026');Shiny.setInputValue('siteExplore','%s',{priority:'event'});",
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
          div(class = "si-row si-star", "Most caught: ", star),
          # detection completeness — present only where an estimable closed-capture
          # bout exists. Qualifies the raw counts above (deserts ~0.95, closed-
          # canopy temperate ~0.57), with the detail on hover, not always-on text.
          if (!is.na(row$mean_p[1]))
            div(class = "si-row si-det",
              title = "Mean per-night detection probability across this site’s estimable closed-capture bouts. Full breakdown on the Population tab’s detection card.",
              bs_icon("incognito"), HTML(sprintf(" Traps detect ~<b>%d%%</b> of animals present per night",
                round(100 * row$mean_p[1]))))),
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

  # ---- delighters: recents strip ------------------------------------------
  # Tap-chips for the last few sites the visitor viewed, read from localStorage
  # (input$smtRecents, a JSON array set on shiny:connected). Each chip routes
  # through the SAME load_site_full() as the map/list, so behaviour is identical.
  # Zero-effort by design: no favourites, no edit UI — just the ring buffer.
  recents_codes <- reactive({
    raw <- input$smtRecents
    if (is.null(raw) || !nzchar(raw)) return(character(0))
    codes <- tryCatch(jsonlite::fromJSON(raw), error = function(e) NULL)
    if (is.null(codes) || !length(codes)) return(character(0))
    codes <- as.character(codes)
    valid <- if (!is.null(SITE_INDEX)) SITE_INDEX$site else codes
    codes <- codes[codes %in% valid]
    utils::head(codes, 4)
  })
  output$recentsStrip <- renderUI({
    codes <- recents_codes()
    if (!length(codes)) return(NULL)
    chips <- lapply(codes, function(code) {
      row <- if (!is.null(SITE_INDEX)) SITE_INDEX[SITE_INDEX$site == code, ] else NULL
      nm  <- if (!is.null(row) && nrow(row)) row$name[1] else code
      actionLink(paste0("recent_", code),
        tagList(span(class = "rc-code", code)),
        class = "recent-chip", title = nm,
        onclick = sprintf(
          "smtLoadStart('%s \\u00b7 loading\\u2026');Shiny.setInputValue('recentPick','%s',{priority:'event'});",
          gsub("'", "\\\\'", nm), code))
    })
    div(class = "recents-strip",
      tags$span(class = "recents-label", bs_icon("clock-history"), " Recently viewed:"),
      div(class = "recents-chips", chips))
  })
  observeEvent(input$recentPick, load_site_full(input$recentPick))

  # ===========================================================================
  # Search the network — query the bundled search index (instant, no fetch)
  # ===========================================================================
  # Populate the species autocomplete once (server-side selectize so the 145
  # species don't all ship in the page).
  updateSelectizeInput(session, "searchTaxon",
    choices = search_taxon_choices(), selected = "", server = TRUE)

  # one shared "Go to this site" button -> load that site's bundle + jump to
  # Overview. Reuses the exact map-click path (load_site_full), so the sidebar
  # state/site selectors sync and the load is identical everywhere.
  search_go_btn <- function(code)
    sprintf("<button type='button' class='sp-btn sp-go search-go' onclick=\"Shiny.setInputValue('searchGoSite','%s',{priority:'event'});\">Go to this site &rarr;</button>", code)
  observeEvent(input$searchGoSite, {
    code <- input$searchGoSite
    if (is.null(code) || code == "") return()
    nav_select("tabs", "overview")     # land on the site's Overview
    load_site_full(code)               # loads the bundle + syncs the sidebar
  })

  n_sites_total <- if (!is.null(SEARCH_INDEX)) length(unique(SEARCH_INDEX$site)) else 0L

  # (a) FIND A SPECIES --------------------------------------------------------
  search_taxon_rows <- reactive({
    sci <- input$searchTaxon
    if (is.null(SEARCH_INDEX) || is.null(sci) || sci == "") return(NULL)
    r <- SEARCH_INDEX[SEARCH_INDEX$scientificName == sci, , drop = FALSE]
    if (nrow(r) == 0) return(r)
    r[order(-r$mnka, -r$individuals), , drop = FALSE]
  })

  output$searchTaxonCaption <- renderUI({
    sci <- input$searchTaxon
    if (is.null(sci) || sci == "") {
      return(div(class = "search-cap muted",
        bsicons::bs_icon("hand-index"),
        " Pick a species to see every NEON site where it was trapped."))
    }
    r <- search_taxon_rows()
    n <- if (is.null(r)) 0L else nrow(r)
    if (n == 0) return(div(class = "search-cap muted",
      bsicons::bs_icon("search"), sprintf(" No bundled site has %s recorded.", sci)))
    div(class = "search-cap",
      bsicons::bs_icon("geo-alt-fill"),
      HTML(sprintf(" <b>%s</b> trapped at <b>%d</b> of %d sites. ", sci, n, n_sites_total)),
      tags$span(class = "muted",
        "The number is MNKA, a within-site index of how many were known alive at once, not a population and not a cross-site ranking."))
  })

  output$searchTaxonTbl <- DT::renderDT({
    r <- search_taxon_rows()
    if (is.null(r) || nrow(r) == 0) {
      empty <- data.frame(Site = character(0), State = character(0),
                          `Peak MNKA` = integer(0), Individuals = integer(0),
                          Captures = integer(0), Years = character(0),
                          ` ` = character(0), check.names = FALSE)
      return(DT::datatable(empty, escape = FALSE, rownames = FALSE,
                           options = list(dom = "t", language = list(emptyTable =
                             "Pick a species above to see where it was trapped.")))) }
    yrs <- ifelse(is.na(r$year_min) | is.na(r$year_max), "—",
                  ifelse(r$year_min == r$year_max, as.character(r$year_min),
                         sprintf("%d–%d", r$year_min, r$year_max)))
    df <- data.frame(
      Site        = sprintf("%s (%s)", r$name, r$site),
      State       = ifelse(is.na(r$state), "—", r$state),
      `Peak MNKA` = r$mnka,
      Individuals = r$individuals,
      Captures    = r$captures,
      Years       = yrs,
      ` `         = vapply(r$site, search_go_btn, character(1)),
      check.names = FALSE, stringsAsFactors = FALSE)
    DT::datatable(df, escape = FALSE, rownames = FALSE, selection = "none",
      options = list(pageLength = 15, dom = "tip", scrollX = TRUE,
                     order = list(list(2, "desc")),
                     columnDefs = list(list(orderable = FALSE, targets = 6))))
  })

  # (b) FIND SITES (threshold query) -----------------------------------------
  # widespread: species recorded at > N sites; richness: sites with > X species.
  search_thresh_rows <- reactive({
    if (is.null(SEARCH_INDEX)) return(NULL)
    kind <- input$threshKind %||% "widespread"
    if (kind == "widespread") {
      N <- as.integer(input$threshN %||% 5)
      tab <- SEARCH_INDEX %>% dplyr::group_by(.data$scientificName, .data$emoji,
                                              .data$group_label, .data$nickname) %>%
        dplyr::summarise(sites = dplyr::n(),
                         individuals = sum(.data$individuals),
                         peak_mnka = max(.data$mnka), .groups = "drop") %>%
        dplyr::filter(.data$sites > N) %>%
        dplyr::arrange(dplyr::desc(.data$sites), dplyr::desc(.data$individuals))
      list(kind = "widespread", N = N, data = tab)
    } else {
      X <- as.integer(input$threshX %||% 10)
      tab <- SEARCH_INDEX %>% dplyr::group_by(.data$site, .data$name,
                                              .data$state, .data$domain) %>%
        dplyr::summarise(species = dplyr::n_distinct(.data$scientificName),
                         individuals = sum(.data$individuals), .groups = "drop") %>%
        dplyr::filter(.data$species > X) %>%
        dplyr::arrange(dplyr::desc(.data$species), dplyr::desc(.data$individuals))
      list(kind = "richness", X = X, data = tab)
    }
  })

  output$searchThreshCaption <- renderUI({
    q <- search_thresh_rows()
    if (is.null(q)) return(NULL)
    n <- nrow(q$data)
    if (q$kind == "widespread") {
      if (n == 0) return(div(class = "search-cap muted", bsicons::bs_icon("search"),
        sprintf(" No species is recorded at more than %d sites.", q$N)))
      div(class = "search-cap", bsicons::bs_icon("diagram-3-fill"),
        HTML(sprintf(" <b>%d</b> species recorded at more than <b>%d</b> sites.", n, q$N)),
        tags$span(class = "muted", " Pick a species to see its sites in Find a species."))
    } else {
      if (n == 0) return(div(class = "search-cap muted", bsicons::bs_icon("search"),
        sprintf(" No site has recorded more than %d species.", q$X)))
      div(class = "search-cap", bsicons::bs_icon("geo-alt-fill"),
        HTML(sprintf(" <b>%d</b> of %d sites have recorded more than <b>%d</b> species.",
                     n, n_sites_total, q$X)),
        tags$span(class = "muted",
          " Species counts use confirmed species-level IDs only and reflect trapping effort, so they are not a fair richness ranking."))
    }
  })

  output$searchThreshTbl <- DT::renderDT({
    q <- search_thresh_rows()
    if (is.null(q) || nrow(q$data) == 0) {
      return(DT::datatable(data.frame(` ` = "No matches at this threshold.", check.names = FALSE),
                           escape = FALSE, rownames = FALSE, colnames = "",
                           options = list(dom = "t"))) }
    if (q$kind == "widespread") {
      d <- q$data
      df <- data.frame(
        Species     = ifelse(is.na(d$nickname),
                             sprintf("%s <i>%s</i>", d$emoji, d$scientificName),
                             sprintf("%s <i>%s</i> (%s)", d$emoji, d$scientificName, d$nickname)),
        Group       = d$group_label,
        Sites       = d$sites,
        Individuals = d$individuals,
        `Peak MNKA` = d$peak_mnka,
        check.names = FALSE, stringsAsFactors = FALSE)
      DT::datatable(df, escape = FALSE, rownames = FALSE, selection = "none",
        options = list(pageLength = 15, dom = "tip", scrollX = TRUE, order = list(list(2, "desc"))))
    } else {
      d <- q$data
      df <- data.frame(
        Site        = sprintf("%s (%s)", d$name, d$site),
        State       = ifelse(is.na(d$state), "—", d$state),
        Species     = d$species,
        Individuals = d$individuals,
        ` `         = vapply(d$site, search_go_btn, character(1)),
        check.names = FALSE, stringsAsFactors = FALSE)
      DT::datatable(df, escape = FALSE, rownames = FALSE, selection = "none",
        options = list(pageLength = 15, dom = "tip", scrollX = TRUE, order = list(list(2, "desc")),
                       columnDefs = list(list(orderable = FALSE, targets = 4))))
    }
  })

  # "Change site" (in the hero band) -> back to the picker-map landing
  observeEvent(input$changeSite, {
    # carry-state: stash the view the user is leaving so the NEXT site lands back
    # on the same tab / leaderboard category / env layer (ingest honours these
    # where the new site supports them). The dossier tab is intentionally NOT
    # carried — it needs a selected individual — so a leaving-dossier user is
    # returned to overview by ingest's valid_tabs gate.
    rv$pendingTab       <- input$tabs %||% NULL
    rv$pendingLeaderCat <- input$leaderCat %||% NULL
    rv$pendingEnvLayer  <- input$envLayer %||% NULL
    rv$data <- NULL; rv$lb <- NULL; rv$lb_view <- NULL; rv$tag <- NULL; rv$label <- NULL; rv$env <- NULL
    shinyjs::hide("mainTabsWrap"); shinyjs::hide("indivPickerWrap"); shinyjs::hide("envPickerWrap"); shinyjs::show("splash")
    # clear the deep link so the splash carries no stale ?site= in the address bar
    updateQueryString("?", mode = "replace")
    # the picker map was hidden while a site was loaded; nudge it to recompute
    # size now that it's visible again, so it never paints blank/grey on return
    session$sendCustomMessage("kickMaps", list())
  })

  # (v2 flow: the Jornada demo path was removed — users pick a real site on the
  #  map, the Browse-all-sites list, or the by-name select panel. The demoBtn /
  #  demoBtn2 inputs and their observers are gone with it.)

  # ---- selecting an individual -------------------------------------------
  pick_individual <- function(tag, navigate = TRUE) {
    if (is.null(tag) || is.na(tag) || tag == "") return()
    rv$tag <- tag
    # keep BOTH pickers (sidebar + the inline Home-Range one) in lockstep
    if (!identical(input$indiv, tag))   updateSelectizeInput(session, "indiv",   selected = tag)
    if (!identical(input$indivHR, tag)) updateSelectizeInput(session, "indivHR", selected = tag)
    # Confetti is a "you arrived at a star's dossier" celebration — only on an
    # active navigate (sidebar / leaderboard / dossier), NOT on the quiet in-place
    # picks (Size Lab QC chip, Home-Range inline picker), which set navigate=FALSE.
    if (navigate) {
      nav_select("tabs", "dossier")
      row <- rv$lb[rv$lb$tagID == tag, ]
      if (nrow(row) && row$rarity[1] %in% c("Epic", "Legendary"))
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

  # Size Lab: tapping "Open QC history card" on a pinned scatter card selects
  # that individual (without leaving the tab) and scrolls its QC card into view.
  observeEvent(input$qcCardRequest, {
    tag <- input$qcCardRequest
    if (is.null(tag) || !nzchar(tag)) return()
    pick_individual(tag, navigate = FALSE)
    session$sendCustomMessage("smtRevealQc", list())
  }, ignoreInit = TRUE)

  # ---- Size Lab table twin (WCAG 2.1.1 keyboard/AT path to the crown jewel) ----
  # plotly's SVG dots aren't focusable and the modebar is off, so tap-to-pin is
  # pointer-only. "View the numbers" swaps in a DT of the SAME prepped frame the
  # scatter draws; a row click opens the same QC history card as a dot's pin chip.
  bodyTbl_rv <- reactiveVal(NULL)
  bodyTblShown <- reactiveVal(FALSE)
  observeEvent(input$bodyTblToggle, {
    show <- !isTRUE(bodyTblShown())
    bodyTblShown(show)
    shinyjs::toggle("bodyTblWrap", condition = show)
    # keep the toggle's state programmatically determinable + rename it (WCAG 4.1.2)
    shinyjs::runjs(sprintf(paste0(
      "var b=document.getElementById('bodyTblToggle'); if(b){",
      "b.setAttribute('aria-pressed','%s');",
      "var t=Array.prototype.filter.call(b.childNodes,function(n){return n.nodeType===3;})[0];",
      "if(t){t.nodeValue=' %s';}}"),
      tolower(as.character(show)),
      if (show) "Hide the numbers" else "View the numbers"))
  })
  output$bodyScatterTbl <- DT::renderDT({
    df <- bodyTbl_rv()
    validate(need(!is.null(df) && nrow(df) > 0, "No individuals to list for these filters."))
    df <- df[order(-df$captures, df$scientificName), , drop = FALSE]
    show <- data.frame(
      ID       = ifelse(is.na(df$emoji), "", paste0(df$emoji, " ")),
      stringsAsFactors = FALSE)
    show$ID       <- paste0(show$ID, df$short)
    show$Species  <- df$scientificName
    show$Stage    <- df$lifeStage %||% NA
    if ("home_plot" %in% names(df)) show$Plot <- df$home_plot
    show$`Hind-foot (mm)` <- round(df$avg_hf, 1)
    show$`Weight (g)`     <- round(df$avg_weight, 1)
    show$Captures <- df$captures
    show$`Career (d)` <- df$career_days
    if ("chonk_pct" %in% names(df))
      show$`Chonk %ile` <- ifelse(is.na(df$chonk_pct), NA, round(df$chonk_pct))
    show$Rarity   <- df$rarity
    DT::datatable(show, rownames = FALSE, selection = "single",
      class = "compact stripe hover",
      caption = htmltools::tags$caption(
        style = "caption-side:top;text-align:left;color:var(--muted);font-size:12px;",
        sprintf("%d individuals · the same data the map plots · select a row to open its QC card.",
                nrow(show))),
      options = list(pageLength = 15, dom = "ftip", scrollX = TRUE,
                     order = list(list(which(names(show) == "Captures") - 1L, "desc"))))
  })
  # a selected row -> open that individual's QC history card (same path as a pin chip)
  observeEvent(input$bodyScatterTbl_rows_selected, {
    df <- bodyTbl_rv(); sel <- input$bodyScatterTbl_rows_selected
    if (is.null(df) || is.null(sel) || !length(sel)) return()
    ord <- df[order(-df$captures, df$scientificName), , drop = FALSE]
    tag <- ord$tagID[sel[1]]
    if (!is.null(tag) && nzchar(tag)) {
      pick_individual(tag, navigate = FALSE)
      session$sendCustomMessage("smtRevealQc", list())
    }
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
  observeEvent(input$goSizeLab,    nav_select("tabs", "sizelab"))
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
    map <- leaflet::addCircleMarkers(map, data = idx, lng = ~lng, lat = ~lat, layerId = ~site,
      radius = picker_radius(idx$captures), stroke = TRUE, color = "#ffffff", weight = 1.5,
      opacity = 1, fillColor = ~group_color, fillOpacity = 0.85, label = labs,
      popup = pops, popupOptions = leaflet::popupOptions(maxWidth = 300, minWidth = 230,
        autoPan = TRUE, autoPanPadding = c(40, 55), keepInView = TRUE,
        closeButton = TRUE, closeOnClick = FALSE, className = "pm-pop-card"),
      labelOptions = picker_label_opts, options = leaflet::markerOptions(riseOnHover = TRUE))
    # Family is also encoded by the dominant-family EMOJI sitting on each dot, so
    # the picker is readable without relying on a 6-hue fill (colour-vision-safe).
    picker_emoji_markers(map, idx)
  }

  # Render the dominant-family emoji as a permanent, non-interactive label centred
  # on each picker dot. Click/hover pass THROUGH to the circle marker underneath
  # (interactive = FALSE), so the emoji never steals the popup. Tiny dots are
  # skipped so the glyph never overflows a small marker.
  picker_emoji_markers <- function(map, df) {
    df <- df[!is.na(df$emoji) & nzchar(df$emoji) &
             picker_radius(df$captures) >= 9, , drop = FALSE]
    if (!nrow(df)) return(map)
    leaflet::addLabelOnlyMarkers(map, data = df, lng = ~lng, lat = ~lat,
      label = ~emoji,
      labelOptions = leaflet::labelOptions(noHide = TRUE, direction = "center",
        textOnly = TRUE, opacity = 1,
        style = list("font-size" = "13px", "pointer-events" = "none",
                     "text-shadow" = "0 1px 2px rgba(0,0,0,.45)")),
      options = leaflet::markerOptions(interactive = FALSE))
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
    map <- leaflet::addCircleMarkers(map, data = r, lng = ~lng, lat = ~lat, layerId = ~site,
      radius = picker_radius(r$individuals), stroke = TRUE, color = "#ffffff", weight = 1.5,
      opacity = 1, fillColor = col, fillOpacity = 0.85, label = labs,
      popup = pops, popupOptions = leaflet::popupOptions(maxWidth = 300, minWidth = 230,
        autoPan = TRUE, autoPanPadding = c(40, 55), keepInView = TRUE,
        closeButton = TRUE, closeOnClick = FALSE, className = "pm-pop-card"),
      labelOptions = picker_label_opts, options = leaflet::markerOptions(riseOnHover = TRUE))
    # same emoji-on-dot treatment as the by-site map (sized by `individuals` here)
    picker_emoji_markers(map, data.frame(lng = r$lng, lat = r$lat, emoji = r$emoji,
                                         captures = r$individuals, stringsAsFactors = FALSE))
  }

  # base map drawn once (tiles + view + initial by-site markers)
  output$pickerMap <- renderLeaflet({
    # Show a clear notice (not an endless spinner) if the site index didn't load.
    # `req()` here would silently halt and spin forever; `validate()` surfaces the
    # cause in-place so a missing/unreadable data/site_index.rds is diagnosable.
    validate(need(
      !is.null(SITE_INDEX) && nrow(SITE_INDEX) > 0,
      "The national site map couldn't load its data (data/site_index.rds is missing or unreadable in this deployment). The rest of the app still works, so pick a site, or try the demo."
    ))
    leaflet(options = leafletOptions(minZoom = 2, worldCopyJump = TRUE)) %>%
      add_suite_basemap("CartoDB.Positron", noWrap = TRUE) %>%
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

  # ---- compare two sites (modal) -----------------------------------------
  compare_site_choices <- function() {
    idx <- SITE_INDEX
    if (is.null(idx)) return(setNames(neon_sites$site, neon_sites$site))
    o <- idx[order(idx$name), ]
    setNames(o$site, sprintf("%s · %s, %s", o$site, o$name, o$state))
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
      spin(uiOutput("compareOut"), img = "rat1.gif"),
      downloadButton("compareReport",
        tagList(bs_icon("file-earmark-arrow-down"), " Download comparison report (PDF)"),
        class = "btn-outline-dark w-100 cmp-dl")
    ))
  })

  # side-by-side comparison report (PDF) for the two picked sites — reads the
  # live cmpA/cmpB selection, so it works whether or not Compare was pressed.
  output$compareReport <- downloadHandler(
    filename = function() sprintf("NEON-compare_%s-vs-%s.pdf", input$cmpA %||% "A", input$cmpB %||% "B"),
    content = function(file) {
      a <- input$cmpA; b <- input$cmpB
      validate(need(!is.null(a) && !is.null(b) && !identical(a, b), "Pick two different sites to compare."))
      ba <- load_site_bundle(a); bb <- load_site_bundle(b)
      validate(need(!is.null(ba) && !is.null(bb), "Both sites must be in the offline bundle."))
      render_compare_pdf(file, clean_mam(ba), site_label(a), clean_mam(bb), site_label(b))
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
    # detection-corrected read so the cross-site comparison carries the
    # correction the app computes everywhere else: per-site mean p̂ (how well
    # this biome's traps catch what's present) + the mean monthly N̂ across
    # estimable bouts. The 2x p̂ spread (HARV ~0.57 temperate vs JORN ~0.95
    # desert) is exactly what makes a raw cross-site count a detection statement,
    # not a density one — so we surface it AND gate the winner-highlight on it.
    cc <- tryCatch(closed_capture_series(d), error = function(e) NULL)
    nhat_pm <- if (!is.null(cc) && !is.null(cc$series) && nrow(cc$series) > 0)
                 round(mean(cc$series$N, na.rm = TRUE)) else NA_real_
    res <- list(site = site, label = site_label(site), cs = cs, hn = hn, sp = sp,
                p_hat = if (!is.null(cc)) cc$mean_p else NA_real_,
                detect = if (!is.null(cc)) cc$mean_detect else NA_real_,
                nhat_pm = nhat_pm,
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

    # Detection differs materially between these two sites? A >= ~1.25x spread in
    # mean p̂ (or one site un-estimable) means a raw count head-to-head is mostly a
    # trapability statement — so we SUPPRESS the winner-highlight on the raw
    # detection-sensitive rows (captures / individuals / trap-nights) and mark
    # them so the green never invites "more captures = more animals" across biomes.
    pa_p <- pa$p_hat; pb_p <- pb$p_hat
    det_mismatch <- is.na(pa_p) || is.na(pb_p) ||
      (max(pa_p, pb_p) / max(min(pa_p, pb_p), 1e-9)) >= 1.25

    # winner-aware metric row: higher value gets a subtle highlight. `gate=TRUE`
    # rows (raw counts) drop the highlight when detection differs materially, and
    # carry a clickable ⓘ that opens the detection-vs-abundance explainer modal.
    row <- function(lab, va, vb, fmt = function(x) format(x, big.mark = ","),
                    higher = TRUE, tip = NULL, gate = FALSE) {
      suppress <- gate && det_mismatch
      hl <- if (suppress || is.na(va) || is.na(vb) || va == vb) c("", "")
            else if ((va > vb) == higher) c("cmp-win", "") else c("", "cmp-win")
      afford <- if (suppress)
        tags$span(class = "cmp-det-flag", onclick = "Shiny.setInputValue('cmpDetWhy', Math.random(), {priority:'event'})",
                  title = "Reflects detection as well as abundance. Why isn't this a winner?",
                  bs_icon("info-circle"))
      else if (!is.null(tip)) info_pop(lab, p(tip))
      tags$tr(
        tags$td(class = "cmp-lab", lab, afford),
        tags$td(class = paste("cmp-val", hl[1]), fmt(va)),
        tags$td(class = paste("cmp-val", hl[2]), fmt(vb)))
    }
    pfmt <- function(x) if (is.na(x)) "—" else format(round(x, 2), nsmall = 2)
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
          row("Captures", pa$cs$total_captures, pb$cs$total_captures, gate = TRUE),
          row("Individuals", pa$cs$individuals, pb$cs$individuals, gate = TRUE),
          row("Species (richness)", pa$cs$species, pb$cs$species),
          row("Effective common species", pa$hn$q1, pb$hn$q1, fmt = function(x) format(x, nsmall = 1),
              tip = "Hill q1 = exp(Shannon): the effective number of common species. Higher = more diverse."),
          row("Evenness (0–1)", pa$hn$even, pb$hn$even, fmt = function(x) format(x, nsmall = 2),
              tip = "How evenly captures spread across species. Near 1 = even; low = a few species dominate."),
          row("Recapture rate", pa$cs$recap_rate, pb$cs$recap_rate, fmt = function(x) paste0(x, "%")),
          row("Trap-nights (effort)", pa$cs$trap_nights, pb$cs$trap_nights, gate = TRUE),
          # the detection-corrected read — the apples-to-apples cross-site numbers:
          # how completely each biome's traps catch what's present, and the mean
          # detection-corrected monthly abundance. These carry the correction the
          # raw counts above do not.
          row("Detection p̂ (per night)", pa$p_hat, pb$p_hat, fmt = pfmt,
              tip = "Mean per-night capture probability from the closed-capture (Schnabel/Chapman) estimates. Deserts run high (~0.6+); closed-canopy temperate sites run low, so a raw count undercounts temperate sites worse."),
          row("Mean N̂ per month", pa$nhat_pm, pb$nhat_pm, fmt = function(x) if (is.na(x)) "—" else format(x, big.mark = ","),
              tip = "Mean detection-corrected abundance per month across estimable (multi-night) bouts: the cross-biome comparison the raw captures can't make honestly."))),
      div(class = "compare-species",
        div(class = "cmp-col", div(class = "cmp-col-h", "Top species · ", pa$site), sp_list(pa)),
        div(class = "cmp-col", div(class = "cmp-col-h", "Top species · ", pb$site), sp_list(pb))),
      div(class = "compare-foot", bs_icon("info-circle"),
        " Higher value highlighted per row. Diversity uses Hill numbers over distinct individuals; richness is the raw species count.",
        if (det_mismatch) tagList(" These two sites' detection differs",
          tags$a(href = "#", class = "cmp-why",
                 onclick = "Shiny.setInputValue('cmpDetWhy', Math.random(), {priority:'event'}); return false;",
                 " · why raw counts aren't compared", bs_icon("info-circle"))))
    )
  })

  # detection-vs-abundance explainer — opened from the ⓘ on a raw-count row or the
  # compare footer. Reuses the app's modalDialog chrome (no always-on wall of text).
  observeEvent(input$cmpDetWhy, {
    showModal(modalDialog(
      title = tagList(bs_icon("incognito"), " Detection is not abundance"),
      easyClose = TRUE, size = "m", footer = modalButton("Got it"),
      div(class = "rank-modal-sub",
        "Raw captures, individuals, and trap-nights count what the traps caught, which depends on how ",
        tags$b("detectable"), " the animals are, not just how many there are."),
      tags$p("Detection completeness swings about 2x across biomes: closed-canopy temperate sites (e.g. HARV) catch only ",
        tags$b("~57%"), " of the animals present per bout (mean p̂ ≈ 0.30), while open deserts (e.g. JORN) catch ",
        tags$b("~95%"), " (mean p̂ ≈ 0.63). So an uncorrected count undercounts temperate sites far worse than deserts."),
      tags$p("That's why the green winner-highlight is ", tags$b("suppressed"),
        " on captures / individuals / trap-nights when these two sites' detection differs materially: “more captures” there would be a trapability statement, not an abundance one."),
      tags$p(class = "rank-modal-insight", style = "margin-top:10px",
        bs_icon("lightbulb"), " For an apples-to-apples cross-biome read, use the ",
        tags$b("Detection p̂"), " and ", tags$b("Mean N̂ per month"),
        " rows below: those carry the closed-capture correction this app computes everywhere else.")
    ))
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
        bs_icon("broadcast-pin"), span(class = "hero-site-label", `data-site` = mode_chr(d$siteID), rv$label),
        if (isTRUE(rv$is_demo)) span(class = "demo-pill", bs_icon("stars"), " DEMO"),
        span(class = "hero-site-range", fmt_range(cs$date_min, cs$date_max)),
        actionLink("changeSite", tagList(bs_icon("arrow-left-circle"), " change site"),
                   class = "hero-change"),
        downloadLink("reportPdf", tagList(bs_icon("file-earmark-arrow-down"), " report card"),
                     class = "hero-report"),
        span(class = "hero-site-hint", bs_icon("hand-index"), " tap any stat for the full ranking")),
      div(class = "stat-grid",
        vb(cs$total_captures, "Captures",      "bullseye",        "#38a8e8", "captures",
           tip = "Total times an animal was caught & handled. Click to see captures by plot."),
        vb(cs$individuals,    "Individuals",   "fingerprint",     "#43b8e8", "individuals",
           tip = "Distinct animals (unique ear-tag IDs). Click for the most-caught individuals."),
        vb(cs$species,        "Species",       "diagram-3-fill",  "#5fb56a", "species",
           tip = "Distinct species identified. Click for species ranked by abundance."),
        vb(cs$recap_rate,     "Recapture rate","arrow-repeat",    "#138086", "recapture", pct = TRUE,
           tip = "Share of captures that were re-encounters. Click for recapture rate by species."),
        vb(cs$trap_nights,    "Trap-nights",   "moon-stars-fill", "#5b3a8a", "trapnights",
           tip = "Total trapping effort. Click for effort & catch-rate by plot."),
        vb(cs$legendary,      "Legends (10+)", "trophy-fill",     "#e0b43a", "legends",
           tip = "Individuals caught 10+ times. Click for the full list of legends.")
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
    # species_summary is sorted by CAPTURES; pick the row with the most
    # INDIVIDUALS, since the banner's claim is a share "of individuals"
    # (in a re-trap-heavy system most-trapped != most-abundant).
    dom <- sp[which.max(sp$individuals), ]
    share <- if (tot > 0) round(100 * dom$individuals / tot) else 0
    insight_banner("collection", tone = "navy",
      HTML(sprintf("The <b><i>%s</i></b> is the most abundant here: <span class='ci-hero'>%s%%</span> of individuals (%s of %s).",
        dom$scientificName, share, fmt_int(dom$individuals), fmt_int(tot))))
  })

  output$hillInsight <- renderUI({
    d <- rv$data; req(d)
    hn <- hill_numbers(d); if (hn$n_sp == 0) return(NULL)
    if (is.na(hn$even)) return(insight_banner("diagram-3-fill", tone = "muted",
      HTML(sprintf("<span class='ci-hero'>%s</span> species recorded at this site.", hn$n_sp))))
    verdict <- if (hn$even >= 0.75) "an even community"
      else if (hn$even >= 0.5) "a moderately even community"
      else if (hn$even >= 0.3) "an uneven community, a few species dominate"
      else "a highly skewed community, one or two species dominate"
    insight_banner("diagram-3-fill", tone = if (hn$even >= 0.5) "pine" else "gold",
      HTML(sprintf("This is <b>%s</b>: <span class='ci-hero'>%s</span> species seen, but only about <b>%s</b> are common.",
        verdict, hn$n_sp, format(hn$q1, nsmall = 1))))
  })

  output$accumInsight <- renderUI({
    d <- rv$data; req(d)
    sa <- tryCatch(species_accum(d), error = function(e) NULL); if (is.null(sa)) return(NULL)
    complete <- sa$sobs >= 0.85 * sa$chao1
    est <- if (isTRUE(sa$unstable)) sprintf("at least <b>%s</b> (only %d species seen twice, a soft floor)", sa$chao1, sa$f2)
           else sprintf("<span class='ci-hero'>%s</span> (95%% CI %s–%s)", sa$chao1, sa$chao_lo, sa$chao_hi)
    insight_banner("graph-up", tone = if (complete) "pine" else "gold",
      HTML(sprintf("Found <span class='ci-hero'>%s</span> species; Chao1 estimates %s are really here, so sampling looks %s.",
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
      HTML(paste0("Breeding peaks: ", paste(segs, collapse = ", "), ".")))
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

    mdl3 <- c("\U0001F947", "\U0001F948", "\U0001F949")   # gold / silver / bronze
    medal <- function(rank) if (rank <= 3)
      sprintf("<span class='medal'>%s</span> %d", mdl3[rank], rank) else paste0("#", rank)
    rar_badge <- function(tier) {
      m <- lapply(tier, rarity_meta)
      vapply(seq_along(tier), function(i) sprintf(
        "<span class='tag-badge%s' style='background:%s;border-color:%s;color:#fff'>%s %s</span>",
        if (tier[i] == "Legendary") " is-legendary" else "",
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

  # category-switch banner: names the current leader so re-ranking has a payoff
  output$leaderBanner <- renderUI({
    v <- rv$lb_view; req(v); if (nrow(v) == 0) return(NULL)
    cat_key <- input$leaderCat %||% "captures"; lead <- v[1, ]
    lab <- switch(cat_key, captures = "most-caught", weight = "heaviest",
                  career = "longest-tenured", roam = "furthest-roaming", chonk = "chonkiest", "top")
    val <- switch(cat_key,
      captures = paste0(fmt_int(lead$captures), " captures"),
      weight   = ifelse(is.na(lead$max_weight), "—", paste0(lead$max_weight, " g")),
      career   = ifelse(lead$career_days > 0, paste0(lead$career_days, " days"), "—"),
      roam     = ifelse(is.na(lead$roam_m), "—", paste0(lead$roam_m, " m")),
      chonk    = ifelse(is.na(lead$chonk_pct), "—", paste0(round(lead$chonk_pct), "% weight-for-species")),
      "")
    insight_banner("trophy-fill", tone = "gold",
      HTML(sprintf("The <b>%s</b> animal at this site is <span class='ci-hero'>%s</span> (<i>%s</i>), with %s.",
        lab, lead$short, lead$scientificName, val)))
  })

  # top-3 podium (olympic layout: 2nd · 1st-raised · 3rd), clickable to dossiers
  output$famePodium <- renderUI({
    v <- rv$lb_view; req(v); if (nrow(v) == 0) return(NULL)
    cat_key <- input$leaderCat %||% "captures"
    big_stat <- function(r) switch(cat_key,
      captures = paste0(fmt_int(r$captures), " caps"),
      weight   = ifelse(is.na(r$max_weight), "—", paste0(r$max_weight, " g")),
      career   = ifelse(r$career_days > 0, paste0(r$career_days, " d"), "—"),
      roam     = ifelse(is.na(r$roam_m), "—", paste0(r$roam_m, " m")),
      chonk    = ifelse(is.na(r$chonk_pct), "—", paste0(round(r$chonk_pct), "%")),
      paste0(fmt_int(r$captures), " caps"))
    top <- utils::head(v, 3)
    pos_order <- if (nrow(top) >= 3) c(2L, 1L, 3L) else seq_len(nrow(top))
    cards <- lapply(pos_order, function(i) {
      r <- top[i, ]; rm <- rarity_meta(r$rarity)
      div(class = paste0("podium-card podium-", i, if (r$rarity == "Legendary") " is-legendary" else ""),
        style = sprintf("--rc:%s", rm$color),
        onclick = sprintf("Shiny.setInputValue('modalPick','%s',{priority:'event'})", r$tagID),
        div(class = "podium-medal", c("\U0001F947", "\U0001F948", "\U0001F949")[i]),
        div(class = "podium-emoji", r$emoji),
        div(class = "podium-id", r$short),
        div(class = "podium-stat", big_stat(r)),
        div(class = "podium-sp", em(r$scientificName)))
    })
    div(class = "podium", cards)
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
      p("Every individual NEON tagged at this site has a full profile: measurements, a trap-grid home range, capture history, and a shareable card."),
      div(class = "empty-actions",
        actionButton("goFameFromDossier", tagList(bs_icon("trophy-fill"), " Browse the Hall of Fame"),
                     class = "btn-primary"),
        actionButton("surpriseFromDossier", tagList(bs_icon("dice-5-fill"), " Surprise me"),
                     class = "btn-outline-dark"))))
    row <- rv$lb[rv$lb$tagID == tag, ]; req(nrow(row) == 1)
    rm <- rarity_meta(row$rarity[1])
    nick <- if (!is.na(row$nickname[1])) row$nickname[1] else "small mammal"

    # numeric stat tiles animate via the shared count-up engine (data-target +
    # optional unit suffix); non-numeric tiles (home plot) render plain.
    stat <- function(label, target = NA, suffix = "", fallback = "—") {
      v <- if (length(target) == 1 && !is.na(target) && is.finite(target))
        tags$span(class = "count-up", `data-target` = target, `data-suffix` = suffix, "0")
      else fallback
      div(class = "ds-stat", div(class = "ds-stat-v", v), div(class = "ds-stat-l", label))
    }
    fmt_date <- function(x) if (is.na(x)) "—" else format(x, "%b %Y")

    # one computed "story" sentence, with the lead stat ranked against peers
    lb <- rv$lb; ntot <- nrow(lb)
    cap_rank <- sum(lb$captures > row$captures[1], na.rm = TRUE) + 1L
    n_tied_top <- sum(lb$captures == row$captures[1], na.rm = TRUE)   # don't crown two tied animals
    rank_phrase <- if (cap_rank == 1 && n_tied_top == 1) "the most-caught individual at this site"
      else if (cap_rank == 1) sprintf("tied for most-caught at this site (%d-way)", n_tied_top)
      else if (cap_rank <= max(2, ceiling(0.05 * ntot))) sprintf("in the top %d%% by captures here", max(1L, round(100 * cap_rank / ntot)))
      else sprintf("#%d of %d by captures here", cap_rank, ntot)
    art <- if (grepl("^[AEIOU]", row$rarity[1])) "an" else "a"
    ci_tone <- switch(row$rarity[1], Legendary = "gold", Epic = "terra", Rare = "navy", Uncommon = "pine", "muted")
    story <- sprintf("<b>%s</b> is %s <b>%s</b> resident, caught <span class='ci-hero'>%s</span> times%s, %s.",
      row$short[1], art, row$rarity[1], fmt_int(row$captures[1]),
      if (row$career_days[1] > 0) sprintf(" over %s days", fmt_int(row$career_days[1])) else "",
      rank_phrase)

    # approximate-age tile — a plain string (it carries a ~ / ≥ glyph the count-up
    # animator can't render). "~X.X yr" only for animals first caught young;
    # "≥X.X yr" (left-censored minimum) for adult/unknown-first. Suppressed when
    # tag_suspect: an impossible history (same-day two-plot, or a beyond-lifespan
    # span) isn't one animal, so any age is fiction — the "verify tag" chip says why.
    age_val <- row$approx_age_years[1]
    age_tile <- if (!isTRUE(row$tag_suspect[1]) && length(age_val) == 1 && !is.na(age_val) && is.finite(age_val)) {
      pre <- if (isTRUE(row$age_is_minimum[1])) "≥" else "~"
      tip <- paste0("Days we knew this animal, plus an estimate of how old it already was at first ",
                    "capture (≈1 mo if first seen as a juvenile, ≈2.5 mo as a subadult). ",
                    "≥ means first caught as an adult, so its true age is at least this; ",
                    "we can't see how old it was before we met it. Approximate to ~0.1 yr, not a birthday.")
      div(class = "ds-stat ds-stat-hint", title = tip,
          div(class = "ds-stat-v", paste0(pre, age_val, " yr")),
          div(class = "ds-stat-l", "approx age"))
    } else NULL

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
            span(class = "ds-warn", title = "This capture history can't be one animal: it was recorded at two plots on the same day, or spans longer than these species live. Likely a tag-number mix-up or data-entry error; verify the record.",
                 bs_icon("exclamation-triangle-fill"), " verify tag"),
          if (isTRUE(row$id_uncertain[1]))
            span(class = "ds-warn", title = "This tag was recorded under more than one species",
                 bs_icon("question-circle-fill"), " ID uncertain")
        ),
        div(class = "ds-sci", em(row$scientificName[1])),
        insight_banner("stars", tone = ci_tone, HTML(story)),
        div(class = "ds-stats",
          stat("captures", target = row$captures[1]),
          stat("career span", target = if (row$career_days[1] > 0) row$career_days[1] else NA, suffix = "d"),
          age_tile,
          stat("traps used", target = row$n_traps[1]),
          stat("max move", target = row$mdm_m[1], suffix = "m"),
          stat("avg weight", target = row$avg_weight[1], suffix = "g"),
          stat("home plot", fallback = row$home_plot[1])
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
    # ADULT reference band — pooling juveniles/subadults drags the species
    # median/IQR down, making a normal adult read as "above" the band.
    sp_w <- rv$data %>% dplyr::filter(.data$scientificName == sp, .data$lifeStage %in% "adult",
                                      !is.na(.data$weight), .data$weight > 0) %>% dplyr::pull(.data$weight)
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
          name = "adult IQR", hoverinfo = "skip") %>%
        add_trace(x = xr, y = rep(qs[2], 2), type = "scatter", mode = "lines",
          line = list(color = "rgba(31,42,48,0.35)", width = 1, dash = "dash"),
          name = "adult median wt", hoverinfo = "skip")
    }
    wt_col <- dcol("#38a8e8"); hf_col <- dcol("#fb8a7e"); gold_col <- dcol("#e0b43a")
    p <- p %>% add_trace(
      data = df, x = ~date, y = ~weight, name = "Weight (g)",
      type = "scatter", mode = "lines+markers",
      line = list(color = wt_col, width = 2), marker = list(color = wt_col, size = 8),
      hovertemplate = "%{x|%b %d, %Y}<br><span style='color:#38a8e8'>●</span> Weight: %{y} g<extra></extra>")
    p <- p %>% add_trace(
      data = df, x = ~date, y = ~hindfootLength, name = "Hind foot (mm)", yaxis = "y2",
      type = "scatter", mode = "lines+markers",
      line = list(color = hf_col, width = 2, dash = "dot"), marker = list(color = hf_col, size = 7),
      hovertemplate = "%{x|%b %d, %Y}<br><span style='color:#fb8a7e'>●</span> Hind foot: %{y} mm<extra></extra>")

    # QC-flag lifecycle · stage 5 (mark): ring any capture whose weight is a flagged
    # possible error (±5·MAD vs species-adults — the SAME record the species table's
    # ⚠ and the QC modal list). The value stays plotted (never silently dropped); the
    # ring + honest hover just mark it for verification against the field sheet.
    wflag <- (df$lifeStage %in% "adult") &
      .is_meas_outlier(df$weight, species_adult_values(rv$data, sp, "weight"))
    if (any(wflag, na.rm = TRUE)) {
      fd <- df[which(wflag), , drop = FALSE]
      p <- p %>% add_trace(data = fd, x = ~date, y = ~weight, name = "possible error",
        type = "scatter", mode = "markers",
        marker = list(color = "rgba(0,0,0,0)", size = 16, symbol = "circle",
                      line = list(color = "#d6453a", width = 2.5)),
        hovertemplate = "%{x|%b %d, %Y}<br>⚠ %{y} g — possible data-entry error<br>(beyond median ± 5·MAD for adults; verify vs the field sheet)<extra></extra>")
    }

    # call out the heaviest capture — excluding a flagged possible-error high outlier
    # so "heaviest" never points at a data-entry error (same exclusion as the chonk).
    ann <- list()
    if (any(is.finite(df$weight))) {
      wclean <- df$weight; wclean[wflag] <- NA_real_
      i <- if (any(is.finite(wclean))) which.max(wclean) else which.max(df$weight)
      ann <- list(list(x = df$date[i], y = df$weight[i],
        text = sprintf("heaviest ♦ %sg", df$weight[i]), showarrow = TRUE, arrowcolor = gold_col,
        ax = 0, ay = -28, font = list(color = gold_col, size = 11)))
    }

    plotly_theme(p) %>% plotly::layout(
      yaxis  = list(title = "Weight (g)", color = wt_col),
      yaxis2 = list(title = "Hind foot (mm)", color = hf_col, overlaying = "y",
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
        increasing = list(color = "#5fb56a"), decreasing = list(color = "#43b8e8"),
        font = list(size = 13)),
      title = list(text = sprintf("<b>%s</b><br><span style='font-size:12px;color:#647585'>adult weight percentile vs %s</span>",
                                  row$chonk_tier[1], row$scientificName[1]),
                   font = list(color = "#1f2a30", size = 16)),
      gauge = list(
        # subtitle span + tick labels nudged #6b7a85 (4.42:1) -> #647585 (4.75:1) for the
        # WCAG text gate; this gauge is light-styled in both modes (see step colours below).
        axis = list(range = list(0, 100), tickcolor = "#647585", tickfont = list(color = "#647585")),
        bar = list(color = "#38a8e8", thickness = 0.28),
        bgcolor = "rgba(0,0,0,0)", borderwidth = 0,
        steps = list(
          list(range = c(0, 20),  color = "#e3eef0"),
          list(range = c(20, 40), color = "#e6f1ea"),
          list(range = c(40, 60), color = "#f0f1ec"),
          list(range = c(60, 80), color = "#faedd6"),
          list(range = c(80, 100),color = "#f6ddd2")),
        threshold = list(line = list(color = "#fb8a7e", width = 3), thickness = 0.8, value = 50))
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
      stage_col <- c(adult = dcol("#38a8e8"), subadult = dcol("#fb8a7e"),
                     juvenile = dcol("#9bd24a"), unknown = "#6c757d")
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
        marker = list(color = dcol("#e0b43a"), size = 15, symbol = "diamond",
                      line = list(color = "#ffffff", width = 1.5)),
        line = list(color = if (is_dark()) "rgba(255,210,74,0.5)" else "rgba(154,107,0,0.45)", width = 1.5),
        hovertemplate = "this animal<br>%{x} mm · %{y} g<extra></extra>")

    # QC-flag lifecycle · stage 5 (mark): ring the individual's flagged possible-error
    # capture(s) so the low/high outlier diamond stands out from its true cloud (the
    # SAME record the species ⚠ / QC modal / measurement-plot flag point to).
    if (nrow(ind) > 0) {
      ifl <- (ind$lifeStage %in% "adult") &
        .is_meas_outlier(ind$weight, species_adult_values(d, sp, "weight"))
      if (any(ifl, na.rm = TRUE)) {
        fdm <- ind[which(ifl), , drop = FALSE]
        p <- p %>% add_trace(data = fdm, x = ~hindfootLength, y = ~weight, name = "possible error",
          type = "scatter", mode = "markers",
          marker = list(color = "rgba(0,0,0,0)", size = 22, symbol = "diamond-open",
                        line = list(color = "#d6453a", width = 3)),
          hovertemplate = "⚠ %{x} mm · %{y} g — possible data-entry error<extra></extra>")
      }
    }

    note <- if (!(nrow(srow) == 1 && !is.na(srow$b)))
      list(list(text = "↳ hind-foot barely predicts mass in this species; read position, not a line",
        x = 0, y = 1.08, xref = "paper", yref = "paper", showarrow = FALSE, xanchor = "left",
        font = list(color = muted_ink(), size = 11))) else list()

    plotly_theme(p) %>% plotly::layout(
      xaxis = list(title = "Hind-foot length (mm)"),
      yaxis = list(title = "Weight (g)"),
      annotations = note, hovermode = "closest")
  })

  # =========================================================================
  # SIZE LAB — the interactive body-size scatter (one dot per individual,
  # coloured by species) + the downloadable per-rodent QC history card.
  # Tap a dot to pin its card (pincards.js); the card's chip selects the
  # individual, which renders its QC history card below.
  # =========================================================================
  output$bodyScatter <- renderPlotly({
    lb <- rv$lb; d <- rv$data; req(lb, d)
    # the measurable universe: individuals with BOTH a weight and a hind-foot
    measured <- dplyr::filter(lb, is.finite(.data$avg_hf), is.finite(.data$avg_weight),
                              .data$avg_hf > 0, .data$avg_weight > 0, !is.na(.data$scientificName))
    if (nrow(measured) == 0)
      return(note_plot("No individuals at this site have both a weight<br>and a hind-foot measurement, so nothing to map.", "\U0001F4CF"))
    pts <- measured
    one_sp <- !is.null(input$scatterSpecies) && nzchar(input$scatterSpecies) && input$scatterSpecies != "all"
    if (one_sp) pts <- pts[pts$scientificName == input$scatterSpecies, ]
    if (!is.null(input$scatterPlot) && nzchar(input$scatterPlot) && input$scatterPlot != "all")
      pts <- pts[!is.na(pts$home_plot) & pts$home_plot == input$scatterPlot, ]
    if (isTRUE(input$scatterAdults)) pts <- pts[pts$lifeStage %in% "adult", ]
    if (nrow(pts) == 0)
      return(note_plot("No individuals match these filters.<br>Widen the species / plot / adults filters.", "\U0001F50D"))

    pal <- species_pal()                 # mode-aware; recolors on light/dark toggle
    n_species_shown <- length(unique(pts$scientificName))

    # per-individual pin-card HTML carried in plotly customdata (read back in
    # pincards.js on plotly_click). Built on the FULL filtered set BEFORE the
    # downsample so the gold-diamond lookup keeps a tip even if it's sampled out.
    cap_lbl <- ifelse(pts$captures == 1, " cap", " caps")
    age <- ifelse(!is.na(pts$approx_age_years) & !(pts$tag_suspect %in% TRUE),
                  paste0(ifelse(pts$age_is_minimum %in% TRUE, "≥", "~"), pts$approx_age_years, " yr"), "")
    statline <- paste0(
      pts$captures, cap_lbl,
      ifelse(pts$career_days > 0, paste0(" · ", pts$career_days, "d"), ""),
      ifelse(is.na(pts$chonk_pct), "", paste0(" · chonk ", round(pts$chonk_pct), "%ile")),
      ifelse(nzchar(age), paste0(" · ", age), ""),
      ifelse(is.na(pts$avg_weight), "", paste0(" · ", round(pts$avg_weight, 1), "g")),
      ifelse(is.na(pts$avg_hf), "", paste0(" · ", round(pts$avg_hf, 1), "mm")))
    warn <- ifelse(pts$tag_suspect %in% TRUE,
      "<br/><span class='smt-pin-rar' style='color:#ffb3a7'>⚠ verify tag</span>",
      ifelse(pts$id_uncertain %in% TRUE,
        "<br/><span class='smt-pin-rar' style='color:#ffd9a7'>⚠ ID uncertain</span>", ""))
    pts$tip <- paste0(
      "<span class='smt-pin-emoji'>", pts$emoji, "</span> <b>", pts$short, "</b> ",
      "<span class='smt-pin-rar'>", pts$rarity, "</span><br/>",
      "<em>", pts$scientificName, "</em><br/>",
      "<span class='smt-pin-stats'>", statline, "</span>", warn,
      "<br/><span class='smt-open' role='button' tabindex='0' data-tag='", pts$tagID,
        "'>\U0001F50D Open QC history card &rarr;</span>",
      "<br/><em class='smt-pin-hint'>Tap the dot to pin this card</em>")

    full_pts <- pts                          # pre-downsample (single-species stats + diamond)
    # a11y table twin (WCAG 2.1.1): stash the COMPLETE (un-downsampled) frame the
    # scatter is built from, so the keyboard/AT "View the numbers" table shows every
    # individual — not just the 1500-point sampled subset the SVG draws.
    bodyTbl_rv(full_pts[, intersect(
      c("tagID","short","emoji","scientificName","rarity","lifeStage","home_plot",
        "avg_hf","avg_weight","captures","career_days","chonk_pct"), names(full_pts)),
      drop = FALSE])
    if (nrow(pts) > 1500) { set.seed(7); pts <- pts[sort(sample.int(nrow(pts), 1500)), ] }

    # S5: deterministic sub-pixel jitter so individuals that round to identical
    # (hind-foot, weight) coords fan out and each stays individually tappable
    # (hind-foot is whole-mm, so a busy species otherwise stacks into one un-pickable
    # blob). Keyed on tagID -> stable across renders, no global-RNG touch; the hover
    # and the pin card still report each animal's TRUE measured means.
    .jit <- function(tags, amp, salt) {
      if (!length(tags) || !is.finite(amp) || amp <= 0) return(rep(0, length(tags)))
      h <- vapply(paste0(tags, salt), function(s) { z <- utf8ToInt(s); (sum(z * seq_along(z)) %% 997) / 997 }, numeric(1))
      (h - 0.5) * 2 * amp
    }
    rng_x <- suppressWarnings(diff(range(pts$avg_hf, na.rm = TRUE)))
    rng_y <- suppressWarnings(diff(range(pts$avg_weight, na.rm = TRUE)))
    amp_x <- if (is.finite(rng_x) && rng_x > 0) rng_x * 0.012 else 0
    amp_y <- if (is.finite(rng_y) && rng_y > 0) rng_y * 0.012 else 0
    pts$jx <- pts$avg_hf + .jit(pts$tagID, amp_x, "x")
    pts$jy <- pts$avg_weight + .jit(pts$tagID, amp_y, "y")
    pts$hovtxt <- paste0(pts$short, " \U00B7 ", pts$scientificName, "<br>",
                         round(pts$avg_hf, 1), " mm \U00B7 ", round(pts$avg_weight, 1), " g")

    # theme-aware greys so every mark stays legible on the dark navy background
    muted_col <- if (is_dark()) "#9fb0c4" else "#6b7a85"
    quad_col  <- if (is_dark()) "#7e8da0" else "#9aa6b2"
    fit_col   <- if (is_dark()) "rgba(210,220,235,0.6)" else "rgba(31,42,48,0.45)"
    # past ~a dozen species a horizontal legend wraps into an unreadable wall on a
    # phone — drop it and lean on hover/tap (the pin card names the species).
    show_leg  <- n_species_shown <= 12

    p <- plot_ly()
    for (s in sort(unique(pts$scientificName))) {
      sub <- pts[pts$scientificName == s, ]
      col <- if (s %in% names(pal)) pal[[s]] else acc()
      p <- p %>% add_trace(data = sub, x = ~jx, y = ~jy,
        type = "scatter", mode = "markers", name = s, customdata = ~tip, showlegend = show_leg,
        marker = list(color = col, size = 9, opacity = 0.82,
                      line = list(color = "#ffffff", width = 0.6)),
        text = ~hovtxt,                                  # carries the TRUE mm/g (markers are jittered)
        hovertemplate = "%{text}<extra></extra>")
    }

    # Be explicit about the life-stage scope: the dot is a mean over an animal's
    # captures, and "Adults only" filters by MODAL stage (so the mean still pools
    # whatever stages that animal was caught at) — say so rather than imply the
    # dot is a clean adult body size.
    scope_note <- if (isTRUE(input$scatterAdults))
      "each dot = an adult-classified animal's mean across its captures · a QC map, not a body-condition index"
    else
      "each dot = one animal's mean across all captures (life stages pooled) · a QC map, not a body-condition index"
    ann <- list(list(text = scope_note,
      x = 0, y = 1.08, xref = "paper", yref = "paper", showarrow = FALSE, xanchor = "left",
      font = list(color = muted_col, size = 11)))
    if (!show_leg)
      ann[[length(ann) + 1L]] <- list(text = "↳ many species shown; hover or tap a dot for its species",
        x = 0, y = 1.14, xref = "paper", yref = "paper", showarrow = FALSE, xanchor = "left",
        font = list(color = muted_col, size = 11))
    shapes <- list()

    if (one_sp) {
      sp <- input$scatterSpecies
      # median crosshairs + body-shape quadrant labels, computed from the FULL
      # (pre-sample) single-species set so the "typical" split reflects the whole
      # population, not a 1500-row subsample. (Cross-species quadrants are
      # meaningless — species occupy different size niches — so single-species only.)
      if (nrow(full_pts) >= 6) {
        mx <- stats::median(full_pts$avg_hf); my <- stats::median(full_pts$avg_weight)
        shapes <- list(
          list(type = "line", xref = "x", yref = "paper", x0 = mx, x1 = mx, y0 = 0, y1 = 1,
               line = list(color = quad_col, dash = "dot", width = 1)),
          list(type = "line", xref = "paper", yref = "y", x0 = 0, x1 = 1, y0 = my, y1 = my,
               line = list(color = quad_col, dash = "dot", width = 1)))
        xr <- range(full_pts$avg_hf); yr <- range(full_pts$avg_weight)
        px <- diff(xr) * 0.02; py <- diff(yr) * 0.02
        qlab <- function(x, y, t, xa, ya) list(text = t, x = x, y = y, xref = "x", yref = "y",
          showarrow = FALSE, xanchor = xa, yanchor = ya, font = list(color = quad_col, size = 10.5))
        ann <- c(ann, list(
          qlab(xr[2] - px, yr[2] - py, "BIG & LEGGY",  "right", "top"),
          qlab(xr[1] + px, yr[2] - py, "ROLY-POLY",    "left",  "top"),
          qlab(xr[2] - px, yr[1] + py, "LEGGY",        "right", "bottom"),
          qlab(xr[1] + px, yr[1] + py, "POCKET-SIZED", "left",  "bottom")))
      }
      # The adult size–mass fit line is only honest when the DOTS are adults — an
      # adult-calibrated line over juvenile dots would read them as "underweight".
      # So draw it only with "Adults only" on, fit on those adult dots, label adult.
      if (isTRUE(input$scatterAdults)) {
        # S4: fit on the DOTS ACTUALLY SHOWN (per-individual adult means, respecting
        # the plot filter) and label with THEIR r — so the line + r describe what's on
        # screen, not a different (all-plots, per-capture) population. SMA (sd-ratio)
        # slope on true coords, same gate as before (n>=15 & |r|>=0.3).
        fp <- full_pts[is.finite(full_pts$avg_hf) & is.finite(full_pts$avg_weight) &
                       full_pts$avg_hf > 0 & full_pts$avg_weight > 0, , drop = FALSE]
        lw <- log(fp$avg_weight); lh <- log(fp$avg_hf)
        rr <- if (nrow(fp) >= 15 && stats::sd(lh) > 0 && stats::sd(lw) > 0)
                suppressWarnings(stats::cor(lh, lw)) else NA_real_
        if (is.finite(rr) && abs(rr) >= 0.3) {
          b <- stats::sd(lw) / stats::sd(lh) * sign(rr)
          a <- mean(lw) - b * mean(lh)
          lx <- seq(min(fp$avg_hf), max(fp$avg_hf), length.out = 40)
          p <- p %>% add_trace(x = lx, y = exp(a + b * log(lx)), type = "scatter", mode = "lines",
            name = sprintf("adult size–mass fit (r=%.2f)", rr), hoverinfo = "skip", inherit = FALSE,
            line = list(color = fit_col, width = 2, dash = "dash"))
        } else {
          ann[[length(ann) + 1L]] <- list(
            text = if (nrow(fp) < 15) "↳ too few adults here to fit a size–mass line"
                   else "↳ hind-foot barely predicts mass in these adults; read position, not a line",
            x = 0, y = 1.20, xref = "paper", yref = "paper", showarrow = FALSE, xanchor = "left",
            font = list(color = muted_col, size = 11))
        }
      } else {
        ann[[length(ann) + 1L]] <- list(
          text = "↳ tick “Adults only” to add the adult size–mass fit line",
          x = 0, y = 1.20, xref = "paper", yref = "paper", showarrow = FALSE, xanchor = "left",
          font = list(color = muted_col, size = 11))
      }
    }

    # the individual currently being tracked, as the gold "this animal" diamond.
    # Look it up in the FULL (pre-sample) filtered set so it never vanishes to the
    # downsample, and give it the same customdata so a tap on it pins its card.
    tag <- rv$tag
    if (!is.null(tag)) {
      ir <- full_pts[full_pts$tagID == tag, ]
      if (nrow(ir) == 1)
        # jitter by the SAME per-tag amount as its species dot so the diamond sits
        # on it (not beside it); hover still reports the true measured means.
        p <- p %>% add_trace(x = ir$avg_hf + .jit(ir$tagID, amp_x, "x"),
          y = ir$avg_weight + .jit(ir$tagID, amp_y, "y"), type = "scatter", mode = "markers",
          name = "★ tracking", showlegend = TRUE, customdata = ir$tip,
          marker = list(symbol = "diamond", size = 17, color = dcol("#e0b43a"),
                        line = list(color = "#ffffff", width = 1.6)),
          hovertemplate = paste0("tracking ", ir$short[1], "<br>",
            round(ir$avg_hf[1], 1), " mm · ", round(ir$avg_weight[1], 1), " g<extra></extra>"))
    }

    # the site/year caption, folded into the annotation list (not via ctx_anno's
    # add_annotations, which appends a fresh copy on every reactive re-render —
    # this scatter re-renders on every filter change, so that would stack copies)
    if (!is.null(rv$ctx))
      ann[[length(ann) + 1L]] <- list(text = rv$ctx, x = 1, y = 1.03,
        xref = "paper", yref = "paper", xanchor = "right", yanchor = "bottom",
        showarrow = FALSE,
        font = list(color = if (is_dark()) "#9fb0c4" else "#6b7a89", size = 11, family = "Rubik"))

    plotly_theme(p) %>% plotly::layout(
      xaxis = list(title = "Hind-foot length (mm)"),
      yaxis = list(title = "Weight (g)"),
      # taller top margin so the stacked y=1.08–1.20 captions don't clip the
      # inherited t=48 (matches the t=72/84 precedent elsewhere in this file)
      margin = list(l = 55, r = 30, t = 96, b = 46),
      annotations = ann, shapes = shapes, hovermode = "closest")
  })

  # ---- the QC history card (the downloadable per-rodent "field record") ----
  output$qcHistoryCard <- renderUI({
    tag <- rv$tag
    if (is.null(tag)) return(div(class = "qc-empty",
      div(class = "qc-empty-icon", "\U0001F50D"),
      h4("Pick an animal to open its QC history card"),
      p("Tap a dot on the scatter above and choose ", tags$b("“Open QC history card”"),
        ", or use the ", tags$b("“Track an individual”"), " picker near the top. You'll get every capture's measurements plus automatic data-quality flags, and you can download the card or the raw history.")))
    lb <- rv$lb; row <- lb[lb$tagID == tag, ]; req(nrow(row) == 1)
    d <- rv$data
    hist <- individual_history(d, tag)
    flags <- individual_qc_flags(hist, row)
    rmeta <- rarity_meta(row$rarity[1])   # not `rm` — that shadows base::rm()

    tile <- function(v, l) div(class = "qc-tile", div(class = "qc-tile-v", v), div(class = "qc-tile-l", l))
    yr <- function(x) if (is.na(x)) "" else format(x, "%Y")
    span_yr <- paste(na.omit(unique(c(yr(row$first_seen[1]), yr(row$last_seen[1])))), collapse = "–")
    age_txt <- if (!is.na(row$approx_age_years[1]) && !isTRUE(row$tag_suspect[1]))
      paste0(ifelse(isTRUE(row$age_is_minimum[1]), "≥", "~"), row$approx_age_years[1], "y") else "—"
    chonk_txt <- if (is.na(row$chonk_pct[1])) "—" else paste0(round(row$chonk_pct[1]), "%")
    home_txt  <- if (is.na(row$home_plot[1])) "—" else row$home_plot[1]

    flag_ic <- c(high = "exclamation-octagon-fill", warn = "exclamation-triangle-fill", info = "info-circle-fill")
    flags_ui <- if (length(flags) == 0)
      div(class = "qc-flag clean",
        span(class = "qc-flag-ic", bs_icon("check-circle-fill")),
        span(HTML("<b>No QC flags.</b> This individual's capture history is internally consistent: measurements, sex, life stage and movement all hold together.")))
    else tagList(lapply(flags, function(f)
      div(class = paste("qc-flag", f$level),
        span(class = "qc-flag-ic", bs_icon(flag_ic[[f$level]] %||% "info-circle-fill")),
        span(HTML(f$text)))))

    cap_tbl <- if (is.null(hist) || !nrow(hist)) p(class = "qc-cap-note", "No dated captures to list.") else {
      fnum <- function(x) ifelse(is.na(x) | !is.finite(x), "—", formatC(round(x, 1), format = "f", digits = 1))
      fchr <- function(x) ifelse(is.na(x) | x == "", "—", as.character(x))
      tagList(
        p(class = "qc-cap-note", sprintf("%d capture%s · weight & hind-foot are taken at nearly every handling; tail & ear far less often (— = not recorded).",
          nrow(hist), ifelse(nrow(hist) == 1, "", "s"))),
        div(class = "qc-cap-scroll",
          tags$table(class = "inspect-tbl",
            tags$thead(tags$tr(lapply(
              c("Date", "Plot", "Trap", "Stage", "Sex", "Wt (g)", "HF (mm)", "Tail (mm)", "Ear (mm)"), tags$th))),
            tags$tbody(lapply(seq_len(nrow(hist)), function(i) tags$tr(
              tags$td(format(hist$date[i], "%Y-%m-%d")),
              tags$td(fchr(hist$plotID[i])),
              tags$td(fchr(hist$trapCoordinate[i])),
              tags$td(fchr(hist$lifeStage[i])),
              tags$td(fchr(hist$sex[i])),
              tags$td(fnum(hist$weight[i])),
              tags$td(fnum(hist$hindfootLength[i])),
              tags$td(fnum(hist$tailLength[i])),
              tags$td(fnum(hist$earLength[i]))))))))
    }

    div(
      div(id = "qcCardNode", class = "qc-card", `data-short` = row$short[1],
          style = sprintf("--rc:%s;", rmeta$color),
        div(class = "qc-head",
          span(class = "qc-emoji", row$emoji[1]),
          div(
            div(class = "qc-id", row$short[1],
              if (isTRUE(row$tag_suspect[1])) span(class = "ds-warn", bs_icon("exclamation-triangle-fill"), " verify tag"),
              if (isTRUE(row$id_uncertain[1])) span(class = "ds-warn", bs_icon("question-circle-fill"), " ID uncertain")),
            div(class = "qc-sci", em(row$scientificName[1]))),
          div(class = "qc-head-badges",
            glow_badge(paste(rmeta$icon, row$rarity[1]), rmeta$color, rmeta$glow),
            tags$span(style = "color:var(--muted);font-size:12px;",
                      mode_chr(d$siteID), if (nzchar(span_yr)) paste0(" · ", span_yr)))),
        div(class = "qc-tiles",
          tile(row$captures[1], "captures"),
          tile(if (row$career_days[1] > 0) paste0(row$career_days[1], "d") else "—", "career"),
          tile(age_txt, "approx age"),
          tile(chonk_txt, "chonk %ile"),
          tile(if (is.na(row$avg_weight[1])) "—" else paste0(round(row$avg_weight[1], 1), "g"), "avg wt"),
          tile(if (is.na(row$avg_hf[1])) "—" else paste0(round(row$avg_hf[1], 1), "mm"), "avg HF"),
          tile(home_txt, "home plot")),
        div(class = "qc-section-h", bs_icon("clipboard-check"), " Data-quality check"),
        flags_ui,
        div(class = "qc-section-h", bs_icon("clock-history"), " Every capture (the meso measurements)"),
        cap_tbl,
        p(class = "qc-cap-note", style = "margin-top:8px",
          bs_icon("info-circle"), " A flag means “verify against the datasheet”, not “wrong”; legitimate causes exist (a field sexing error vs a real one, lactation mass vs a typo). Gaps between captures are NEON's seasonal sampling cadence, not death.")),
      div(class = "qc-toolbar",
        tags$button(class = "smt-snap-btn", type = "button", onclick = "smtSaveQcCard()",
                    bsicons::bs_icon("download"), " Save QC card (PNG)"),
        downloadButton("qcHistoryCsv", "Download history (CSV)", class = "smt-clear-btn"),
        tags$span(class = "sizelab-hint", style = "margin-left:0", "a downloadable field record for QC")))
  })

  # raw capture history as a CSV (the analysis-ready companion to the QC card)
  output$qcHistoryCsv <- downloadHandler(
    filename = function() {
      tg <- rv$tag %||% "individual"
      sprintf("NEON-SmallMammal-QC_%s_%s.csv",
              gsub("[^A-Za-z0-9]+", "-", short_tag(tg)), format(Sys.Date(), "%Y%m%d"))
    },
    content = function(file) {
      tag <- rv$tag; req(tag)
      h <- individual_history(rv$data, tag); req(!is.null(h), nrow(h) > 0)
      out <- data.frame(tagID = tag, short = short_tag(tag), h, stringsAsFactors = FALSE)
      utils::write.csv(out, file, row.names = FALSE, na = "")
    },
    contentType = "text/csv"
  )

  # ---- capture history table ---------------------------------------------
  output$capHistory <- DT::renderDT({
    tag <- rv$tag
    if (is.null(tag)) return(DT::datatable(
      data.frame(` ` = "Pick an individual first: open the Hall of Fame and tap a row.", check.names = FALSE),
      rownames = FALSE, options = list(dom = "t", ordering = FALSE)))
    ir <- ind_rows()
    sp <- rv$lb$scientificName[rv$lb$tagID == tag][1]
    # QC-flag lifecycle · stage 5 (mark): a flagged possible-error weight (±5·MAD vs
    # species-adults — the same record the species ⚠ and the QC modal list) gets an
    # inline ⚠ in the row too, so the reviewer sees exactly which capture to verify.
    wflag <- (ir$lifeStage %in% "adult") &
      .is_meas_outlier(ir$weight, species_adult_values(rv$data, sp, "weight"))
    wcell <- ifelse(is.na(ir$weight), "",
      ifelse(wflag,
        sprintf("%s <span title='possible data-entry error — beyond median &#177; 5&#183;MAD for adults; verify vs the field sheet' style='color:#d6453a;font-weight:700'>&#9888;</span>", ir$weight),
        as.character(ir$weight)))
    df <- tibble::tibble(
      Date = format(ir$date, "%Y-%m-%d"),
      Plot = ir$plotID, Trap = ir$trapCoordinate, Recap = ir$recapture,
      `Weight (g)` = wcell, `Hind foot (mm)` = ir$hindfootLength,
      Sex = ir$sex, `Life stage` = ir$lifeStage, Fate = ir$fate,
      Remarks = htmltools::htmlEscape(ifelse(is.na(ir$remarks), "", ir$remarks)))
    DT::datatable(df, rownames = FALSE, selection = "none", escape = FALSE,
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
             else      list(c(0, empty), c(0.001, "#cfddf0"), c(0.5, "#4f8fc4"), c(1, "#38a8e8"))
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
        mode = "markers", marker = list(symbol = "x", size = 16, color = dcol("#fb8a7e"),
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
        line = list(color = if (is_dark()) "rgba(56,168,232,0.5)" else "rgba(21,101,176,0.5)", width = 2),
        marker = list(size = ~pmax(8, 18 - age * 2), color = ~age,
          colorscale = if (is_dark()) list(c(0, "#e0b43a"), c(1, "#38a8e8"))
                       else list(c(0, "#9a6b00"), c(1, "#1565b0")), showscale = FALSE,
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
        font = list(color = acc()))) %>%
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
    sp_pal <- species_pal()              # mode-aware; recolors on light/dark toggle
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

    leaflet(geo) %>%
      add_suite_basemap(view) %>%
      setView(lng = mean(geo$lng), lat = mean(geo$lat), zoom = 13) %>%
      addScaleBar("bottomleft", options = scaleBarOptions(imperial = FALSE)) %>%
      addCircleMarkers(~lng, ~lat, radius = ~pmax(5, sqrt(count) * 3.4 * r),
        fillColor = ~pal(scientificName), color = "#ffffff", weight = 1.2,
        fillOpacity = 0.78, opacity = 0.85,
        label = lapply(txt, htmltools::HTML),
        labelOptions = labelOptions(textsize = "13px", direction = "auto",
          style = list("background" = "#1f2937", "color" = "#eef3f8", "font-family" = "Inter",
                       "border-color" = "rgba(56,168,232,0.5)", "border-radius" = "8px"))) %>%
      # always-on plot name tags so the grid is legible at a glance
      addLabelOnlyMarkers(data = plot_tot, ~lng, ~lat, label = ~plotID,
        labelOptions = labelOptions(noHide = TRUE, direction = "top", textOnly = TRUE, offset = c(0, -14),
          style = list("color" = "#ffffff", "font-family" = "Jura, Rubik, monospace", "font-weight" = "700",
                       "font-size" = "11px", "text-shadow" = "0 1px 4px #000"))) %>%
      addLegend("bottomright", pal = pal, values = ~scientificName,
        title = "Species", opacity = 0.9)
  }, ignoreNULL = FALSE)

  output$map <- renderLeaflet({
    m <- mapBase()
    if (is.null(m)) {
      return(leaflet() %>%
        add_suite_basemap("CartoDB.DarkMatter") %>% setView(-98, 39, 3) %>%
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
      fillColor = "transparent", color = "#e0b43a", weight = 3, opacity = 0.9,
      label = lapply(sprintf("⭐ selected individual here (%d caps)", hl$n), htmltools::HTML))
  })

  # ---- between-plot recapture connectivity (the "movement" layer) ----------
  recap_flow <- reactive({ d <- rv$data; if (is.null(d)) return(NULL); recapture_edges(d) })

  # plain-English answer + the honest mark-recapture framing, above the map
  output$mapInsight <- renderUI({
    d <- rv$data; req(d)
    rf <- recap_flow(); if (is.null(rf)) return(NULL)
    if (rf$n_plots < 2)
      return(insight_banner("diagram-2", tone = "muted",
        "Only one trapping grid has coordinates here, so there's no between-grid movement to map."))
    if (rf$max_pair_m < 30)
      return(insight_banner("diagram-2", tone = "muted",
        HTML(sprintf("The grids here sit within <b>%d m</b> of each other; movement between them isn't resolvable at this scale.", round(rf$max_pair_m)))))
    if (rf$n_movers == 0)
      return(insight_banner("geo-fill", tone = "navy",
        "No between-grid recaptures: every tagged animal stayed on its home grid (high site fidelity)."))
    insight_banner("share-fill", tone = "navy",
      HTML(sprintf("<span class='ci-hero'>%d</span> of %d tagged individuals were recaptured across <b>2+ grids</b>. Toggle <b>recapture movement</b> (top-right) to see the links: curved lines connect successive capture plots, <i>not</i> tracked routes, and a long gap between the two just means the animal went undetected in between. <a href='#' class='inspect-link' onclick=\"Shiny.setInputValue('showMovers', Math.random(), {priority:'event'}); return false;\">Inspect these %d &raquo;</a>",
        rf$n_movers, rf$n_tagged, rf$n_movers)))
  })

  # QC inspector — list each cross-grid mover's capture history so a user can
  # judge "one real mover" vs "two animals sharing a tag" (a sex/species change
  # across captures, or a same-day two-plot record, is the tell).
  observeEvent(input$showMovers, {
    d <- rv$data; rf <- recap_flow(); req(d, !is.null(rf))
    cap <- inspect_captures(d, rf$movers)
    if (is.null(cap) || !nrow(cap)) {
      showNotification("No cross-grid movers to inspect here.", type = "message"); return()
    }
    blocks <- lapply(split(cap, factor(cap$short, levels = unique(cap$short))), function(g) {
      sexes <- unique(g$sex[g$sex %in% c("M", "F")])
      spp   <- unique(g$scientificName[!is.na(g$scientificName)])
      sameday <- any(tapply(g$plotID, g$date, function(p) length(unique(p))) > 1, na.rm = TRUE)
      tells <- c(if (length(sexes) > 1) "sex changes", if (length(spp) > 1) "species changes",
                 if (isTRUE(sameday)) "same day at 2 plots, physically impossible")
      div(class = "mover-block",
        div(class = "mover-head", tags$b(g$short[1]), " · ", em(spp[1]),
          tags$span(class = "mover-n", sprintf(" %d captures, %d plots", nrow(g), dplyr::n_distinct(g$plotID))),
          if (length(tells))
            tags$span(class = "mover-warn", bs_icon("exclamation-triangle-fill"),
                      paste0(" ", paste(tells, collapse = "; "), " → likely two animals"))),
        tags$table(class = "inspect-tbl",
          tags$thead(tags$tr(lapply(c("Date", "Plot", "Species", "Sex", "Stage"), tags$th))),
          tags$tbody(lapply(seq_len(nrow(g)), function(i)
            tags$tr(
              tags$td(format(g$date[i], "%Y-%m-%d")), tags$td(g$plotID[i]),
              tags$td(em(g$scientificName[i])),
              tags$td(ifelse(is.na(g$sex[i]), "?", g$sex[i])),
              tags$td(ifelse(is.na(g$lifeStage[i]), "?", g$lifeStage[i])))))))
    })
    showModal(modalDialog(
      title = tagList(bs_icon("share-fill"), " Individuals caught at 2+ grids"),
      size = "l", easyClose = TRUE, footer = modalButton("Close"),
      p(class = "inspect-note", HTML("NEON keeps a tag on one animal for life and doesn't reuse numbers, so most of these are <b>real</b> long-distance movers. But a tag whose <b>sex</b> or <b>species</b> changes across captures, or that shows up at two plots on the <b>same day</b>, is more likely two animals sharing a number; those are flagged below for you to verify against the field records.")),
      div(class = "movers-wrap", blocks)
    ))
  })

  # teal AGGREGATE layer — redraws only when the toggle or base map changes
  # (kept separate from the gold layer so clicking a new individual doesn't
  # flicker the whole network).
  observeEvent(list(input$showFlow, mapBase()), {
    proxy <- leafletProxy("map") %>% clearGroup("flow")
    if (!isTRUE(input$showFlow)) return()
    rf <- recap_flow()
    if (is.null(rf) || is.null(rf$edges) || !nrow(rf$edges) || rf$max_pair_m < 30) return()
    e <- rf$edges; maxw <- max(e$n_movers)
    for (i in seq_len(nrow(e))) {
      a <- arc_xy(e$lng0[i], e$lat0[i], e$lng1[i], e$lat1[i])
      frac <- if (maxw > 0) e$n_movers[i] / maxw else 1   # width+opacity carry weight (not hue) — CVD-safe
      proxy %>% addPolylines(lng = a$lng, lat = a$lat, group = "flow",
        weight = 1.8 + 5.5 * frac, opacity = 0.30 + 0.45 * frac, color = "#15b8a6",
        label = htmltools::HTML(sprintf("%s &harr; %s<br/><b>%d</b> individuals moved between", e$plot_a[i], e$plot_b[i], e$n_movers[i])))
    }
  }, ignoreNULL = FALSE)

  # selected individual's own date-ordered path, in gold — redraws on tag change
  observeEvent(list(rv$tag, input$showFlow, mapBase()), {
    proxy <- leafletProxy("map") %>% clearGroup("flowSel")
    if (!isTRUE(input$showFlow)) return()
    tag <- rv$tag; d <- rv$data; rf <- recap_flow()
    if (is.null(tag) || is.null(d) || is.null(rf) || is.null(rf$cen) || rf$max_pair_m < 30) return()
    th <- d %>% dplyr::filter(.data$tagID == tag, !is.na(.data$plotID), !is.na(.data$date)) %>%
      dplyr::arrange(.data$date)
    pl <- if (nrow(th)) rle(as.character(th$plotID))$values else character(0)
    cl <- rf$cen
    for (j in seq_len(max(0, length(pl) - 1))) {
      ra <- cl[match(pl[j], cl$plotID), ]; rb <- cl[match(pl[j + 1], cl$plotID), ]
      if (is.na(ra$lat) || is.na(rb$lat)) next   # plot lacked coords -> skip the leg, don't fabricate it
      a <- arc_xy(ra$lng, ra$lat, rb$lng, rb$lat)
      proxy %>% addPolylines(lng = a$lng, lat = a$lat, group = "flowSel",
        weight = 4, opacity = 0.95, color = "#e0b43a",
        label = htmltools::HTML(sprintf("selected: %s &rarr; %s", pl[j], pl[j + 1])))
    }
  }, ignoreNULL = FALSE)

  # ---- community pulse ----------------------------------------------------
  output$speciesBar <- renderPlotly({
    d <- rv$data; req(d)
    # "most common first" = ABUNDANCE = distinct individuals, NOT captures: a
    # trap-happy species inflates captures without being more abundant, so bar
    # length must be the metric the label prints and the headline ranks (individuals).
    s <- species_summary(d) %>% dplyr::arrange(dplyr::desc(.data$individuals)) %>%
      dplyr::slice_head(n = 14) %>% dplyr::arrange(.data$individuals)
    if (nrow(s) == 0) return(note_plot("No identified captures", "\U0001F9EC"))
    s$label <- paste0(s$emoji, " ", s$scientificName)
    s$recaps_per <- round(s$captures / s$individuals, 1)  # captures per animal (trap-happiness)
    s$hover <- paste0(format(s$individuals, big.mark = ","), " individuals \U00B7 ",
                      format(s$captures, big.mark = ","), " captures (", s$recaps_per, "\U00D7/animal)")
    # colour = the species' IDENTITY hue (the same palette the plot map + community
    # charts use), so each bar reads as that species at a glance instead of a drab
    # single-hue ramp. Trap-happiness (captures per individual) rides on the hover.
    pal <- make_species_pal(d, is_dark())
    s$col <- unname(pal[s$scientificName]); s$col[is.na(s$col)] <- if (is_dark()) "#8a97a8" else "#5a6a82"
    p <- plot_ly(s, x = ~individuals, y = ~factor(label, levels = label), type = "bar",
      orientation = "h",
      marker = list(color = ~col),
      customdata = ~scientificName, source = "speciesBar",
      text = ~paste0(format(individuals, big.mark = ","), " indiv"), textposition = "outside",
      hovertext = ~hover,
      # mode-aware: #6b7a85 is 4.42:1 on white (under the 4.5:1 gate) AND near-invisible
      # on the dark navy panel. Light twin #63727d = 4.96:1; dark uses a light slate.
      textfont = list(color = if (is_dark()) "#9fb0c4" else "#63727d", size = 11),
      hovertemplate = "<b>%{y}</b><br>%{hovertext}<br><span style='color:#cfe0f5'>tap for the full species breakdown</span><extra></extra>")
    # event_register LAST, on the final piped object, so no downstream step can drop
    # the click registration the observer at ~1486 reads (source="speciesBar").
    plotly_theme(p, legend = FALSE) %>%
      plotly::layout(xaxis = list(title = "individuals (abundance)"), yaxis = list(title = ""),
                     showlegend = FALSE, margin = list(l = 180, t = 44)) %>%
      ctx_anno() %>%
      plotly::event_register("plotly_click")
  })

  donut_center <- function(total, label) list(
    # center subtitle nudged #6b7a85 (4.42:1) -> #63727d (4.96:1) for the WCAG text gate
    text = sprintf("<b>%s</b><br><span style='font-size:11px;color:%s'>%s</span>",
      format(total, big.mark = ","), if (is_dark()) "#9fb0c4" else "#63727d", label),
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
    col <- c(F = "#c2255c", M = dcol("#43b8e8"), U = "#6c757d")
    tab$label <- lab[as.character(tab$key)]
    slice_col <- unname(col[as.character(tab$key)])
    plot_ly(tab, labels = ~label, values = ~n, type = "pie", hole = 0.62, sort = FALSE,
      marker = list(colors = slice_col, line = list(color = "#ffffff", width = 2)),
      pull = c(0.03, 0, 0), textinfo = "percent", textposition = "inside",
      insidetextorientation = "horizontal",
      # luminance-picked ink per slice: white-on-bright (dark-mode azure #43b8e8
      # 2.27:1) vanished; label_ink() gives navy on bright, white on dark.
      insidetextfont = list(color = label_ink(slice_col), size = 13),
      textfont = list(color = label_ink(slice_col), size = 13),
      hovertemplate = "<b>%{label}</b><br>%{value:,} individuals · %{percent:.0%} of handled<extra></extra>") %>%
      plotly::layout(title = list(text = "Sex", font = list(color = if (is_dark()) "#c3cedd" else "#344049", size = 14)),
        paper_bgcolor = "rgba(0,0,0,0)", showlegend = TRUE,
        legend = list(orientation = "h", y = -0.05, x = 0.5, xanchor = "center", font = list(size = 11)),
        annotations = list(donut_center(sum(tab$n), "individuals")),
        hoverlabel = list(bgcolor = "rgba(12,35,75,0.96)", bordercolor = "#ffd24a",
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
    col  <- c(juvenile = dcol("#9bd24a"), subadult = dcol("#fb8a7e"),
              adult = dcol("#38a8e8"), unknown = "#6c757d")
    per$stage <- factor(ifelse(!is.na(per$lifeStage) & per$lifeStage %in% lvls, per$lifeStage, "unknown"), levels = lvls)
    tab <- as.data.frame(table(per$stage)); names(tab) <- c("stage", "n")
    tab <- tab[tab$n > 0, , drop = FALSE]
    slice_col <- unname(col[as.character(tab$stage)])
    plot_ly(tab, labels = ~stage, values = ~n, type = "pie", hole = 0.62, sort = FALSE,
      marker = list(colors = slice_col, line = list(color = "#ffffff", width = 2)),
      textinfo = "percent", textposition = "inside", insidetextorientation = "horizontal",
      # luminance-picked ink: white on juvenile lime #9bd24a was 1.79:1 in dark mode.
      insidetextfont = list(color = label_ink(slice_col), size = 13),
      textfont = list(color = label_ink(slice_col), size = 13),
      hovertemplate = "<b>%{label}</b><br>%{value:,} individuals · %{percent:.0%} of handled<extra></extra>") %>%
      plotly::layout(title = list(text = "Life stage", font = list(color = if (is_dark()) "#c3cedd" else "#344049", size = 14)),
        paper_bgcolor = "rgba(0,0,0,0)", showlegend = TRUE,
        legend = list(orientation = "h", y = -0.05, x = 0.5, xanchor = "center", font = list(size = 11)),
        annotations = list(donut_center(sum(tab$n), "individuals")),
        hoverlabel = list(bgcolor = "rgba(12,35,75,0.96)", bordercolor = "#ffd24a",
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
      col = c(dcol("#5fb56a"), dcol("#43b8e8"), dcol("#38a8e8")))
    plot_ly(df, x = ~val, y = ~lab, type = "bar", orientation = "h",
      marker = list(color = ~col, line = list(color = "#ffffff", width = 1)),
      text = ~sprintf("%.1f eff.", val), textposition = "outside",
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
      else if (hn$even >= 0.75) "very even, captures are spread across many species"
      else if (hn$even >= 0.5)  "moderately even"
      else if (hn$even >= 0.3)  "uneven, a few species dominate the catch"
      else "highly skewed, one or two species dominate"
    tile <- function(v, lab, sub, col) div(class = "hill-tile", style = sprintf("--hc:%s", col),
      div(class = "hill-v", v), div(class = "hill-l", lab), div(class = "hill-s", sub))
    div(class = "hill-note",
      div(class = "hill-tiles",
        tile(hn$q0, "richness", "all species", dcol("#38a8e8")),
        tile(hn$q1, "common", "exp(Shannon)", dcol("#43b8e8")),
        tile(hn$q2, "dominant", "inv. Simpson", dcol("#5fb56a"))),
      div(class = "hill-even",
        bs_icon("bar-chart-steps"),
        HTML(sprintf(" Evenness <b>%s</b>: %s.",
                     ifelse(is.na(hn$even), "—", format(hn$even, nsmall = 2)), even_word))),
      div(class = "hill-foot",
        sprintf("From %s individuals across %s species.",
                format(hn$n_ind, big.mark = ","), hn$n_sp)))
  })

  output$plotTrend <- renderPlotly({
    d <- rv$data; req(d)
    sel <- input$plotTrendPlot %||% "all"
    ds <- d %>% dplyr::filter(!is.na(.data$tagID), !is.na(.data$scientificName), !is.na(.data$ym))
    if (!identical(sel, "all") && nzchar(sel)) ds <- ds[ds$plotID %in% sel, , drop = FALSE]
    if (nrow(ds) == 0) return(note_plot("No dated captures to chart for this plot", "\U0001F4C8"))
    # ONE panel: captures by species per month, summed across the chosen scope —
    # all plots (the site total) by default, or a single plot from the picker.
    # Replaces the old 8-panel small-multiples (one mini-chart per plot + a 25-line
    # legend) — same data, far less clutter; the dropdown drills into a plot.
    ds <- ds %>% dplyr::group_by(.data$scientificName, .data$ym) %>%
      dplyr::summarise(count = dplyr::n(), .groups = "drop")
    ds$date <- as.Date(paste0(ds$ym, "-01"))
    # keep species with a meaningful number of captures so the legend stays readable
    keep_sp <- ds %>% dplyr::group_by(.data$scientificName) %>%
      dplyr::summarise(tot = sum(.data$count), .groups = "drop") %>%
      dplyr::filter(.data$tot >= 5) %>% dplyr::pull(.data$scientificName)
    if (length(keep_sp) == 0) keep_sp <- unique(ds$scientificName)
    ds <- ds[ds$scientificName %in% keep_sp, ]
    pal <- species_pal()                 # mode-aware; recolors on light/dark toggle
    scope_lbl <- if (identical(sel, "all") || !nzchar(sel)) "all plots \U00B7 site total"
                 else paste0("plot ", sel)
    p <- plot_ly()
    for (s in sort(unique(ds$scientificName))) {
      sd <- ds[ds$scientificName == s, ]
      p <- p %>% add_trace(data = sd, x = ~date, y = ~count, type = "scatter",
        mode = "lines+markers", name = s,
        marker = list(size = 6, color = pal[[s]]), line = list(width = 2, color = pal[[s]]),
        hovertemplate = paste0("<b>", s, "</b><br>%{x|%b %Y}: %{y} captures<extra></extra>"))
    }
    # Both captions go through layout(annotations=...) (REPLACES wholesale), NOT
    # add_annotations()/ctx_anno() (which APPEND and so stack a fresh copy on every
    # filter re-render of this same plotly div — the documented accumulation trap).
    cap_col <- if (is_dark()) "#9fb0c4" else "#6b7a89"
    anns <- list(list(text = scope_lbl, x = 0, y = 1.03, xref = "paper", yref = "paper",
      xanchor = "left", yanchor = "bottom", showarrow = FALSE,
      font = list(color = cap_col, size = 11.5, family = "Rubik")))
    if (!is.null(rv$ctx))
      anns[[length(anns) + 1L]] <- list(text = rv$ctx, x = 1, y = 1.03, xref = "paper", yref = "paper",
        xanchor = "right", yanchor = "bottom", showarrow = FALSE,
        font = list(color = cap_col, size = 11, family = "Rubik"))
    plotly_theme(p) %>% plotly::layout(
      xaxis = list(title = ""),
      yaxis = list(title = "captures", rangemode = "tozero"),
      annotations = anns) %>%
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
        line = list(color = dcol("#43b8e8"), width = 3), marker = list(size = 8, color = dcol("#43b8e8")),
        connectgaps = FALSE, customdata = by_m$males,
        hovertemplate = "%{x}<br>%{y}% of adult males scrotal<br>(n = %{customdata} adult males)<extra></extra>") %>%
      add_trace(x = mlab, y = by_m$pf, type = "scatter", mode = "lines+markers", name = "reproductive females",
        line = list(color = "#c2255c", width = 3), marker = list(size = 8, color = "#c2255c"),
        connectgaps = FALSE, customdata = by_m$females,
        hovertemplate = "%{x}<br>%{y}% of adult females pregnant/lactating<br>(n = %{customdata} adult females)<extra></extra>")
    p <- plotly_theme(p) %>% plotly::layout(
      xaxis = list(title = "", categoryorder = "array", categoryarray = month.abb),
      yaxis = list(title = "% of that sex reproductively active", range = c(0, 100)),
      hovermode = "x unified", margin = list(t = 44)) %>% ctx_anno()
    if (!is.null(es)) p <- p %>% plotly::layout(yaxis2 = env_axis_spec(es$layer, show = TRUE))
    p
  })

  # ---- body-size profile (violin per species, "Position DNA") ------------
  # per-species body-measurement profile (weight/hindfoot dense; tail/ear sparse)
  output$speciesMeas <- DT::renderDT({
    d <- rv$data; req(d)
    m <- species_measurements(d)
    if (is.null(m) || !nrow(m)) return(DT::datatable(data.frame(Note = "No measured animals"),
      rownames = FALSE, options = list(dom = "t")))
    # headline range is now the robust p5–p95 envelope; an amber ⚠ appears when a
    # measure contains values flagged as possible data-entry errors (beyond
    # median±5·MAD). The flagged value is KEPT — its raw min–max rides in the
    # tooltip for field QC — but it's excluded from the median and the p5–p95 range.
    cell <- function(med, lo, hi, n, nflag, rmin, rmax, unit, sp, meas, min_n = 3) {
      warn <- ifelse(!is.na(nflag) & nflag > 0,
        sprintf(" <span class='mz-flag' style='cursor:pointer' onclick=\"Shiny.setInputValue('showOutliers','%s||%s||'+Math.random(),{priority:'event'})\" title='%s value(s) beyond the plausible adult range (raw %s–%s %s). Click to inspect &amp; verify. Kept in the data but excluded from the median and the 5th–95th-percentile range.'>&#9888;</span>",
                sp, meas, format(nflag, big.mark = ","), rmin, rmax, unit),
        "")
      ifelse(is.na(med) | n < min_n, "<span class='muted'>—</span>",
        sprintf("<b>%s</b> <span class='mz-rng'>[%s–%s]</span> <span class='mz-n'>n=%s</span>%s",
                med, lo, hi, format(n, big.mark = ","), warn))
    }
    df <- tibble::tibble(
      Species = sprintf("<span class='ind-cell'><span class='ind-emoji'>%s</span><span class='ind-id'><i>%s</i></span></span>", m$emoji, m$scientificName),
      Indiv = m$n_ind,
      `Weight (g)`    = cell(m$w_med,  m$w_lo,  m$w_hi,  m$w_n,  m$w_nflag,  m$w_min,  m$w_max,  "g",  m$scientificName, "weight"),
      `Hindfoot (mm)` = cell(m$hf_med, m$hf_lo, m$hf_hi, m$hf_n, m$hf_nflag, m$hf_min, m$hf_max, "mm", m$scientificName, "hindfoot"),
      `Tail (mm)`     = cell(m$tl_med, m$tl_lo, m$tl_hi, m$tl_n, m$tl_nflag, m$tl_min, m$tl_max, "mm", m$scientificName, "tail"),
      `Ear (mm)`      = cell(m$el_med, m$el_lo, m$el_hi, m$el_n, m$el_nflag, m$el_min, m$el_max, "mm", m$scientificName, "ear"))
    DT::datatable(df, escape = FALSE, rownames = FALSE, selection = "none",
      class = "compact stripe hover nowrap leader-dt",
      options = list(pageLength = 12, dom = "tip", scrollX = TRUE,
        columnDefs = list(list(className = "dt-center", targets = 1:5)),
        language = list(search = "", searchPlaceholder = "filter species…")))
  })

  # QC inspector — clicking a measurement's ⚠ lists the exact flagged captures so
  # a field tech can verify the record (the value stays in the data either way).
  observeEvent(input$showOutliers, {
    d <- rv$data; req(d)
    parts <- strsplit(input$showOutliers, "\\|\\|")[[1]]
    if (length(parts) < 2) return()
    sp <- parts[1]; measure <- parts[2]
    fc <- flagged_measure_captures(d, sp, measure)
    if (is.null(fc) || !nrow(fc)) { showNotification("No flagged values to inspect.", type = "message"); return() }
    unit <- if (measure == "weight") "g" else "mm"
    mlab <- c(weight = "weight", hindfoot = "hind-foot length",
              tail = "tail length", ear = "ear length")[[measure]]
    showModal(modalDialog(
      title = tagList(bs_icon("exclamation-triangle-fill"), sprintf(" Possible %s errors · %s", mlab, sp)),
      size = "m", easyClose = TRUE, footer = modalButton("Close"),
      p(class = "inspect-note", HTML(sprintf("Adult %s median here is <b>%s %s</b>. The records below fall far outside the plausible adult range (beyond median ± 5×MAD), most likely data-entry errors. They are <b>kept in the data</b> but excluded from the median and the 5th–95th-percentile range; verify them against the field sheets.",
        mlab, round(fc$median[1], 1), unit))),
      tags$table(class = "inspect-tbl",
        tags$thead(tags$tr(lapply(c("Tag", "Date", "Plot", sprintf("Value (%s)", unit), "Sex"), tags$th))),
        tags$tbody(lapply(seq_len(nrow(fc)), function(i)
          tags$tr(
            tags$td(tags$a(href = "#", class = "qc-drill",
              style = "cursor:pointer;color:#1f78c4;font-weight:600;text-decoration:none;",
              onclick = sprintf("Shiny.setInputValue('qcDrillTag','%s',{priority:'event'});return false;", fc$tag[i]),
              title = "Open this animal's dossier to evaluate the record",
              fc$short[i], HTML("&nbsp;"), bsicons::bs_icon("box-arrow-up-right"))),
            tags$td(format(fc$date[i], "%Y-%m-%d")),
            tags$td(fc$plotID[i]), tags$td(tags$b(fc$value[i])),
            tags$td(ifelse(is.na(fc$sex[i]), "?", fc$sex[i]))))))
    ))
  })

  # QC-flag lifecycle · stage 4 (drill): tapping a flagged tag in the QC modal
  # jumps straight to that animal's dossier so the reviewer can evaluate the record
  # in context (its measurement track, body-size position, and full capture history,
  # where the same value is now marked). Reuses pick_individual (navigate = TRUE).
  observeEvent(input$qcDrillTag, {
    tag <- input$qcDrillTag
    if (is.null(tag) || !nzchar(tag)) return()
    removeModal()
    pick_individual(tag)
  }, ignoreInit = TRUE)

  # ---- minimum known lifespan (a right-censored FLOOR, not a lifespan) -----
  # Longest age-at-last-capture among animals first caught young; muted tone so a
  # tentative floor never reads as a confident "good" finding. Captive AnAge
  # maxima shown for scale (always several times the wild value).
  output$lifespanBanner <- renderUI({
    lb <- rv$lb
    if (is.null(lb)) return(NULL)
    ml <- min_known_lifespan(lb)
    if (is.null(ml) || !nrow(ml))
      return(insight_banner("hourglass-split", tone = "muted", HTML(paste0(
        "Not enough recaptured individuals here to say how long animals are confirmed alive; ",
        "it needs at least 5 individuals per species, each caught 3+ times."))))
    rows <- paste0(
      "<div class='lsp-row'>",
        "<span class='lsp-sp'>", ml$emoji, " <i>", ml$scientificName, "</i></span>",
        "<span class='lsp-val'>&ge;", ml$min_known_yr, " yr</span>",
        "<span class='lsp-n'>n=", ml$n_qual, "</span>",
        ifelse(is.na(ml$captive_max_yr), "",
               paste0("<span class='lsp-cap'>captive max ~", ml$captive_max_yr, " yr</span>")),
      "</div>", collapse = "")
    insight_banner("hourglass-split", tone = "muted", HTML(paste0(
      "<b>Longest we confirmed an individual alive</b>: how old the single longest-tracked animal ",
      "was when last caught (caught 3+ times; NEON keeps a tag for the animal's life, so these are real ",
      "recapture spans). A <b>floor</b>, not a lifespan: animals still alive, or that left the trapping ",
      "grid, aren't counted (absence isn't death), the record only spans the years sampled here, and ",
      "species we trap more often reach higher floors just from more chances, so read each as its own ",
      "floor, not a ranking. The captive maxima shown for scale run several times these wild figures.",
      "<div class='lsp-list'>", rows, "</div>")))
  })

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
    pal <- species_pal()                 # mode-aware; recolors on light/dark toggle

    p <- plot_ly()
    for (s in ord) {
      sub <- w[w$scientificName == s, ]
      col <- pal[[s]] %||% acc()
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
          marker = list(symbol = "diamond", size = 15, color = dcol("#e0b43a"),
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
  output$mnkaInsight <- renderUI({
    d <- rv$data; req(d)
    insight_banner("people-fill", tone = "navy",
      HTML("<b>MNKA</b> (per plot, left axis) counts individuals known alive each month; the dotted <b>site-total catch-per-effort</b> (right axis) is a different quantity: they're on separate scales and can move apart, so read each on its own axis."))
  })

  output$mnkaPlot <- renderPlotly({
    d <- rv$data; req(d)
    mn <- mnka_series(d)
    if (is.null(mn) || nrow(mn) == 0) return(note_plot("Not enough data for MNKA", "\U0001F465"))
    pal <- species_pal()                 # mode-aware (kept for parity; loop colors by plot)
    plots <- sort(unique(mn$plotID))
    # per-PLOT ramp via the SHARED collision-safe anchors (helpers.R): head(N) up to
    # 14, so distinct plots never blend into near-identical hues (the old
    # colorRampPalette-over-8 collapsed to 12 RGB-units apart at 12+ series). Keeps
    # the MNKA ramp in lockstep with make_species_pal so the two never drift.
    plot_cols <- categorical_colors(max(length(plots), 3), dark = is_dark())
    p <- plot_ly()
    # environmental overlay FIRST so it reads as soft context behind the lines
    es <- env_sel()
    if (!is.null(es))
      p <- add_env_overlay(p, es$env, es$layer, es$lag, yaxis = "y3",
                           xlim = range(mn$date, na.rm = TRUE), demo = es$demo)
    max_tn <- max(mn$trap_nights, na.rm = TRUE)
    for (i in seq_along(plots)) {
      pl <- plots[i]; dd <- mn[mn$plotID == pl, ]
      # marker size tracks trap-EFFORT: a low-effort month reads as a smaller,
      # more tentative point (MNKA isn't effort-normalized, so this is the only
      # honest "how hard did we look that month" cue).
      msz <- 4 + 7 * (dd$trap_nights / max_tn)
      p <- p %>% add_trace(data = dd, x = ~date, y = ~mnka, type = "scatter", mode = "lines+markers",
        name = pl, legendgroup = pl, line = list(color = plot_cols[i], width = 2),
        marker = list(size = msz, color = plot_cols[i]), customdata = round(dd$trap_nights),
        hovertemplate = paste0(pl, "<br>%{x|%b %Y}<br>MNKA: %{y} · %{customdata} trap-nights<extra></extra>"))
    }
    # site-total CPUE companion line on a secondary axis
    site <- mn %>% dplyr::group_by(.data$date) %>%
      dplyr::summarise(cap = sum(.data$captures), tn = sum(.data$trap_nights), .groups = "drop") %>%
      dplyr::mutate(cpue = round(100 * .data$cap / .data$tn, 1))
    p <- p %>% add_trace(data = site, x = ~date, y = ~cpue, yaxis = "y2", type = "scatter",
      mode = "lines", name = "site total, per 100 trap-nights",
      line = list(color = "rgba(31,42,48,0.55)", width = 2, dash = "dot"),
      hovertemplate = "%{x|%b %Y}<br>%{y} captures per 100 trap-nights<extra></extra>")
    # Shaded "no sampling" bands behind long pauses (>~4 months between sampled
    # months — e.g. the COVID-2020 hiatus): the line stays continuous (clean) but
    # a labelled band names the gap, so a bridged segment is never read as a real
    # trend through months that weren't surveyed.
    sdates <- sort(unique(mn$date))
    gi <- if (length(sdates) > 1) which(as.numeric(diff(sdates)) > 124) else integer(0)
    bands <- lapply(gi, function(k) list(type = "rect", xref = "x", yref = "paper",
      x0 = sdates[k], x1 = sdates[k + 1], y0 = 0, y1 = 1, layer = "below",
      fillcolor = "rgba(120,130,140,0.13)", line = list(width = 0)))
    # WCAG 1.4.3: this is the ONLY on-chart label naming a data gap — it must be
    # legible, not a whisper. dcol() keeps the dark-mode grey but swaps to a
    # ~4.9:1 slate on the light panel; nudged 10->11px as the sole affordance.
    band_lbls <- lapply(gi, function(k) list(text = "no sampling",
      x = sdates[k] + (sdates[k + 1] - sdates[k]) / 2, y = 0.5, xref = "x", yref = "paper",
      showarrow = FALSE, font = list(color = dcol("#9aa7b4"), size = 11, family = "Rubik")))
    p <- plotly_theme(p) %>% plotly::layout(
      yaxis  = list(title = "MNKA (individuals known alive)", color = acc(), rangemode = "tozero"),
      # #7a8896 was 3.63:1 on white (secondary-axis title/spike/legend note) — under
      # the text gate; muted_ink() clears 4.5:1 on white AND stays legible on navy.
      yaxis2 = list(title = "captures per 100 trap-nights (site total)", color = muted_ink(),
                    overlaying = "y", side = "right", gridcolor = "rgba(0,0,0,0)", rangemode = "tozero"),
      xaxis  = list(title = "", showspikes = TRUE, spikemode = "across",
                    spikethickness = 1, spikecolor = "#7a8896", spikedash = "dot"),
      hovermode = "x", margin = list(t = 44), shapes = bands,
      annotations = c(list(list(text = "⋯ dotted = SITE-TOTAL catch-per-effort · right axis, its own scale (don't compare its height to the coloured per-plot lines)",
        x = 0, y = 1.08, xref = "paper", yref = "paper", xanchor = "left", yanchor = "bottom",
        showarrow = FALSE, font = list(color = muted_ink(), size = 11, family = "Rubik"))), band_lbls)) %>% ctx_anno()
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
    band_name <- if (isTRUE(sa$unstable)) "Chao1 interval (wide, unstable)"
                 else sprintf("Chao1 95%% CI (%d–%d)", sa$chao_lo, sa$chao_hi)
    p <- plot_ly() %>%
      add_trace(x = cv$bouts, y = cv$hi, type = "scatter", mode = "lines",
        line = list(width = 0), showlegend = FALSE, hoverinfo = "skip") %>%
      add_trace(x = cv$bouts, y = cv$lo, type = "scatter", mode = "lines", fill = "tonexty",
        fillcolor = "rgba(22,56,110,0.14)", line = list(width = 0),
        name = "±1 SD (resampling)", hoverinfo = "skip") %>%
      add_trace(x = cv$bouts, y = cv$richness, type = "scatter", mode = "lines+markers",
        name = "species found", line = list(color = acc(), width = 3),
        marker = list(size = 6, color = acc()),
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
        line = list(color = dcol("#fb8a7e"), width = 1.5, dash = "dash"), hoverinfo = "skip")
    # Short, left-anchored, AND on its own line ABOVE the right-anchored
    # "SITE · years" corner note (ctx sits at y=1.03; this rides higher at y=1.13)
    # so the two never collide even on a narrow phone-width card. Just the headline
    # count — the Chao1 estimate, its interval, and the (wide/unstable) flag all
    # live in the legend (band_name + the dashed-line name), and the doubleton
    # caveat is in the info popover, so the subtitle stays short enough never to clip.
    anno_txt <- sprintf("observed %d species", sa$sobs)
    plotly_theme(p) %>% plotly::layout(
      xaxis = list(title = "trapping bouts (sampling order, resampled)"),
      yaxis = list(title = "cumulative species", range = c(0, cap + 1), rangemode = "tozero"),
      margin = list(l = 50, r = 30, t = 84, b = 40),
      annotations = list(list(text = anno_txt,
        x = 0, y = 1.13, xref = "paper", yref = "paper", xanchor = "left", yanchor = "bottom",
        showarrow = FALSE, font = list(color = muted_ink(), size = 11)))) %>% ctx_anno()
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
    if (is.null(cc)) return(NULL)
    pct <- function(x) if (is.na(x)) "—" else paste0(round(100 * x), "%")
    chip <- function(v, lab, col) div(class = "detect-chip", style = sprintf("--dc:%s", col),
      div(class = "detect-v", v), div(class = "detect-l", lab))
    # single-night (k=1) share — these bouts are MNKA/CPUE-only BY DESIGN (no
    # within-bout recapture to detection-correct), so a sparse N̂ series reads as
    # a sampling-design fact, not missing data.
    sn <- cc$n_single %||% NA_integer_
    sn_pct <- if (!is.na(sn) && (cc$n_bouts %||% 0) > 0) round(100 * sn / cc$n_bouts) else NA_real_
    # No estimable bouts: still surface the single-night share so an all-k=1 site
    # reads "index-only by design" — the case where that message matters most.
    if (is.null(cc$series) || nrow(cc$series) == 0) {
      if (is.na(sn_pct)) return(NULL)
      return(tagList(
        div(class = "detect-head",
          chip(paste0(sn_pct, "%"), "single-night (index-only)", dcol("#9aa7b4"))),
        div(style = "font-size:.82rem; opacity:.72; margin-top:6px; max-width:48ch;",
          "Every bout here is single-night, so abundance is index-only (MNKA/CPUE) by design: there's no within-bout recapture to detection-correct.")))
    }
    lead <- if (!is.na(cc$mean_detect))
      insight_banner("incognito", tone = "navy",
        HTML(sprintf("Traps caught about <span class='ci-hero'>%s</span> of the animals present per bout: the gap between the navy estimate and the grey known-alive line is everything they missed.",
          paste0(round(100 * cc$mean_detect), "%")))) else NULL
    tagList(lead,
      div(class = "detect-head",
        chip(pct(cc$mean_p),      "per-night detection (p̂)", dcol("#38a8e8")),
        chip(pct(cc$mean_detect), "of population caught / bout", dcol("#43b8e8")),
        chip(cc$n_estimable,      sprintf("estimable bouts (of %d)", cc$n_bouts), dcol("#5fb56a")),
        if (!is.na(sn_pct))
          chip(paste0(sn_pct, "%"), "single-night (index-only)", dcol("#9aa7b4"))))
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
        name = "MNKA (known alive)", line = list(color = dcol("#8a97a8"), width = 2),
        marker = list(size = 5, color = dcol("#8a97a8")),
        hovertemplate = "%{x|%b %Y}<br>MNKA %{y}<extra></extra>") %>%
      add_trace(data = s, x = ~date, y = ~N, type = "scatter", mode = "lines+markers",
        name = "estimated abundance (N̂)", line = list(color = acc(), width = 3),
        marker = list(size = 7, color = acc()),
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
      HTML(sprintf(" Across estimable bouts, the corrected estimate runs about <b>%s%%</b> above the raw known-alive count: the animals the traps missed. Estimates are summed across grids per month; months with too few recaptures are omitted.",
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

  # ---- tidy analysis-ready exports (FAIR) ---------------------------------
  # A small downloads suite for reproducibility: the cleaned trap-event/handling table,
  # the monthly MNKA/CPUE/N̂ series, and a column codebook. Lives in the About
  # tab (a deliberate navigation, never an always-on wall) so the default view
  # stays clean. Filenames are site-stamped so a folder of them stays legible.
  export_slug <- function() {
    gsub("(^-|-$)", "", gsub("[^A-Za-z0-9]+", "-", rv$label %||% "site"))
  }

  # SINGLE SOURCE OF TRUTH for the captures CSV: the ordered keep-vector AND its
  # codebook in one place, so the column list the export emits and the dictionary
  # that documents it can never drift apart (the codebook iterates this list).
  # Each entry: units = what the column is + its unit; note = NA-semantics / caveat.
  CAPTURE_COLS <- list(
    siteID         = list(units = "NEON 4-letter site code", note = ""),
    plotID         = list(units = "NEON plot ID", note = ""),
    date           = list(units = "ISO date (YYYY-MM-DD)", note = "the capture/handling date (collectDate, day precision)"),
    ym             = list(units = "month (YYYY-MM)", note = "derived from date; the grain the monthly series aggregates to"),
    year           = list(units = "calendar year (YYYY)", note = "derived from date"),
    tagID          = list(units = "ear-tag ID (unique within site, lifelong)", note = "NA = an untagged/empty-trap row; NEON never recycles a tag within a site"),
    short          = list(units = "short species label", note = "derived display name (genus initial + species), for compact tables"),
    taxonID        = list(units = "NEON taxon code (join key)", note = "NEON DP1.10072.001 taxonomy; the stable code behind scientificName"),
    scientificName = list(units = "Latin binomial", note = "NEON DP1.10072.001 taxonomy; genus-only 'X sp.' and ambiguous 'A/B' kept as recorded"),
    taxonRank      = list(units = "NEON taxonomic rank", note = "species / subspecies / speciesGroup / genus / family — NA for unranked records"),
    nativeStatusPlaceholder = NULL,  # placeholder removed below; keeps explicit ordering readable
    nativeStatusCode = list(units = "NEON native-status code (N/I/etc.)", note = "as published by NEON; NA where unassigned"),
    sex            = list(units = "M / F / U", note = "7.6% NA (not sexed)"),
    lifeStage      = list(units = "juvenile / subadult / adult", note = "NA where life stage was not assessed"),
    weight         = list(units = "grams", note = "NA = unmeasured: recaptures are often not re-weighed, and empty-trap rows carry no animal (~23% NA overall; convention, not error)"),
    hindfootLength = list(units = "millimetres", note = "NA = unmeasured (~28% NA overall; same convention as weight)"),
    tailLength     = list(units = "millimetres", note = "sparsely recorded; mostly NA (NEON rarely measures tail length)"),
    earLength      = list(units = "millimetres", note = "sparsely recorded; mostly NA (NEON rarely measures ear length)"),
    totalLength    = list(units = "millimetres", note = "sparsely recorded; mostly NA (NEON almost never records total body length, which is why the Chonk Index is a within-species percentile, not a Scaled Mass Index)"),
    trapCoordinate = list(units = "trap grid cell (e.g. A1)", note = "canonical cells are A-J x 1-10; AX-JX, X1-X10, and XX are non-unique placeholders retained with row-level uncertain effort"),
    recapture      = list(units = "Y/N", note = "NEON cross-bout recapture flag; carries cross-BOUT history, within-bout status is recomputed for the estimators"),
    fate           = list(units = "NEON fate code (released/dead/etc.)", note = "disposition of the animal at handling; NA where not recorded"),
    trapStatus     = list(units = "NEON trap-status code", note = "e.g. '5 - capture', '1 - trap not set'; the raw NEON status string"),
    trap_event     = list(units = "reviewed physical-event key", note = "year|nightuid|plotID|canonical coordinate; placeholder coordinates use an explicit row-level key"),
    trap_effort    = list(units = "trap-nights (1; sprung/disturbed = 0.5; not-set = 0)", note = "allocated once per reviewed physical trap event; secondary animal rows from one multi-capture trap carry 0 so summing this column does not double-count effort"),
    trap_effort_rule = list(units = "effort-resolution rule", note = "canonical-single / canonical-multi-capture-one-trap / reviewed-double-trap-rows / placeholder-row-level"),
    trap_event_source_rows = list(units = "source rows", note = "number of source rows resolved into this event; repeated on every row for audit"),
    trap_effort_owner = list(units = "TRUE / FALSE", note = "TRUE where this row owns an allocated effort contribution; multi-capture companion rows remain captures but do not own another trap-night"),
    is_capture     = list(units = "TRUE / FALSE", note = "TRUE if an animal was handled (has a tagID); FALSE for empty/not-set trap rows"),
    remarks        = list(units = "free text", note = "NEON field remarks; usually NA")
  )
  CAPTURE_COLS[["nativeStatusPlaceholder"]] <- NULL   # drop the readability placeholder
  CAPTURE_KEEP <- names(CAPTURE_COLS)

  # (a) cleaned site trap-event/handling table — all source rows, including
  #     empty/not-set opportunities, with NEON-native names + reviewed effort/IDs.
  output$dlCapturesCsv <- downloadHandler(
    filename = function() sprintf("NEON-SmallMammal_captures_%s_%s.csv",
                                  export_slug(), format(Sys.Date(), "%Y%m%d")),
    content = function(file) {
      d <- rv$data; req(d)
      keep <- intersect(CAPTURE_KEEP, names(d))
      out <- d[, keep, drop = FALSE]
      out <- out[order(out$date, out$plotID, out$tagID), , drop = FALSE]
      utils::write.csv(out, file, row.names = FALSE, na = "")
    },
    contentType = "text/csv"
  )

  # (b) monthly site-total series: MNKA + CPUE (from mnka_series, summed to the
  #     site) joined to the detection-corrected N̂ / p̂ (from the closed-capture
  #     series) by month. The exact numbers behind the Population & Detection tabs.
  output$dlSeriesCsv <- downloadHandler(
    filename = function() sprintf("NEON-SmallMammal_monthly-series_%s_%s.csv",
                                  export_slug(), format(Sys.Date(), "%Y%m%d")),
    content = function(file) {
      d <- rv$data; req(d)
      ms <- mnka_series(d)
      validate(need(!is.null(ms) && nrow(ms) > 0, "No estimable monthly series for this site."))
      site_m <- ms %>% dplyr::group_by(.data$ym) %>%
        dplyr::summarise(
          mnka        = sum(.data$mnka, na.rm = TRUE),
          captures    = sum(.data$captures, na.rm = TRUE),
          trap_nights = round(sum(.data$trap_nights, na.rm = TRUE), 1),
          cpue_per100tn = round(100 * sum(.data$captures, na.rm = TRUE) /
                                  sum(.data$trap_nights, na.rm = TRUE), 1),
          n_plots     = dplyr::n_distinct(.data$plotID),
          .groups = "drop")
      cc <- tryCatch(detect_cc(), error = function(e) NULL)
      if (!is.null(cc) && !is.null(cc$series) && nrow(cc$series) > 0) {
        dc <- cc$series %>% dplyr::transmute(.data$ym,
          Nhat = round(.data$N, 1), Nhat_lo = round(.data$lo, 1),
          Nhat_hi = ifelse(is.finite(.data$hi), round(.data$hi, 1), NA_real_),
          p_hat = .data$p)
        site_m <- dplyr::left_join(site_m, dc, by = "ym")
      } else {
        site_m$Nhat <- NA_real_; site_m$Nhat_lo <- NA_real_
        site_m$Nhat_hi <- NA_real_; site_m$p_hat <- NA_real_
      }
      site_m <- data.frame(siteID = mode_chr(d$siteID), site_m[order(site_m$ym), ],
                           stringsAsFactors = FALSE)
      utils::write.csv(site_m, file, row.names = FALSE, na = "")
    },
    contentType = "text/csv"
  )

  # (c) column codebook — units + the captures-vs-handled-and-measured NA
  #     convention, so a downstream analyst can read the two CSVs above without
  #     guessing. The weight ~23% / hindfoot ~28% NA is unmeasured recaptures &
  #     empty-trap rows, NOT data error — stated here in the metadata, once.
  output$dlCodebookCsv <- downloadHandler(
    filename = function() sprintf("NEON-SmallMammal_codebook_%s.csv",
                                  format(Sys.Date(), "%Y%m%d")),
    content = function(file) {
      # Captures rows are built by iterating the EXACT keep-vector the captures
      # CSV emits (CAPTURE_COLS), so the dictionary can never drift from the file.
      cap <- data.frame(
        file   = "captures",
        column = CAPTURE_KEEP,
        units  = vapply(CAPTURE_COLS, function(x) x$units, character(1)),
        note   = vapply(CAPTURE_COLS, function(x) x$note,  character(1)),
        stringsAsFactors = FALSE, row.names = NULL)
      # Monthly-series rows (the second exported CSV) documented inline.
      ser <- data.frame(
        file = "monthly-series",
        column = c("siteID","ym","mnka","captures","trap_nights","cpue_per100tn",
                   "Nhat","Nhat_lo","Nhat_hi","p_hat","n_plots"),
        units = c(
          "NEON 4-letter code","month (YYYY-MM)",
          "Minimum Number Known Alive (Krebs 1966), summed across plots",
          "handling events that month","trap-nights of effort that month",
          "captures per 100 trap-nights (within-site index)",
          "detection-corrected abundance (Schnabel/Chapman); blank where un-estimable",
          "lower confidence bound for detection-corrected abundance",
          "upper confidence bound for detection-corrected abundance",
          "per-night detection probability (Model M0)","distinct plots sampled"),
        note = c(
          "","","an INDEX, not a census; counts animals known alive, not corrected for detection",
          "raw count of handling events that month (a numerator, no denominator)","","captures per 100 trap-nights, a within-site relative index, NOT a cross-site density (detection differs by biome)",
          "blank for single-night / low-recapture months that can't be detection-corrected (about half of bouts are single-night by design)",
          "blank whenever Nhat is un-estimable; interval method matches the detection estimator",
          "blank whenever Nhat is un-estimable or the upper interval is not finite",
          "0–1; deserts run high, closed-canopy temperate sites low",""),
        stringsAsFactors = FALSE, row.names = NULL)
      # Provenance / license row (NEON CC BY 4.0) — first row of the codebook so
      # any analyst who opens the dictionary sees the source + attribution terms.
      lic <- data.frame(
        file   = "_source",
        column = "NEON DP1.10072.001",
        units  = "license",
        note   = "Source: NEON DP1.10072.001, CC BY 4.0 (https://creativecommons.org/licenses/by/4.0/); aggregated and derived by this app.",
        stringsAsFactors = FALSE, row.names = NULL)
      cb <- rbind(lic, cap, ser)
      utils::write.csv(cb, file, row.names = FALSE, na = "")
    },
    contentType = "text/csv"
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
        p("Each site ships as a pre-built, compressed bundle. An automated job prepares and validates a new candidate ", tags$b("late on the first Saturday night of each month"), " (around 11 pm Arizona time). It reaches production only after its exact site, schema, index, checksum, package, and offline-start gates pass and the review PR is intentionally merged."),
        p("The public app is bundle-only for fast, reproducible starts. Local builds that deliberately install the optional NEON fetch tooling can enable ", tags$b("Include provisional"), " to inspect newer, still-unpublished records; that control stays hidden in production.")),
      div(class = "about-card",
        h4(bs_icon("calculator"), " The Chonk Index · honest version"),
        p("It would be tempting to dress this up as a Scaled Mass Index (Peig & Green 2009), but in these desert rodents hind-foot length barely scales with mass (r ≈ 0.15 for kangaroo & pocket mice) and NEON almost never records total body length, so a standardized condition index would just rank measurement noise."),
        p("Instead, the Chonk score is an honest ", tags$b("adult weight percentile within species"), ", a true statement (\"heavy for its kind\"). The ", tags$b("body-size map"), " on the dossier shows the actual weight × hind-foot cloud, and draws a size–mass fit line ", tags$em("only"), " for species where the relationship is real (n ≥ 15, |r| ≥ 0.3)."),
        p(class = "caveat", bs_icon("exclamation-triangle"), " Computed on adults only; species with < 4 measured adults show ", tags$b("—"), ".")),
      div(class = "about-card",
        h4(bs_icon("clipboard-data"), " Metrics"),
        tags$ul(
          tags$li(tags$b("Career span"), ": days between first and last capture (these desert rodents genuinely live 1–3.5 yr, so long careers are real). Only a history that can't be one animal, the same tag at two plots on a single day, or a span beyond any wild lifespan, is flagged ", tags$b("verify tag"), "."),
          tags$li(tags$b("Roam radius"), ": mean displacement of captures from the trap-grid centroid (a grid-bounded dispersion index, not a true home-range area)."),
          tags$li(tags$b("Max move"), ": the largest distance between any two captures (MDM)."),
          tags$li(tags$b("MNKA"), ": Minimum Number Known Alive (Krebs 1966): a transparent abundance ", tags$em("index"), ", shown with captures / 100 trap-nights (CPUE)."),
          tags$li(tags$b("Species accumulation"), ": richness vs trapping effort (Gotelli & Colwell 2001) with a Chao1 estimate of total richness."),
          tags$li(tags$b("Rarity"), ": a playful tier from total captures; it tracks trappability & residency, not ecological rarity."))),
      div(class = "about-card",
        h4(bs_icon("graph-up-arrow"), " Environmental drivers"),
        p("On the Population tab you can overlay a co-located NEON product, such as precipitation, air temperature, or plant flowering / green-up / fruiting, and the app reports which driver this population tracks best: a ", tags$b("deseasonalized Pearson correlation"), " scanned across 0–12-month lags, so the signal is year-to-year anomalies, not a shared summer peak."),
        p("The ", tags$b("r"), " value runs −1…+1 (strength + direction); it is ", tags$b("not"), " the percentage of the population explained. Square it (r²) for that. Driver colours are made to read naturally and colour-blind-safe: warm for a positive temperature link, cool for an inverse one; green vs. brown for vegetation."),
        p(class = "caveat", bs_icon("exclamation-triangle"), " Correlation, not cause: drivers co-vary, and scanning many lags can flag a match by chance, so read it as a lead to investigate.")),
      div(class = "about-card",
        h4(bs_icon("share-fill"), " Between-grid movement"),
        p("The Plot map can overlay ", tags$b("recapture connectivity"), ": curved arcs linking trapping grids where the same tagged animals were recaptured, thicker where more individuals made that move. It shows site fidelity vs. inter-grid movement that the per-grid dots can't."),
        p(class = "caveat", bs_icon("exclamation-triangle"), " Mark-recapture, ", tags$b("not"), " telemetry: an arc means \"caught here, then there,\" not a tracked route, and a long gap between the two captures just means the animal went undetected in between.")),
      div(class = "about-card",
        h4(bs_icon("download"), " Download the data"),
        p("Take this site's records as tidy, analysis-ready CSVs, with every column documented in the codebook so they're reproducible without guessing."),
        div(class = "about-dl",
          downloadButton("dlCapturesCsv", tagList(bs_icon("table"), " Site capture table (CSV)"),
                         class = "smt-clear-btn"),
          downloadButton("dlSeriesCsv", tagList(bs_icon("graph-up"), " Monthly MNKA / CPUE / N̂ series (CSV)"),
                         class = "smt-clear-btn"),
          downloadButton("dlCodebookCsv", tagList(bs_icon("book"), " Column codebook (CSV)"),
                         class = "smt-clear-btn")),
        p(class = "caveat", style = "margin-top:10px", bs_icon("info-circle"),
          " A blank measurement (weight ~23%, hind foot ~28% of rows) is an ", tags$b("unmeasured recapture or empty-trap row"),
          ", not an error; recaptures are often not re-weighed. The codebook states each column's units and this NA convention.")),
      div(class = "about-card about-suite-card",
        h4(bs_icon("stars"), " Explore the NEON Explorer Suite"),
        p("Small Mammals is one part of a ten-app ecology suite. Open a companion cover page to follow the cascade from environmental conditions through plants and animal communities."),
        div(class = "suite-app-grid",
          tags$a(class = "suite-app-link", href = "https://tgilbert14.github.io/NEON-Driver-Cascade/", target = "_blank", rel = "noopener", "Driver Cascade"),
          tags$a(class = "suite-app-link", href = "https://tgilbert14.github.io/NEON-Breeding-Birds/", target = "_blank", rel = "noopener", "Breeding Birds"),
          tags$a(class = "suite-app-link", href = "https://tgilbert14.github.io/NEON-Ground-Beetle-Tracker/", target = "_blank", rel = "noopener", "Ground Beetles"),
          tags$a(class = "suite-app-link", href = "https://tgilbert14.github.io/NEON-Plant-Diversity/", target = "_blank", rel = "noopener", "Plant Diversity"),
          tags$a(class = "suite-app-link", href = "https://tgilbert14.github.io/NEON-Plant-Phenology-Explorer/", target = "_blank", rel = "noopener", "Plant Phenology"),
          tags$a(class = "suite-app-link", href = "https://tgilbert14.github.io/NEON-Vegetation-Structure-Explorer/", target = "_blank", rel = "noopener", "Vegetation Structure"),
          tags$a(class = "suite-app-link", href = "https://tgilbert14.github.io/NEON-WaterChemistry-Analyte-Viewer-App/", target = "_blank", rel = "noopener", "Water Chemistry"),
          tags$a(class = "suite-app-link", href = "https://tgilbert14.github.io/NEON-Mosquito-Pulse/", target = "_blank", rel = "noopener", "Mosquito Pulse"),
          tags$a(class = "suite-app-link", href = "https://tgilbert14.github.io/NEON-My-Little-Inverts/", target = "_blank", rel = "noopener", "My Little Inverts"))),
      div(class = "about-card",
        h4(bs_icon("exclamation-diamond"), " Caveats"),
        p("NEON keeps a tag on one animal for life and doesn't recycle tag numbers (a number is unique within a site), so a multi-year capture career is a real long-lived individual, not a tag mix-up; we flag only the rare history that can't be one animal (e.g. the same tag at two plots on a single day). A trap that caught nothing means \"not detected,\" not \"absent.\" This is a data-exploration toy, not an authoritative population analysis, but the metrics are built to be defensible."),
        p("Reviewed for scientific soundness with input from a wildlife-monitoring methods audit (Peig & Green 2009; Krebs 1966; Gotelli & Colwell 2001; NEON DP1.10072.001 User Guide)."),
        p(bs_icon("envelope"), " ", tags$a(href = "mailto:desertdatalabs@gmail.com", "desertdatalabs@gmail.com"),
          " · ", tags$a(href = "https://data.neonscience.org/data-products/DP1.10072.001",
                        target = "_blank", "NEON data product"))),
      div(class = "about-card",
        h4(bs_icon("award"), " Data attribution & license"),
        p(class = "caveat",
          "Built with data from the National Ecological Observatory Network (NEON), a U.S. National Science Foundation program operated by Battelle. NEON data are provided under a Creative Commons Attribution 4.0 International (CC BY 4.0) license (",
          tags$a(href = "https://creativecommons.org/licenses/by/4.0/", target = "_blank", "creativecommons.org/licenses/by/4.0"),
          "). This app aggregates and derives summary metrics from the raw NEON data products; the underlying measurements are unaltered. It is an independent, unofficial tool and is not endorsed by NEON, Battelle, or the NSF.")))
  })

  # ---- help dialog (also wired in confirm.js) ----------------------------

  # ===========================================================================
  # STARTUP RESOLVER (delighters: deep-link + restore-last-site)
  # ===========================================================================
  # Resolution now happens ON THE CLIENT (www/app.js -> smtStartupResolve): it
  # reads the ?site= deep link (precedence) and the localStorage last-site, and
  # dispatches the chosen code through input$siteExplore AFTER the first render —
  # the same proven path a map-dot tap uses. That avoids the trap where a
  # server-side load during the initial reactive flush ran before #splash was in
  # the DOM, so ingest()'s shinyjs::hide("splash") was silently lost and the site
  # loaded underneath a still-visible splash. The existing observeEvent(siteExplore)
  # below validates the code (load_site_full -> SITE_INDEX lookup) and loads it;
  # an unknown ?site= simply yields the "not in the bundle" notice. Restore stays
  # NON-STICKY: the hero "change site" returns to the map and clears the query.
}
