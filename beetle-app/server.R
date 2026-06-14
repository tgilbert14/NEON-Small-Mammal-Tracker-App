# ===========================================================================
# NEON Ground Beetle Tracker — server.R
# Data flow + all outputs (community, diversity, seasonality, biogeography).
# ===========================================================================

function(input, output, session) {

  # ---- small plot helpers -------------------------------------------------
  plotly_theme <- function(p, legend = TRUE) {
    p %>% plotly::layout(
      paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
      font = list(color = "#1f2a30", family = "Rubik"),
      xaxis = list(gridcolor = "rgba(31,42,48,0.08)", zerolinecolor = "rgba(31,42,48,0.15)"),
      yaxis = list(gridcolor = "rgba(31,42,48,0.08)", zerolinecolor = "rgba(31,42,48,0.15)"),
      margin = list(l = 55, r = 30, t = 36, b = 46),
      showlegend = legend,
      legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.16),
      hoverlabel = list(bgcolor = "rgba(19,99,43,0.96)", bordercolor = "#FFD200",
                        font = list(color = "#fff", family = "Rubik", size = 13))) %>%
      plotly::config(displayModeBar = FALSE, responsive = TRUE)
  }
  note_plot <- function(msg, icon = "\U0001FAB2") {
    plotly::plot_ly(type = "scatter", mode = "markers") %>% plotly::layout(
      paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
      xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
      annotations = list(list(text = paste0(icon, "<br>", msg), showarrow = FALSE,
        font = list(color = "#6b7a85", size = 15), align = "center"))) %>%
      plotly::config(displayModeBar = FALSE)
  }

  rv <- reactiveValues(data = NULL, label = NULL, pal = NULL, ctx = NULL)

  # ---- cascading state -> site picker ------------------------------------
  .states <- state_choices()
  updateSelectInput(session, "stateSel", choices = .states,
                    selected = if ("KS" %in% .states) "KS" else unname(.states)[1])
  observeEvent(input$stateSel, {
    sites <- sites_in_state(input$stateSel)
    sel <- if (!is.null(rv$pendingSite) && rv$pendingSite %in% sites) rv$pendingSite
           else if (length(sites)) sites[[1]] else NULL
    rv$pendingSite <- NULL
    updateSelectInput(session, "site", choices = sites, selected = sel)
  }, ignoreNULL = TRUE)

  output$siteBio <- renderUI({
    req(input$site); b <- site_bio(input$site)
    if (is.null(b)) return(NULL)
    div(class = "site-bio", bs_icon("info-circle-fill"), span(b))
  })

  shinyjs::hide("mainTabsWrap")

  # ---- load a site --------------------------------------------------------
  load_site <- function(site) {
    if (is.null(site) || site == "") return(invisible())
    d0 <- load_site_bundle(site)
    if (is.null(d0) || !nrow(d0)) {
      # try a live fetch when no bundle/demo exists
      if (LIVE_FETCH) {
        d0 <- tryCatch(fetch_neon_beetles(site, input$dateRange[1], input$dateRange[2]),
                       error = function(e) { showNotification(paste("NEON fetch failed:",
                         conditionMessage(e)), type = "error"); NULL })
      }
      if (is.null(d0) || !nrow(d0)) {
        showNotification("No beetle data bundled for that site yet.", type = "warning")
        return(invisible())
      }
    }
    src <- attr(d0, "source") %||% "neon"
    d <- filter_window(d0, input$dateRange[1], input$dateRange[2])
    if (is.null(d) || !nrow(d)) {
      showNotification("No beetle records in that date window — widen it.", type = "warning")
      return(invisible())
    }
    attr(d, "source") <- src
    rv$data  <- d
    rv$pal   <- make_species_pal(d)
    rv$label <- site_label(site)
    y1 <- format(min(d$date, na.rm = TRUE), "%Y"); y2 <- format(max(d$date, na.rm = TRUE), "%Y")
    rv$ctx <- paste0(site, " · ", if (y1 == y2) y1 else paste0(y1, "–", y2))
    shinyjs::show("mainTabsWrap"); shinyjs::hide("splash")
    nav_select("tabs", "overview")
  }
  observeEvent(input$loadBtn, load_site(input$site))

  output$srcNote <- renderUI({
    if (is.null(rv$data)) return(NULL)
    if (identical(attr(rv$data, "source"), "demo"))
      div(class = "env-source env-demo", bs_icon("info-circle-fill"),
          tags$span(HTML(" <b>Demo data</b> — illustrative, <b>not</b> NEON records. Run <code>scripts/refresh_data.R</code> to bundle the real product.")))
    else
      div(class = "env-source env-real", bs_icon("patch-check-fill"),
          tags$span(" NEON ground-beetle records for this site."))
  })

  # ---- splash + hero ------------------------------------------------------
  output$splash <- renderUI({
    if (!is.null(rv$data)) return(NULL)
    div(class = "splash",
      div(class = "splash-icon", "\U0001FAB2"),
      h3("Pick a site to begin"),
      p("Choose a state and site at left, then ", tags$b("Load this site"),
        " — or open the Biogeography map and tap a marker."),
      if (!is.null(SITE_INDEX))
        p(class = "splash-sub", sprintf("%d site%s with beetle data available.",
          nrow(SITE_INDEX), if (nrow(SITE_INDEX) == 1) "" else "s")))
  })

  output$heroStats <- renderUI({
    d <- rv$data; if (is.null(d)) return(NULL)
    ct <- community_table(d); hn <- hill_numbers(ct$individuals)
    tn <- effort_trapnights(d)
    bouts <- length(unique(paste(d$plotID, d$collectDate)))
    stat <- function(v, l) div(class = "hero-stat", div(class = "hs-v", v), div(class = "hs-l", l))
    div(class = "hero-stats",
      stat(fmt_int(sum(ct$individuals)), "individuals"),
      stat(nrow(ct), "species"),
      stat(hn$q1, "effective species (q1)"),
      stat(fmt_int(bouts), "trap bouts"),
      stat(fmt_int(round(tn)), "trap-nights"))
  })

  # ---- Overview: community bar -------------------------------------------
  output$commBar <- renderPlotly({
    d <- rv$data; req(d)
    ct <- community_table(d); if (is.null(ct) || !nrow(ct)) return(note_plot("No community data"))
    ct <- ct[order(ct$individuals), ]   # plotly bars: bottom = first
    pal <- rv$pal
    cols <- unname(pal[ct$scientificName]); cols[is.na(cols)] <- DDL$forest
    plot_ly(ct, x = ~individuals, y = ~factor(scientificName, levels = scientificName),
            type = "bar", orientation = "h",
            marker = list(color = cols),
            text = ~paste0(cpn, " /100TN"), textposition = "outside",
            hovertemplate = ~paste0("<b>", scientificName, "</b><br>",
              individuals, " individuals · ", bouts, " bouts<br>",
              cpn, " per 100 trap-nights<extra></extra>")) %>%
      plotly_theme(legend = FALSE) %>%
      plotly::layout(xaxis = list(title = "individuals"), yaxis = list(title = ""),
                     margin = list(l = 200)) %>%
      plotly::add_annotations(text = rv$ctx, x = 1, y = 1.04, xref = "paper", yref = "paper",
        xanchor = "right", showarrow = FALSE, font = list(color = "#6b7a89", size = 11))
  })

  output$meetBeetles <- renderUI({
    d <- rv$data; req(d)
    ct <- community_table(d); ct <- utils::head(ct, 6)
    cards <- lapply(seq_len(nrow(ct)), function(i) {
      sp <- ct$scientificName[i]
      div(class = "meet-card",
        div(class = "meet-ico", "\U0001FAB2"),
        div(class = "meet-body",
          div(class = "meet-name", sp),
          div(class = "meet-stat", sprintf("%s individuals · %s per 100 trap-nights",
              fmt_int(ct$individuals[i]), ct$cpn[i])),
          div(class = "meet-blurb", beetle_blurb(sp))))
    })
    div(class = "meet-grid", cards)
  })

  # ---- Diversity ----------------------------------------------------------
  output$hillPlot <- renderPlotly({
    d <- rv$data; req(d)
    ct <- community_table(d); hn <- hill_numbers(ct$individuals)
    if (is.na(hn$q0)) return(note_plot("Not enough data for diversity"))
    df <- data.frame(q = c("q0\nrichness", "q1\ncommon", "q2\ndominant"),
                     v = c(hn$q0, hn$q1, hn$q2))
    plot_ly(df, x = ~q, y = ~v, type = "bar",
            marker = list(color = c("#13632b", "#1a7f37", "#5cb56e")),
            text = ~round(v, 1), textposition = "outside",
            hovertemplate = ~paste0("%{x}: %{y:.2f} effective species<extra></extra>")) %>%
      plotly_theme(legend = FALSE) %>%
      plotly::layout(xaxis = list(title = ""), yaxis = list(title = "effective species"))
  })

  output$hillNote <- renderUI({
    d <- rv$data; req(d)
    hn <- hill_numbers(community_table(d)$individuals)
    if (is.na(hn$even)) return(NULL)
    word <- if (hn$even >= 0.6) "an even community" else if (hn$even >= 0.35)
              "a moderately uneven community" else "a community dominated by a few species"
    div(class = "hill-note", bs_icon("info-circle"),
      HTML(sprintf(" Evenness q1/q0 = <b>%.2f</b> — %s.", hn$even, word)))
  })

  output$rarePlot <- renderPlotly({
    d <- rv$data; req(d)
    rc <- rarefaction_curve(community_table(d)$individuals)
    if (is.null(rc)) return(note_plot("Not enough individuals to rarefy"))
    plot_ly() %>%
      add_trace(x = rc$n, y = rc$hi, type = "scatter", mode = "lines",
                line = list(width = 0), showlegend = FALSE, hoverinfo = "skip") %>%
      add_trace(x = rc$n, y = rc$lo, type = "scatter", mode = "lines", fill = "tonexty",
                fillcolor = "rgba(19,99,43,0.14)", line = list(width = 0),
                name = "±1 SD", hoverinfo = "skip") %>%
      add_trace(x = rc$n, y = rc$richness, type = "scatter", mode = "lines",
                line = list(color = "#13632b", width = 3), name = "expected species",
                hovertemplate = "%{x} individuals<br>%{y:.1f} species<extra></extra>") %>%
      plotly_theme(legend = FALSE) %>%
      plotly::layout(xaxis = list(title = "individuals sampled"),
                     yaxis = list(title = "expected species"))
  })

  output$accumPlot <- renderPlotly({
    d <- rv$data; req(d)
    ac <- accumulation_by_bout(d)
    if (is.null(ac)) return(note_plot("Not enough bouts for accumulation"))
    plot_ly() %>%
      add_trace(x = ac$bouts, y = ac$richness + ac$sd, type = "scatter", mode = "lines",
                line = list(width = 0), showlegend = FALSE, hoverinfo = "skip") %>%
      add_trace(x = ac$bouts, y = pmax(0, ac$richness - ac$sd), type = "scatter",
                mode = "lines", fill = "tonexty", fillcolor = "rgba(47,127,181,0.14)",
                line = list(width = 0), name = "±1 SD", hoverinfo = "skip") %>%
      add_trace(x = ac$bouts, y = ac$richness, type = "scatter", mode = "lines+markers",
                line = list(color = "#2f7fb5", width = 3), marker = list(size = 5),
                name = "species found",
                hovertemplate = "after %{x} bouts<br>%{y:.1f} species<extra></extra>") %>%
      plotly_theme(legend = FALSE) %>%
      plotly::layout(xaxis = list(title = "trapping bouts"),
                     yaxis = list(title = "cumulative species"))
  })

  # ---- Trends (inter-annual) ----------------------------------------------
  trend_data <- reactive({ d <- rv$data; req(d); annual_trend(d) })

  output$trendVerdict <- renderUI({
    t <- trend_data()
    if (is.null(t) || nrow(t) < 2) return(NULL)
    slope <- attr(t, "slope"); p <- attr(t, "p"); pct <- attr(t, "pct_per_yr")
    if (is.null(slope) || is.null(p) || !is.finite(slope)) {
      return(div(class = "trend-verdict trend-flat", bs_icon("dash-circle"),
        HTML(sprintf(" Only %d years of data — too few to fit a trend yet.", nrow(t)))))
    }
    sig <- is.finite(p) && p < 0.05
    dir <- if (!sig) "flat" else if (slope > 0) "up" else "down"
    word <- switch(dir, up = "rising", down = "declining", flat = "roughly flat")
    icon <- switch(dir, up = "arrow-up-right-circle-fill",
                   down = "arrow-down-right-circle-fill", flat = "dash-circle")
    pcttxt <- if (is.finite(pct)) sprintf(" (~%+.0f%%/yr)", pct) else ""
    sigtxt <- if (sig) sprintf("statistically clear (p = %.3f)", p)
              else sprintf("not statistically distinguishable from no change (p = %.2f)",
                           if (is.finite(p)) p else NA)
    div(class = paste("trend-verdict", paste0("trend-", dir)), bs_icon(icon),
      HTML(sprintf(" Over %d years, catch-per-effort is <b>%s</b>%s — %s. %s",
        nrow(t), word, pcttxt, sigtxt,
        if (identical(attr(t, "metric_kind"), "count"))
          "<i>(raw counts — this bundle has no effort data)</i>" else "")))
  })

  output$trendPlot <- renderPlotly({
    t <- trend_data()
    if (is.null(t) || nrow(t) < 2)
      return(note_plot("Need at least two years of data for a trend"))
    kind <- attr(t, "metric_kind") %||% "cpn"
    ytitle <- if (kind == "cpn") "catch per 100 trap-nights" else "individuals caught"
    p <- plot_ly(x = ~t$year, y = ~t$metric, type = "scatter", mode = "lines+markers",
      name = "observed", line = list(color = "#13632b", width = 3),
      marker = list(size = 9, color = "#13632b"),
      hovertemplate = paste0("%{x}: %{y:.1f} ", ytitle, "<extra></extra>"))
    pred <- attr(t, "pred")
    if (!is.null(pred) && length(pred) == nrow(t)) {
      p <- p %>% add_trace(x = t$year, y = pred, mode = "lines", name = "trend",
        line = list(color = "#c9a300", width = 2, dash = "dash"),
        hoverinfo = "skip", inherit = FALSE)
    }
    plotly_theme(p) %>% plotly::layout(
      xaxis = list(title = "", dtick = 1),
      yaxis = list(title = ytitle, rangemode = "tozero"))
  })

  # ---- Seasonality --------------------------------------------------------
  output$seasonPlot <- renderPlotly({
    d <- rv$data; req(d)
    if (isTRUE(input$seasonBySpecies)) {
      s <- seasonality(d, by_species = TRUE)
      if (is.null(s) || !nrow(s)) return(note_plot("No seasonal data"))
      pal <- rv$pal; p <- plot_ly()
      for (sp in unique(s$scientificName)) {
        ss <- s[s$scientificName == sp, ]
        p <- p %>% add_trace(x = month.abb[ss$mon], y = ss$cpn, type = "scatter",
          mode = "lines+markers", name = sp,
          line = list(color = pal[[sp]] %||% "#13632b", width = 2),
          marker = list(size = 6, color = pal[[sp]] %||% "#13632b"),
          hovertemplate = paste0("<b>", sp, "</b><br>%{x}: %{y:.1f} /100TN<extra></extra>"))
      }
      return(plotly_theme(p) %>% plotly::layout(
        xaxis = list(title = "", categoryorder = "array", categoryarray = month.abb),
        yaxis = list(title = "catch per 100 trap-nights")))
    }
    s <- seasonality(d, by_species = FALSE)
    if (is.null(s) || !nrow(s)) return(note_plot("No seasonal data"))
    plot_ly(x = month.abb[s$mon], y = s$cpn, type = "scatter", mode = "lines+markers",
            fill = "tozeroy", fillcolor = "rgba(19,99,43,0.16)",
            line = list(color = "#13632b", width = 3), marker = list(size = 7, color = "#13632b"),
            hovertemplate = "%{x}: %{y:.1f} per 100 trap-nights<extra></extra>") %>%
      plotly_theme(legend = FALSE) %>%
      plotly::layout(xaxis = list(title = "", categoryorder = "array", categoryarray = month.abb),
                     yaxis = list(title = "catch per 100 trap-nights"))
  })

  # ---- Biogeography -------------------------------------------------------
  updateSelectInput(session, "rangeSpecies",
                    choices = c("All species (richness)" = "", species_choices()))

  output$map <- renderLeaflet({
    si <- SITE_INDEX
    base <- leaflet() %>% addProviderTiles("CartoDB.Positron") %>% setView(-98, 39, zoom = 3)
    if (is.null(si)) return(base)
    si <- si[!is.na(si$lat), ]
    sp <- input$rangeSpecies %||% ""
    if (sp != "" && !is.null(SPECIES_SITES)) {
      rng <- SPECIES_SITES[SPECIES_SITES$scientificName == sp, ]
      rng <- merge(rng, si[, c("site", "name", "lat", "lng")],
                   by.x = "siteID", by.y = "site")
      if (!nrow(rng)) return(base)
      return(base %>% addCircleMarkers(data = rng, lng = ~lng, lat = ~lat, layerId = ~siteID,
        radius = ~pmax(6, sqrt(individualCount) * 2.2), color = "#13632b",
        fillOpacity = 0.75, stroke = TRUE, weight = 1.5,
        label = ~lapply(sprintf("<b>%s</b><br><i>%s</i>: %s individuals",
          siteID, sp, fmt_int(individualCount)), htmltools::HTML)))
    }
    pal <- c(neon = "#13632b", demo = "#c9a300")
    base %>% addCircleMarkers(data = si, lng = ~lng, lat = ~lat, layerId = ~site,
        radius = ~pmax(6, sqrt(richness) * 4), color = ~unname(pal[source]),
        fillOpacity = 0.7, stroke = TRUE, weight = 1.5,
        label = ~lapply(sprintf("<b>%s</b> · %s<br>%d species · %s individuals<br>dominant: <i>%s</i>%s",
          site, name, richness, fmt_int(individuals), dominant,
          ifelse(source == "demo", "<br><b>demo data</b>", "")), htmltools::HTML))
  })

  # cross-site community ordination (PCoA on Bray-Curtis)
  output$ordPlot <- renderPlotly({
    o <- ORDINATION
    if (is.null(o) || nrow(o) < 4)
      return(note_plot("Not enough sites/samples to ordinate<br><span style='font-size:13px'>Bundle more sites with scripts/refresh_data.R</span>"))
    pal <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, "Dark2"))(length(unique(o$site)))
    pal <- stats::setNames(pal, sort(unique(o$site)))
    ve <- attr(o, "var_explained")
    p <- plot_ly()
    for (s in sort(unique(o$site))) {
      os <- o[o$site == s, ]
      p <- p %>% add_trace(data = os, x = ~x, y = ~y, type = "scatter", mode = "markers",
        name = s, marker = list(size = 9, color = pal[[s]], opacity = 0.8,
                                line = list(color = "#fff", width = 1)),
        hovertemplate = paste0("<b>", s, "</b><br>%{text}<extra></extra>"), text = ~sample)
    }
    plotly_theme(p) %>% plotly::layout(
      xaxis = list(title = if (!is.na(ve[1])) sprintf("PCoA 1 (%d%%)", ve[1]) else "PCoA 1"),
      yaxis = list(title = if (!is.na(ve[2])) sprintf("PCoA 2 (%d%%)", ve[2]) else "PCoA 2"))
  })
  observeEvent(input$map_marker_click, {
    s <- input$map_marker_click$id; req(s)
    m <- neon_sites[neon_sites$site == s, ]
    if (nrow(m)) { rv$pendingSite <- s; updateSelectInput(session, "stateSel", selected = m$state) }
    load_site(s)
  })

  output$siteTable <- DT::renderDT({
    si <- SITE_INDEX; if (is.null(si)) return(NULL)
    tab <- si[order(-si$richness), c("site", "name", "state", "richness", "individuals", "dominant", "source")]
    names(tab) <- c("Site", "Name", "State", "Species", "Individuals", "Dominant species", "Source")
    DT::datatable(tab, rownames = FALSE, selection = "none",
                  options = list(pageLength = 10, dom = "tp")) %>%
      DT::formatCurrency("Individuals", currency = "", interval = 3, mark = ",", digits = 0)
  })

  output$indicatorTable <- DT::renderDT({
    ind <- INDICATORS
    if (is.null(ind) || !nrow(ind))
      return(DT::datatable(data.frame(Note = "Need ≥2 sites bundled to rank indicators."),
                           rownames = FALSE, options = list(dom = "t")))
    tab <- ind[, c("scientificName", "indicator_site", "indval", "specificity", "fidelity", "total")]
    names(tab) <- c("Species", "Indicator of", "IndVal", "Specificity %", "Fidelity %", "Total caught")
    DT::datatable(tab, rownames = FALSE, selection = "none",
                  options = list(pageLength = 12, dom = "tp", order = list(list(2, "desc")))) %>%
      DT::formatStyle("Species", fontStyle = "italic") %>%
      DT::formatStyle("IndVal",
        background = DT::styleColorBar(c(0, 100), "#cfe6d6"),
        backgroundSize = "98% 70%", backgroundRepeat = "no-repeat",
        backgroundPosition = "center")
  })

  # ---- About --------------------------------------------------------------
  output$aboutPanel <- renderUI({
    div(class = "about",
      h3("About this app"),
      p("The ", tags$b("NEON Ground Beetle Tracker"), " explores carabid beetle biodiversity from ",
        tags$a(href = "https://data.neonscience.org/data-products/DP1.10022.001", target = "_blank",
               "NEON DP1.10022.001 — Ground beetles sampled from pitfall traps"), "."),
      p("Ground beetles are a classic ", tags$b("bioindicator"),
        ": they respond quickly to habitat, disturbance, and climate, so their richness, diversity, and seasonal activity tell a rich story about each NEON site."),
      h4("What you can explore"),
      tags$ul(
        tags$li(tags$b("Community"), " — which species dominate, by abundance and per-trap-night effort."),
        tags$li(tags$b("Diversity"), " — Hill numbers, rarefaction at equal sample size, and species accumulation."),
        tags$li(tags$b("Seasonality"), " — activity-density by month, overall and per species."),
        tags$li(tags$b("Trends"), " — inter-annual catch-per-effort with a fitted trend and decline/increase verdict."),
        tags$li(tags$b("Biogeography"), " — a richness/range map, a Bray–Curtis community ordination, and indicator species (IndVal) per site.")),
      h4("Methods"),
      tags$ul(
        tags$li("Abundance is normalised to ", tags$b("catch per 100 trap-nights"),
                " (effort = unique plot × bout trap-night totals) so sites compare fairly."),
        tags$li("Hill numbers (Hill 1973; Jost 2006); Hurlbert (1971) rarefaction; Gotelli & Colwell (2001) accumulation."),
        tags$li("Real bundles reconcile parataxonomist IDs with authoritative ", tags$b("expert IDs"),
                " and normalise the 2018 trap-count and 2023 plot-count protocol changes via per-trap-night effort.")),
      div(class = "about-note", bs_icon("exclamation-triangle"),
        " The bundled demo (HARV, KONZ, JORN) is ", tags$b("illustrative, not real NEON data"),
        " — it exists so the app is usable before the real bundle is built with ",
        tags$code("scripts/refresh_data.R"), "."),
      p(style = "margin-top:16px", "An educational data-exploration tool by Desert Data Labs. Not affiliated with NEON, Battelle, or the NSF.")
    )
  })
}
