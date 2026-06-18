# ===========================================================================
# report_pdf.R — deploy-safe server-side PDF site report.
#
# Replaces the old browser-print "report card" (window.print of hidden HTML,
# which cut off / mis-paginated, esp. on mobile) with a TRUE vector PDF built
# from base grDevices + base grid + ggplot2 (already a dependency), streamed by
# a Shiny downloadHandler. NO LaTeX, NO headless Chrome, NO extra packages.
#
# Design from the report-card-pdf workflow (Don=structure, Quinn=honest content,
# Vera=static charts, Quill=PDF mechanics). Sourced from global.R after helpers.R.
# Verify locally without Shiny:
#   Rscript -e 'source("global.R"); source("R/report_pdf.R");
#     d <- read_bundle(DEMO_PATH); render_report_pdf("test.pdf", d,
#       "JORN · Jornada Experimental Range", TRUE, NULL)'
# ===========================================================================
suppressPackageStartupMessages(library(grid))

# cairo_pdf renders system fonts + full Unicode; plain pdf() is base-14 WinAnsi
# and would tofu em-dashes / the paw glyph — the ascii() sanitizer covers that
# fallback either way. Probe once at source time.
PDF_DEV <- if (isTRUE(capabilities("cairo"))) grDevices::cairo_pdf else grDevices::pdf

PG <- list(w = 8.5, h = 11, margin = 0.75, lineH = 0.165,
           navy = DDL$navy, navy2 = DDL$navy2, gold = DDL$gold, gold2 = DDL$gold2,
           cardinal = DDL$cardinal, sky = DDL$sky, green = DDL$green,
           ink = DDL$ink, muted = DDL$muted, line = DDL$line,
           tint = "#eef2f8", zebra = "#f6f8fc")
PG$cw <- PG$w - 2 * PG$margin      # 7.0in content width
PG$ch <- PG$h - 2 * PG$margin      # 9.5in content height

# ---- text + cursor helpers -------------------------------------------------
# Drop emoji / non-Latin1 and map the few typographic glyphs so a base-pdf()
# fallback never boxes; harmless under cairo_pdf.
ascii <- function(x) {
  x <- gsub("—|–", "-", x)   # em/en dash
  x <- gsub("·", " | ", x)         # middot
  x <- gsub("≈", "~", x)           # almost-equal
  x <- gsub("±", "+/-", x)         # plus-minus
  x <- gsub("≥", ">=", x); x <- gsub("≤", "<=", x)
  x <- gsub("→", "to", x)          # fmt_range uses an arrow date separator
  enc2utf8(iconv(x, "UTF-8", "latin1", sub = ""))
}
gy <- function(yTop) unit(1, "npc") - unit(yTop, "in")   # top-down cursor -> grid's bottom-up

# Measure-then-wrap (run only AFTER the device is open — metrics are device-specific).
wrap_to_width <- function(txt, fontsize, width_in, fontface = 1) {
  gp <- gpar(fontsize = fontsize, fontface = fontface)
  words <- strsplit(txt, " ")[[1]]; lines <- character(0); cur <- ""
  for (w in words) {
    test <- if (nzchar(cur)) paste(cur, w) else w
    wpx <- convertWidth(grobWidth(textGrob(test, gp = gp)), "in", valueOnly = TRUE)
    if (wpx > width_in && nzchar(cur)) { lines <- c(lines, cur); cur <- w } else cur <- test
  }
  c(lines, cur)
}
draw_para <- function(txt, yTop, fontsize = 10, col = PG$ink, fontface = 1, gapAfter = 0.10) {
  for (ln in wrap_to_width(ascii(txt), fontsize, PG$cw, fontface)) {
    grid.text(ln, x = unit(0, "npc"), y = gy(yTop), just = c("left", "top"),
              gp = gpar(fontsize = fontsize, col = col, fontface = fontface))
    yTop <- yTop + PG$lineH
  }
  yTop + gapAfter
}
draw_h4 <- function(txt, yTop) {
  grid.text(ascii(txt), x = unit(0, "npc"), y = gy(yTop), just = c("left", "top"),
            gp = gpar(fontsize = 11, fontface = "bold", col = PG$navy))
  yTop + 0.24
}

# ---- stat tile band (the omitted-in-draft helper) --------------------------
# cells: list of c(value, label). Lays them into `ncol` columns of height cellH.
draw_stat_grid <- function(cells, yTop, ncol, cellH = 0.5, gap = 0.10) {
  n <- length(cells); cwn <- 1 / ncol
  for (i in seq_len(n)) {
    r <- (i - 1) %/% ncol; cc <- (i - 1) %% ncol
    x0 <- cc * cwn; yc <- yTop + r * (cellH + gap)
    grid.rect(x = unit(x0, "npc") + unit(2, "pt"), y = gy(yc),
              width = unit(cwn, "npc") - unit(4, "pt"), height = unit(cellH, "in"),
              just = c("left", "top"), gp = gpar(fill = PG$tint, col = NA))
    grid.rect(x = unit(x0, "npc") + unit(2, "pt"), y = gy(yc),     # navy left spine
              width = unit(3, "pt"), height = unit(cellH, "in"),
              just = c("left", "top"), gp = gpar(fill = PG$navy, col = NA))
    grid.text(ascii(cells[[i]][1]), x = unit(x0, "npc") + unit(11, "pt"), y = gy(yc + 0.12),
              just = c("left", "top"), gp = gpar(fontsize = 17, fontface = "bold", col = PG$navy))
    grid.text(ascii(cells[[i]][2]), x = unit(x0, "npc") + unit(11, "pt"), y = gy(yc + 0.40),
              just = c("left", "top"), gp = gpar(fontsize = 8.5, col = PG$muted))
  }
  yTop + ceiling(n / ncol) * (cellH + gap) + 0.04
}

# ---- hand-drawn table (zebra + header rule + page-break/reprint) -----------
draw_table <- function(cols, rows, colx, yTop, faces = rep(1, length(cols)),
                       fs = 9, rowH = 0.205, repeat_header) {
  draw_header <- function(yT) {
    for (j in seq_along(cols))
      grid.text(ascii(cols[j]), x = unit(colx[j], "npc"), y = gy(yT), just = c("left", "top"),
                gp = gpar(fontface = "bold", fontsize = fs, col = PG$navy))
    grid.lines(x = unit(c(0, 1), "npc"), y = gy(yT + 0.17), gp = gpar(col = PG$line, lwd = 1))
    yT + 0.26
  }
  yTop <- draw_header(yTop)
  for (i in seq_along(rows)) {
    if (yTop + rowH > PG$ch - 0.45) yTop <- draw_header(repeat_header())
    if (i %% 2 == 0)
      grid.rect(x = unit(0, "npc"), y = gy(yTop), width = unit(1, "npc"), height = unit(rowH, "in"),
                just = c("left", "top"), gp = gpar(fill = PG$zebra, col = NA))
    r <- rows[[i]]
    for (j in seq_along(r))
      grid.text(ascii(r[j]), x = unit(colx[j], "npc"), y = gy(yTop + 0.02), just = c("left", "top"),
                gp = gpar(fontsize = fs, fontface = faces[j]))
    yTop <- yTop + rowH
  }
  yTop + 0.12
}

# ---- static charts (Vera) --------------------------------------------------
theme_report <- function(base = 9) {
  ggplot2::theme_minimal(base_size = base) + ggplot2::theme(
    plot.title    = ggplot2::element_text(face = "bold", colour = PG$navy, size = base + 2),
    plot.subtitle = ggplot2::element_text(colour = PG$muted, size = base - 1),
    plot.caption  = ggplot2::element_text(colour = PG$muted, size = base - 2, hjust = 0),
    plot.title.position = "plot", plot.caption.position = "plot",
    axis.title = ggplot2::element_text(colour = PG$ink, size = base - 1),
    axis.text  = ggplot2::element_text(colour = PG$ink),
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_line(colour = PG$line, linewidth = 0.3),
    legend.position = "none")
}
note_gg <- function(msg = "Not enough data for this window") {
  ggplot2::ggplot() + ggplot2::annotate("text", 0, 0, label = msg, colour = PG$muted, size = 4) +
    ggplot2::theme_void()
}
scope_cap <- function(cs)
  ascii(sprintf("NEON small-mammal trapping  |  %s to %s  |  %s trap-nights, %s plots",
                cs$date_min, cs$date_max, format(round(cs$trap_nights), big.mark = ","), cs$plots))

chart_mnka <- function(d, cs) {
  mn <- tryCatch(mnka_series(d), error = function(e) NULL)
  if (is.null(mn) || nrow(mn) == 0) return(note_gg())
  site <- dplyr::summarise(dplyr::group_by(mn, .data$date),
            mnka = sum(.data$mnka),
            cpue = round(100 * sum(.data$captures) / pmax(sum(.data$trap_nights), 1), 1),
            .groups = "drop")
  long <- tidyr::pivot_longer(site, c("mnka", "cpue"), names_to = "metric", values_to = "value")
  long$metric <- factor(long$metric, c("mnka", "cpue"),
                        c("MNKA (individuals known alive)", "Catch per 100 trap-nights"))
  is_mnka <- function(x) x[grepl("MNKA", x$metric), , drop = FALSE]
  is_cpue <- function(x) x[grepl("Catch", x$metric), , drop = FALSE]
  ggplot2::ggplot(long, ggplot2::aes(.data$date, .data$value)) +
    ggplot2::geom_col(data = is_mnka, fill = PG$navy, width = 22) +
    ggplot2::geom_line(data = is_cpue, colour = PG$gold2, linewidth = 0.9) +
    ggplot2::geom_point(data = is_cpue, colour = PG$gold2, size = 1.4) +
    ggplot2::facet_wrap(~metric, ncol = 1, scales = "free_y", strip.position = "left") +
    ggplot2::expand_limits(y = 0) +
    ggplot2::labs(title = "Monthly small-mammal abundance",
      subtitle = "Population index (top) and effort-adjusted catch rate (bottom)",
      x = NULL, y = NULL, caption = scope_cap(cs)) +
    theme_report() + ggplot2::theme(strip.placement = "outside",
      strip.text.y.left = ggplot2::element_text(angle = 90, colour = PG$navy2, face = "bold"))
}
chart_species <- function(d, cs) {
  s <- utils::head(species_summary(d), 12)
  if (nrow(s) == 0) return(note_gg())
  s$sci <- factor(s$scientificName, levels = rev(s$scientificName))
  ggplot2::ggplot(s, ggplot2::aes(.data$captures, .data$sci)) +
    ggplot2::geom_col(fill = PG$green, width = 0.72) +
    ggplot2::geom_text(ggplot2::aes(label = paste0(format(.data$individuals, big.mark = ","), " indiv")),
      hjust = -0.12, size = 3, colour = PG$muted) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.18))) +
    ggplot2::labs(title = "Species composition, by capture count",
      subtitle = "Bar = total captures; label = distinct individuals",
      x = "captures", y = NULL, caption = scope_cap(cs)) +
    theme_report() + ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(face = "italic", colour = PG$ink))
}
# Reserve a region and print a ggplot into it (newpage=FALSE because vp != NULL).
embed_chart <- function(p, yTop, height_in) {
  vp <- viewport(x = unit(0, "npc"), y = gy(yTop), width = unit(PG$cw, "in"),
                 height = unit(height_in, "in"), just = c("left", "top"))
  tryCatch(print(p, vp = vp),
           error = function(e) grid.text("(chart unavailable)", y = gy(yTop + height_in / 2),
                                          gp = gpar(col = PG$muted, fontsize = 9)))
  yTop + height_in + 0.18
}

# ---- per-page frame (content viewport + footer) ----------------------------
FOOT <- paste("Data: NEON Small Mammal Box Trapping (DP1.10072.001).",
  "Generated by the NEON Small Mammal Tracker - Desert Data Labs.",
  "Unofficial educational summary; not affiliated with NEON, Battelle, or NSF.")
new_page <- function(page_i, title, footer_full = FALSE) {
  grid.newpage()
  pushViewport(viewport(width = unit(PG$cw, "in"), height = unit(PG$ch, "in"), name = "content"))
  if (page_i > 1) {
    # truncate a long site label so the running header never collides with the
    # right-corner title (e.g. UNDE = "University of Notre Dame Environmental
    # Research Center" used to run straight into it).
    hl <- ascii(title); if (nchar(hl) > 50) hl <- paste0(substr(hl, 1, 49), "…")
    grid.text(hl, x = unit(0, "npc"), y = gy(0), just = c("left", "top"),
              gp = gpar(fontsize = 10, fontface = "bold", col = PG$navy))
    grid.text("NEON Small Mammal Report Card", x = unit(1, "npc"), y = gy(0),
              just = c("right", "top"), gp = gpar(fontsize = 8.5, col = PG$muted))
    grid.lines(x = unit(c(0, 1), "npc"), y = gy(0.24), gp = gpar(col = PG$line, lwd = 1))
  }
  ftxt <- if (footer_full)
    paste(FOOT, "MNKA = Minimum Number Known Alive (Krebs 1966); Hill numbers (Jost 2006);",
          "closed-capture estimators (Schnabel 1938; Chapman 1951; Otis et al. 1978).") else FOOT
  fl <- wrap_to_width(ascii(ftxt), 7, PG$cw)
  for (k in seq_along(fl))
    grid.text(fl[k], x = unit(0, "npc"), y = unit((length(fl) - k) * 0.12 + 0.04, "in"),
              just = c("left", "bottom"), gp = gpar(fontsize = 7, col = PG$muted))
}

# ---- the composer ----------------------------------------------------------
# Everything is a pure function of `d` except cc (the detection reactive), which
# the downloadHandler passes in.
render_report_pdf <- function(file, d, label, is_demo = FALSE, cc = NULL) {
  lb <- tryCatch(build_leaderboard(d), error = function(e) NULL)
  cs <- community_stats(d, lb)
  hn <- hill_numbers(d)
  sp <- species_summary(d)
  bio <- tryCatch(site_bio(mode_chr(d$siteID)), error = function(e) NULL)
  has_gg <- requireNamespace("ggplot2", quietly = TRUE)

  # population structure + breeding (lifted from the old reportCard)
  h <- dplyr::filter(d, !is.na(.data$tagID)); n_handled <- nrow(h)
  sex_ratio <- if (cs$n_male > 0) sprintf("%.2f F per M", cs$n_female / cs$n_male) else "-"
  stage_tbl <- if (n_handled > 0)
    sort(table(factor(ifelse(is.na(h$lifeStage) | h$lifeStage == "", "unknown", h$lifeStage))),
         decreasing = TRUE) else NULL
  stage_txt <- if (!is.null(stage_tbl))
    paste(sprintf("%s %s", format(as.integer(stage_tbl), big.mark = ","), names(stage_tbl)),
          collapse = " | ") else "-"
  fr <- tryCatch(flag_repro(d), error = function(e) NULL); repro_txt <- "-"
  if (!is.null(fr)) {
    fh <- fr[!is.na(fr$tagID), ]
    nd <- function(lvl) dplyr::n_distinct(fh$tagID[fh$repro == lvl])
    repro_txt <- sprintf("%s breeding males | %s pregnant | %s lactating/receptive (distinct individuals)",
      fmt_int(nd("breeding male")), fmt_int(nd("pregnant female")), fmt_int(nd("lactating/receptive female")))
  }

  PDF_DEV(file, width = PG$w, height = PG$h, onefile = TRUE)
  on.exit(grDevices::dev.off(), add = TRUE)   # CRITICAL — an unclosed device truncates the file

  # ---------- PAGE 1 (stands alone) ----------
  new_page(1, label)
  grid.rect(x = unit(0, "npc"), y = gy(0), width = unit(1, "npc"), height = unit(0.9, "in"),
            just = c("left", "top"), gp = gpar(fill = PG$navy, col = NA))
  grid.text("NEON Small Mammal Report Card", x = unit(0.12, "in"), y = gy(0.20),
            just = c("left", "top"), gp = gpar(col = "white", fontsize = 17, fontface = "bold"))
  grid.text(ascii(paste0(label, "  |  ", fmt_range(cs$date_min, cs$date_max))),
            x = unit(0.12, "in"), y = gy(0.56), just = c("left", "top"),
            gp = gpar(col = "white", fontsize = 10.5))
  grid.lines(x = unit(c(0, 1), "npc"), y = gy(0.90), gp = gpar(col = PG$gold, lwd = 3))
  y <- 1.06
  if (isTRUE(is_demo)) {
    grid.rect(x = unit(0, "npc"), y = gy(y), width = unit(1, "npc"), height = unit(0.30, "in"),
              just = c("left", "top"), gp = gpar(fill = PG$gold, col = NA))
    grid.text("DEMO DATASET - illustrative figures, not real NEON observations.",
              x = unit(0.5, "npc"), y = gy(y + 0.07), just = c("centre", "top"),
              gp = gpar(fontsize = 10, fontface = "bold", col = PG$ink))
    y <- y + 0.44
  }
  top_sp <- if (nrow(sp) > 0) sp$scientificName[1] else "n/a"
  lead <- sprintf(paste("Across %s plots at %s, roughly %s trap-nights between %s recorded %s",
    "captures of %s individual animals spanning %s species. The most-captured species was %s;",
    "the recapture rate was %s%%."),
    cs$plots, label, format(cs$trap_nights, big.mark = ","), fmt_range(cs$date_min, cs$date_max),
    format(cs$total_captures, big.mark = ","), format(cs$individuals, big.mark = ","),
    cs$species, top_sp, cs$recap_rate)
  y <- draw_para(lead, y, fontsize = 10.5, gapAfter = 0.14)
  y <- draw_stat_grid(list(
    c(format(cs$total_captures, big.mark = ","), "captures"),
    c(format(cs$individuals, big.mark = ","), "individuals"),
    c(as.character(cs$species), "species"),
    c(paste0(cs$recap_rate, "%"), "recapture rate"),
    c(format(cs$trap_nights, big.mark = ","), "trap-nights"),
    c(as.character(cs$legendary), "10+ caught")), y, ncol = 3)
  y <- draw_para(paste("Counts are minima from box-trapping conditioned on effort - not a landscape census.",
    "Trap-nights: set-and-empty = 1, partial = 0.5, not-set = 0. Recapture rate counts cross-session",
    "re-encounters, distinct from the within-bout recaptures used to estimate detection."),
    y, fontsize = 8, col = PG$muted, gapAfter = 0.08)
  if (!is.null(bio)) y <- draw_para(bio, y, fontsize = 9.5, fontface = 3, gapAfter = 0.14)
  y <- draw_h4("Diversity", y)
  y <- draw_stat_grid(list(
    c(as.character(hn$q0), "richness (q0)"),
    c(ifelse(is.na(hn$q1), "-", format(hn$q1, nsmall = 1)), "common (q1)"),
    c(ifelse(is.na(hn$q2), "-", format(hn$q2, nsmall = 1)), "dominant (q2)"),
    c(ifelse(is.na(hn$even), "-", format(hn$even, nsmall = 2)), "evenness")), y, ncol = 4, cellH = 0.46)
  y <- draw_para(sprintf(paste("Hill numbers are effective species counts (q0 >= q1 >= q2);",
    "evenness = q1/q0 (1 = even). Abundances are distinct individuals per species (genus-only IDs",
    "excluded), so a heavily re-trapped animal isn't double-counted. Descriptive of this sample,",
    "not effort-rarefied; n = %s individuals."), format(hn$n_ind, big.mark = ",")), y, fontsize = 9)

  # ---------- PAGE 2 (detail) ----------
  new_page(2, label); y <- 0.5
  y <- draw_h4("Species recorded", y)
  sp_show <- utils::head(sp, 14)
  rows <- lapply(seq_len(nrow(sp_show)), function(i) {
    nm <- sp_show$scientificName[i]
    if (!is.na(sp_show$nickname[i])) nm <- paste0(nm, " (", sp_show$nickname[i], ")")
    c(nm, format(sp_show$individuals[i], big.mark = ","),
      format(sp_show$captures[i], big.mark = ","),
      ifelse(is.na(sp_show$avg_weight[i]), "-", as.character(sp_show$avg_weight[i])))
  })
  rh_fn <- function() { new_page(2, label); 0.5 }
  y <- draw_table(c("Species", "Indiv.", "Caps", "Adult g"), rows,
                  colx = c(0.00, 0.66, 0.78, 0.90), yTop = y, faces = c(3, 1, 1, 1), repeat_header = rh_fn)
  more_n <- nrow(sp) - nrow(sp_show)
  if (more_n > 0) y <- draw_para(sprintf("+ %d more taxa recorded", more_n), y, 9, PG$muted, 3)
  y <- draw_h4("Population structure", y)
  y <- draw_para(sprintf("Sex ratio: %s (%s F, %s M of %s handled). Life stage: %s.",
    sex_ratio, fmt_int(cs$n_female), fmt_int(cs$n_male), fmt_int(n_handled), stage_txt), y, 9.5)
  y <- draw_h4("Breeding & phenology", y)
  y <- draw_para(repro_txt, y, 9.5, gapAfter = 0.04)
  y <- draw_para(paste("Reproductive condition is scored from field-collected fields and is most reliable",
    "for adults. These are counts of distinct individuals, not rates; a window spanning seasons mixes cohorts."),
    y, 8.5, PG$muted)
  y <- draw_h4("Methods: detection-corrected abundance", y)
  p_txt <- if (!is.null(cc) && !is.null(cc$series) && nrow(cc$series) > 0 && !is.na(cc$mean_p))
    sprintf("~%.0f%% per-bout capture probability (Model M0), across %d of %d estimable bouts.",
            100 * cc$mean_p, cc$n_estimable, cc$n_bouts)
  else "not estimable here (single-night grids or too few within-bout recaptures - MNKA & CPUE are the right index for these)."
  y <- draw_para(paste("Per-bout detection:", p_txt,
    "Closed-capture estimators (Schnabel for >=3-night bouts, Chapman for 2) on within-bout recaptures;",
    "N is floored at MNKA and a point estimate needs >= 3 within-bout recaptures, else the month shows the MNKA index only."),
    y, 9)

  # ---------- PAGE 3 (notable + charts) ----------
  new_page(3, label, footer_full = TRUE); y <- 0.5
  if (!is.null(lb) && nrow(lb) > 0) {
    y <- draw_h4("Notable individuals", y)
    mc <- lb[which.max(lb$captures), ]
    nrows <- list(c("Most caught", as.character(mc$scientificName), paste0(fmt_int(mc$captures), " captures")))
    if (any(is.finite(lb$max_weight))) {
      hv <- lb[which.max(replace(lb$max_weight, !is.finite(lb$max_weight), -Inf)), ]
      nrows <- c(nrows, list(c("Heaviest", as.character(hv$scientificName), paste0(hv$max_weight, " g"))))
    }
    cp <- lb[!lb$tag_suspect & !is.na(lb$career_days), ]
    if (nrow(cp) > 0) { cr <- cp[which.max(cp$career_days), ]
      nrows <- c(nrows, list(c("Longest career", as.character(cr$scientificName),
                               paste0(fmt_int(cr$career_days), " d")))) }
    y <- draw_table(c("", "Species", "Value"), nrows, colx = c(0.00, 0.24, 0.66),
                    yTop = y, faces = c(2, 3, 1), repeat_header = function() { new_page(3, label, TRUE); 0.5 })
    y <- draw_para(paste("Career length excludes likely reused ear-tags (career > 550 d or a > 300 d gap)."),
                   y, 8, PG$muted)
  }
  if (has_gg) {
    y <- embed_chart(chart_mnka(d, cs), y, 3.5)
    embed_chart(chart_species(d, cs), y, 3.0)
  } else {
    draw_para("(Charts require the ggplot2 package, which isn't available in this build.)", y, 9, PG$muted)
  }
  popViewport()
  invisible(file)
}

# ---- two-site comparison report -------------------------------------------
# Side-by-side PDF for two sites: a winner-highlighted metric table, species
# overlap, top species per site, and (if ggplot2) composition charts on page 2.
render_compare_pdf <- function(file, dA, labelA, dB, labelB) {
  pack <- function(d) list(
    cs = community_stats(d), hn = hill_numbers(d),
    sp = utils::head(species_summary(d), 6),
    all_sp = sort(unique(species_level_only(dplyr::filter(d, !is.na(.data$scientificName)))$scientificName)))
  A <- pack(dA); B <- pack(dB)
  codeA <- sub("[ ].*$", "", labelA); codeB <- sub("[ ].*$", "", labelB)
  has_gg <- requireNamespace("ggplot2", quietly = TRUE)

  PDF_DEV(file, width = PG$w, height = PG$h, onefile = TRUE)
  on.exit(grDevices::dev.off(), add = TRUE)

  new_page(1, "")
  y <- 0.05
  grid.text(ascii("Two-site comparison"), x = unit(0, "npc"), y = gy(y),
            just = c("left", "top"), gp = gpar(fontsize = 20, fontface = "bold", col = PG$navy))
  y <- y + 0.36
  grid.text(ascii(sprintf("%s   vs   %s", labelA, labelB)), x = unit(0, "npc"), y = gy(y),
            just = c("left", "top"), gp = gpar(fontsize = 11, col = PG$muted))
  y <- y + 0.44

  cax <- 0.58; cbx <- 0.79
  grid.text("Metric", x = unit(0, "npc"), y = gy(y), just = c("left", "top"), gp = gpar(fontface = "bold", fontsize = 10, col = PG$navy))
  grid.text(ascii(codeA), x = unit(cax, "npc"), y = gy(y), just = c("left", "top"), gp = gpar(fontface = "bold", fontsize = 10, col = PG$navy))
  grid.text(ascii(codeB), x = unit(cbx, "npc"), y = gy(y), just = c("left", "top"), gp = gpar(fontface = "bold", fontsize = 10, col = PG$navy))
  grid.lines(x = unit(c(0, 1), "npc"), y = gy(y + 0.19), gp = gpar(col = PG$line, lwd = 1))
  y <- y + 0.30
  mrow <- function(lab, va, vb, fmt = function(x) format(round(x), big.mark = ","), higher = TRUE) {
    tie <- is.na(va) || is.na(vb) || va == vb
    aw <- !tie && ((va > vb) == higher); bw <- !tie && !aw && !(va == vb)
    list(lab = lab, sa = fmt(va), sb = fmt(vb), fa = if (aw) 2 else 1, fb = if (bw) 2 else 1)
  }
  rows <- list(
    mrow("Captures", A$cs$total_captures, B$cs$total_captures),
    mrow("Individuals", A$cs$individuals, B$cs$individuals),
    mrow("Species (richness)", A$cs$species, B$cs$species),
    mrow("Effective common species (Hill q1)", A$hn$q1, B$hn$q1, fmt = function(x) format(round(x, 1), nsmall = 1)),
    mrow("Evenness (0-1)", A$hn$even, B$hn$even, fmt = function(x) format(round(x, 2), nsmall = 2)),
    mrow("Recapture rate", A$cs$recap_rate, B$cs$recap_rate, fmt = function(x) paste0(round(x, 1), "%")),
    mrow("Trap-nights (effort)", A$cs$trap_nights, B$cs$trap_nights))
  for (i in seq_along(rows)) {
    r <- rows[[i]]
    if (i %% 2 == 0)
      grid.rect(x = unit(0, "npc"), y = gy(y), width = unit(1, "npc"), height = unit(0.22, "in"),
                just = c("left", "top"), gp = gpar(fill = PG$zebra, col = NA))
    grid.text(ascii(r$lab), x = unit(0, "npc"), y = gy(y + 0.02), just = c("left", "top"), gp = gpar(fontsize = 9.5))
    grid.text(ascii(r$sa), x = unit(cax, "npc"), y = gy(y + 0.02), just = c("left", "top"),
              gp = gpar(fontsize = 9.5, fontface = r$fa, col = if (r$fa == 2) PG$navy else PG$ink))
    grid.text(ascii(r$sb), x = unit(cbx, "npc"), y = gy(y + 0.02), just = c("left", "top"),
              gp = gpar(fontsize = 9.5, fontface = r$fb, col = if (r$fb == 2) PG$navy else PG$ink))
    y <- y + 0.24
  }
  y <- y + 0.05
  grid.text("Higher value per row in bold navy. Diversity is Hill numbers over distinct individuals.",
            x = unit(0, "npc"), y = gy(y), just = c("left", "top"), gp = gpar(fontsize = 8, col = PG$muted))
  y <- y + 0.34

  shared <- intersect(A$all_sp, B$all_sp); onlyA <- setdiff(A$all_sp, B$all_sp); onlyB <- setdiff(B$all_sp, A$all_sp)
  y <- draw_h4("Species overlap", y)
  y <- draw_para(sprintf("%d species shared - %d only at %s - %d only at %s.",
                         length(shared), length(onlyA), codeA, length(onlyB), codeB), y, 9.5)
  y <- y + 0.12

  y <- draw_h4("Top species (by individuals)", y)
  grid.text(ascii(codeA), x = unit(0, "npc"), y = gy(y), just = c("left", "top"), gp = gpar(fontsize = 9.5, fontface = "bold", col = PG$navy))
  grid.text(ascii(codeB), x = unit(0.5, "npc"), y = gy(y), just = c("left", "top"), gp = gpar(fontsize = 9.5, fontface = "bold", col = PG$navy))
  y <- y + 0.24
  splist <- function(p, x0) {
    yy <- y
    for (i in seq_len(nrow(p$sp))) {
      grid.text(ascii(sprintf("%s  (%s ind)", p$sp$scientificName[i], format(p$sp$individuals[i], big.mark = ","))),
                x = unit(x0, "npc"), y = gy(yy), just = c("left", "top"), gp = gpar(fontsize = 9, fontface = 3))
      yy <- yy + 0.2
    }
    yy
  }
  y <- max(splist(A, 0.0), splist(B, 0.5)) + 0.1
  popViewport()

  if (has_gg) {
    new_page(2, sprintf("%s vs %s", codeA, codeB)); yy <- 0.5
    yy <- draw_h4(sprintf("Species composition - %s", codeA), yy)
    yy <- embed_chart(chart_species(dA, A$cs), yy, 3.2)
    yy <- draw_h4(sprintf("Species composition - %s", codeB), yy)
    embed_chart(chart_species(dB, B$cs), yy, 3.2)
    popViewport()
  }
  invisible(file)
}
