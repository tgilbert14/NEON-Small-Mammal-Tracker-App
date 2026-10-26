#!/usr/bin/env Rscript

# Server contract tests (shiny::testServer, no browser, no network). Covers the
# site-picker cascade (contract B), the site-code allowlist, clearing the
# Track-an-individual picker, and the SRER startup default. Run from the
# repository root: `Rscript --vanilla scripts/test_server.R`.

args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grepl("^--file=", args)])
root <- if (length(file_arg)) normalizePath(file.path(dirname(file_arg), "..")) else
  normalizePath(".")
setwd(root)
Sys.setenv(SMT_LIVE = "0")
suppressPackageStartupMessages(source("global.R"))

# Record what the server sends to the select inputs. server.R calls these
# unqualified, so a global wrapper sees every call and still forwards it.
sent <- new.env()
sent$select <- list()
updateSelectInput <- function(session, inputId, label = NULL, choices = NULL, selected = NULL) {
  sent$select[[length(sent$select) + 1L]] <- list(id = inputId, choices = choices, selected = selected)
  shiny::updateSelectInput(session, inputId, label = label, choices = choices, selected = selected)
}
last_selected <- function(id) {
  hits <- Filter(function(m) identical(m$id, id) && !is.null(m$selected), sent$select)
  if (length(hits)) hits[[length(hits)]]$selected else NULL
}
# Notifications: capture the text so the allowlist message can be checked.
notes <- new.env(); notes$text <- character(0)
showNotification <- function(ui, ...) {
  notes$text <- c(notes$text, paste(as.character(ui), collapse = ""))
  invisible("id")
}

source("server.R")
# ui.R helpers (footer suite directory and provenance) are checked directly
ui_env <- new.env()
suppressPackageStartupMessages(sys.source("ui.R", envir = ui_env))
html_of <- function(x) paste(as.character(x), collapse = "")
TEST_META <- list(product = "DP1.10072.001", release = "RELEASE-2099",
                  doi = "https://doi.org/10.48443/test-0000", data_through = "2099-09-30")

passed <- 0L
check <- function(ok, label) {
  if (!isTRUE(ok)) stop(sprintf("FAIL: %s", label), call. = FALSE)
  passed <<- passed + 1L
  cat(sprintf("PASS %02d: %s\n", passed, label))
}

idx <- SITE_INDEX
wa <- sort(idx$site[idx$state %in% "WA"])
stopifnot(length(wa) >= 2)

shiny::testServer(server, {
  # (d) startup: the state select defaults to Arizona, so SRER is the default site
  check(identical(last_selected("stateSel"), "AZ"), "startup selects AZ (SRER default)")
  session$setInputs(stateSel = "AZ")
  check(identical(last_selected("site"), "SRER"), "AZ cascade selects SRER")

  # (a) contract B: a site in another state goes through rv$pendingSite ...
  session$setInputs(siteExplore = wa[2])
  check(identical(rv$pendingSite, wa[2]), sprintf("cross-state pick (%s) parks the site as pending", wa[2]))
  check(identical(last_selected("stateSel"), "WA"), "cross-state pick moves the state select to WA")
  # a late, unrelated state event must not drop the pending site
  session$setInputs(stateSel = "NM")
  check(identical(rv$pendingSite, wa[2]), "an unrelated state event keeps the pending site")
  session$setInputs(stateSel = "WA")
  check(is.null(rv$pendingSite), "the WA state event consumes the pending site")
  check(identical(last_selected("site"), wa[2]),
        sprintf("deep link to a non-first site lands on %s, not %s", wa[2], wa[1]))
  check(!is.null(rv$data) && identical(mode_chr(rv$data$siteID), wa[2]), "the cross-state site loaded")

  # ... and a second site in the SAME state sets the site select directly
  session$setInputs(siteExplore = wa[1])
  check(is.null(rv$pendingSite), "same-state pick leaves no pending site")
  check(identical(last_selected("site"), wa[1]), sprintf("same-state pick selects %s in the site select", wa[1]))
  check(identical(mode_chr(rv$data$siteID), wa[1]), "the same-state site loaded")

  # the label (report card, exports) carries the loaded data's own span
  span <- fmt_range(safe_date_min(rv$data$date), safe_date_max(rv$data$date))
  check(endsWith(rv$label, span) && identical(rv$site_name, site_label(wa[1])),
        "the label ends with the loaded data span, the hero name is the site alone")
  check(grepl(gsub("[^A-Za-z0-9]+", "-", format(safe_date_max(rv$data$date), "%b %Y")),
              export_slug(), fixed = TRUE), "export filenames carry the data span")
  pop <- as.character(site_popup_html(SITE_INDEX[SITE_INDEX$site == "SRER", , drop = FALSE]))
  check(!grepl("onclick", pop, fixed = TRUE) && grepl("data-smt-value='SRER'", pop, fixed = TRUE),
        "the site popup carries its payload in data attributes, not onclick")

  # item 1: the species banner's denominator is the Individuals card, and the
  # named species is a species-level row (never a genus-only "sp.")
  ban <- html_of(output$speciesBarInsight$html)
  tot <- community_stats(rv$data, rv$lb)$individuals
  check(grepl(sprintf(" of %s)", fmt_int(tot)), ban, fixed = TRUE),
        "species banner share is out of the Individuals card count")
  named <- sub("^.*<i>([^<]+)</i>.*$", "\\1", ban)
  check(nrow(species_level_only(data.frame(scientificName = named))) == 1,
        sprintf("species banner names a species-level row (%s)", named))

  # item 8: About renders cleanly without bundle metadata, and shows the
  # citation with a linked DOI when it is present
  about0 <- html_of(output$aboutPanel$html)
  check(!grepl("neon-citation", about0, fixed = TRUE) && !grepl(">NA<", about0, fixed = TRUE),
        "About has no citation block and no NA without BUNDLE_META")
  old_meta <- BUNDLE_META
  assign("BUNDLE_META", TEST_META, envir = globalenv())
  about1 <- html_of(neon_citation_tag())
  assign("BUNDLE_META", old_meta, envir = globalenv())
  check(grepl('href="https://doi.org/10.48443/test-0000"', about1, fixed = TRUE) &&
          grepl("RELEASE-2099", about1, fixed = TRUE), "with BUNDLE_META the citation links the DOI")

  # (c) clearing the Track-an-individual picker returns the dossier to its prompt
  tag <- rv$lb$tagID[1]
  session$setInputs(indiv = tag)
  check(identical(rv$tag, tag), "picking an individual sets the tag")
  session$setInputs(indiv = "")
  check(is.null(rv$tag), "clearing the picker sets the tag to NULL")

  # (b) allowlist: refused codes load nothing and say so
  before <- mode_chr(rv$data$siteID)
  for (bad in c("../site_index", "srer", "ZZZZ")) {
    n0 <- length(notes$text)
    session$setInputs(siteExplore = bad)
    check(identical(mode_chr(rv$data$siteID), before), sprintf("'%s' is refused without loading", bad))
    check(length(notes$text) > n0 && grepl("No NEON site called", notes$text[length(notes$text)], fixed = TRUE),
          sprintf("'%s' shows the no-such-site notice", bad))
  }
  session$setInputs(siteExplore = "<b>x</b>")
  check(grepl("&lt;b&gt;", notes$text[length(notes$text)], fixed = TRUE), "markup in a refused code is escaped")

  # ---- phase B: load SRER (cross-state: the AZ state event consumes it) ----
  session$setInputs(siteExplore = "SRER")
  session$setInputs(stateSel = "AZ")
  check(identical(mode_chr(rv$data$siteID), "SRER"), "SRER loaded for the phase B checks")

  # item B1: the seasonal card reads the month-adjusted yearly index and its
  # rail comes from seasonal_verdict()
  sea <- html_of(output$seasonalDriver$html)
  resp <- annual_cpue_index(rv$data, ok_only = TRUE)
  check(nrow(resp) >= 3 && all(resp$ok), "seasonal response is annual_cpue_index(ok_only = TRUE)")
  check(grepl("adjusted for which months were trapped", sea, fixed = TRUE),
        "seasonal card says the yearly rate is month-adjusted")
  check(!grepl("tracks (more|fewer) animals", sea), "seasonal lead no longer claims more or fewer animals")

  # item B2: the driver gate is p < 0.02, and the detection auto-overlay obeys it
  check(identical(ENV_P_GATE, 0.02), "monthly driver gate is p < 0.02 (calibrated)")
  pv <- env_pval()
  ov <- detect_overlay()
  if (is.null(pv) || !is.finite(pv$p) || pv$p >= ENV_P_GATE) {
    check(is.null(ov), sprintf("no auto overlay when the dredge p (%s) is not below 0.02",
                               if (is.null(pv)) "NA" else format(round(pv$p, 3))))
  } else {
    check(isTRUE(ov$auto), "auto overlay present when the dredge p is below 0.02")
  }
  en <- html_of(output$envCorrNote$html)
  if (!is.null(pv) && is.finite(pv$p) && pv$p >= ENV_P_GATE)
    check(grepl("Apparent", en, fixed = TRUE), "a driver at p >= 0.02 reads as Apparent")

  # item B4: the three QC downloads write non-empty CSVs at SRER
  csv_rows <- function(path) nrow(utils::read.csv(path, stringsAsFactors = FALSE))
  check(csv_rows(output$dlQcReportCsv) > 0, "QC report CSV is non-empty at SRER")
  sp_flag <- NULL
  for (sp in unique(species_level_only(rv$data)$scientificName)) {
    fc <- flagged_measure_captures(rv$data, sp, "weight")
    if (!is.null(fc) && nrow(fc)) { sp_flag <- sp; n_fc <- nrow(fc); break }
  }
  check(!is.null(sp_flag), "SRER has a species with a flagged weight")
  session$setInputs(showOutliers = paste0(sp_flag, "||weight"))
  check(csv_rows(output$dlOutliersCsv) == n_fc, sprintf("outlier CSV writes exactly the %d rows shown", n_fc))
  session$setInputs(showMovers = 1)
  check(csv_rows(output$dlMoversCsv) == nrow(inspect_rows$movers) && nrow(inspect_rows$movers) > 0,
        "movers CSV writes exactly the rows shown")
  session$setInputs(indiv = rv$lb$tagID[1])
  qh <- utils::read.csv(output$qcHistoryCsv, stringsAsFactors = FALSE)
  check(nrow(qh) > 0 && "qc_flags" %in% names(qh), "QC history CSV carries a qc_flags column")

  # item B5: MNKA lines break across gaps and edge months are hollow
  mj <- paste(as.character(output$mnkaPlot), collapse = "")
  mn_s <- mnka_series(rv$data)
  check(nrow(insert_gap_breaks(mn_s, 2L)) > nrow(mn_s) && grepl("null", mj, fixed = TRUE),
        "MNKA lines carry gap-break rows (null y) so seasonal gaps are not bridged")
  check(!any(mn_s$edge) || grepl("circle-open", mj, fixed = TRUE),
        "edge months draw as hollow markers")
  check(!any(mn_s$edge) || grepl("edge month: MNKA is biased low here", mj, fixed = TRUE),
        "edge-month hover says MNKA is biased low")
})

# item 4: Hurlbert rarefaction on a small fixture
check(isTRUE(all.equal(hurlbert_rarefy(c(2, 1), 2), 1 + 2 / 3)), "E[S_2] of (2, 1) is 5/3")
check(isTRUE(all.equal(hurlbert_rarefy(c(5, 3, 2), 10), 3)), "rarefying to the full count returns raw richness")
check(isTRUE(all.equal(hurlbert_rarefy(c(5, 3, 2), 1), 1)), "a draw of one individual holds one species")
check(is.na(hurlbert_rarefy(c(5, 3, 2), 11)) && is.na(hurlbert_rarefy(numeric(0), 1)),
      "n above the pooled count, or no counts, is NA")

# item 7: footer suite directory lists all ten apps, the current one unlinked
nav <- html_of(ui_env$suite_directory_nav())
self <- SUITE_APPS[SUITE_APPS$is_self, ]
check(lengths(regmatches(nav, gregexpr("<li>", nav, fixed = TRUE))) == 10, "footer lists ten suite apps")
check(lengths(regmatches(nav, gregexpr("<a ", nav, fixed = TRUE))) == 9 &&
        grepl('aria-current="page"', nav, fixed = TRUE) &&
        !grepl(self$cover_url, nav, fixed = TRUE), "the current app is marked, not linked")
check(grepl('aria-label="NEON Explorer Suite directory"', nav, fixed = TRUE) &&
        grepl("NEON Cross-Product Response Atlas", nav, fixed = TRUE), "footer nav is named and uses the Atlas name")

# item 8: footer and landing line with and without bundle metadata
f0 <- html_of(ui_env$footer_provenance(NULL))
f1 <- html_of(ui_env$footer_provenance(TEST_META))
check(!grepl("ddl-footer-cite", f0, fixed = TRUE) && !grepl("NA", gsub("NEON|NSF", "", f0)) &&
        grepl("CC BY 4.0", f0, fixed = TRUE), "footer without BUNDLE_META: licence line only, no NA")
check(grepl("ddl-footer-cite", f1, fixed = TRUE) && grepl("10.48443/test-0000</a>", f1, fixed = TRUE),
      "footer with BUNDLE_META: citation with the DOI linked")
check(is.null(neon_provenance_line(NULL)) &&
        identical(neon_provenance_line(TEST_META), "NEON RELEASE-2099 \u00B7 data through Sep 2099"),
      "landing line reads NEON <release> \u00B7 data through <Mon YYYY>, NULL without metadata")

cat(sprintf("\nAll %d server contract tests passed.\n", passed))
