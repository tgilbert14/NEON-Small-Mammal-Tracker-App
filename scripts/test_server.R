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
})

cat(sprintf("\nAll %d server contract tests passed.\n", passed))
