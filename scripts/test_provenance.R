#!/usr/bin/env Rscript
# Provenance and shell contracts that live in global.R: the site-code allowlist,
# the NEON citation and data-through accessors, the NEON issue-log notes, the
# suite registry and the build digest. No network. Run from the repository root:
#   Rscript --vanilla scripts/test_provenance.R
Sys.setenv(SMT_LIVE = "0")
source("global.R")
stopifnot(is.null(load_site_bundle(file.path("data/sites", "../site_index.rds"))),
          is.null(load_site_bundle("../site_index")), is.null(load_site_bundle("srer")),
          is.null(load_site_bundle("ZZZZ")), is.null(load_site_bundle(NA_character_)),
          is.null(load_site_env("../env/SRER")), !is.null(load_site_bundle("SRER")),
          identical(attr(load_site_env("SRER"), "source"), "neon"))
cat("allowlist ok\n")
meta <- list(product = "DP1.10072.001", release = "RELEASE-2026",
             doi = "https://doi.org/10.48443/a83h-tb34", data_through = "2024-11-20")
want <- "NEON (National Ecological Observatory Network). Small mammal box trapping (DP1.10072.001), RELEASE-2026. https://doi.org/10.48443/a83h-tb34. Dataset accessed from https://data.neonscience.org. Licensed CC BY 4.0."
stopifnot(identical(neon_citation_text(meta), want), is.null(neon_citation_text(NULL)),
          identical(neon_data_through(meta), as.Date("2024-11-20")), is.na(neon_data_through(NULL)))
log <- data.frame(
  id = c("1", "2", "3", "4", "5"),
  issueDate = c("2021-01-01T00:00:00Z", "2023-05-01T00:00:00Z", "2022-03-01T00:00:00Z", "2020-01-01T00:00:00Z", "2024-01-01T00:00:00Z"),
  resolvedDate = NA, dateRangeStart = NA, dateRangeEnd = NA,
  locationAffected = c("All", "BLAN, HEAL, SRER", "D14", "Core sites with > 4 bouts HARV, SCBI", "SRERX"),
  issue = paste("issue", 1:5), resolution = paste("res", 1:5), stringsAsFactors = FALSE)
s <- neon_site_notes("SRER", log)
stopifnot(identical(s$id, c("2", "3")), inherits(s$date, "Date"),
          identical(neon_site_notes("HARV", log)$id, "4"),
          identical(neon_site_notes("JORN", log)$id, "3"),   # JORN is D14
          identical(neon_product_notes(log)$id, "1"),
          nrow(neon_site_notes("SRER", NULL)) == 0, nrow(neon_product_notes(NULL)) == 0,
          identical(names(s), c("date", "id", "issue", "resolution")))
stopifnot(nrow(SUITE_APPS) == 10, sum(SUITE_APPS$is_self) == 1, SUITE_APPS$key[1] == "small-mammal")
cat("BUILD_DIGEST", BUILD_DIGEST, "\n")
f <- build_digest(manifest = "nope.json"); cat("fallback", f, "\n")
stopifnot(grepl("^m-[0-9a-f]{32}$", BUILD_DIGEST), grepl("^f-[0-9a-f]{32}$", f))
cat("provenance ok\n")
cat("All provenance checks passed.
")
