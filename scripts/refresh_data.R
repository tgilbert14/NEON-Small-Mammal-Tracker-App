# ===========================================================================
# refresh_data.R — build the bundled per-site "database"
#
# Downloads each NEON site's full small-mammal record (DP1.10072.001), trims it
# to the columns the app uses, and xz-compresses one .rds per site into
# data/sites/<SITE>.rds. Each trimmed+compressed site is tiny (~0.1–0.5 MB), so
# all ~47 fit in the app bundle and load instantly — no live download for users.
#
# RESUMABLE: skips sites whose .rds already exists. Delete a file to re-pull it.
# To refresh with newer data: delete data/sites/*.rds (or the ones you want) and
# re-run, then redeploy.
#
# Run from the project root:
#   Rscript scripts/refresh_data.R
# ===========================================================================

options(timeout = 1800)
suppressMessages({
  library(neonUtilities)
  library(dplyr)
  library(tibble)
})

source("R/site_metadata.R")  # for the canonical site list

keep <- c("tagID","individualCode","taxonID","scientificName","taxonRank",
  "identificationQualifier","nativeStatusCode","plotID","trapCoordinate",
  "decimalLatitude","decimalLongitude","elevation","nlcdClass","namedLocation",
  "collectDate","nightuid","trapStatus","recapture","fate","hindfootLength",
  "earLength","tailLength","totalLength","weight","lifeStage","sex","testes",
  "nipples","pregnancyStatus","vagina","domainID","siteID","remarks")

out_dir <- "data/sites"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

start_d <- "2013-01"
end_d   <- format(Sys.Date(), "%Y-%m")

sites <- neon_sites$site
cat(sprintf("Refreshing %d sites (%s → %s) into %s/\n\n", length(sites), start_d, end_d, out_dir))

summary_rows <- list()
for (s in sites) {
  out <- file.path(out_dir, paste0(s, ".rds"))
  if (file.exists(out)) { cat(sprintf("• %-5s skip (exists, %.2f MB)\n", s, file.size(out)/1e6)); next }

  cat(sprintf("• %-5s downloading…\n", s))
  raw <- tryCatch(
    loadByProduct(dpID = "DP1.10072.001", site = s, startdate = start_d, enddate = end_d,
                  package = "basic", check.size = "F"),
    error = function(e) { cat(sprintf("    ERROR %s: %s\n", s, conditionMessage(e))); NULL })
  if (is.null(raw) || is.null(raw$mam_pertrapnight) || nrow(raw$mam_pertrapnight) == 0) {
    cat(sprintf("    no data for %s\n", s)); next
  }
  d <- raw$mam_pertrapnight
  d <- d[, intersect(keep, colnames(d)), drop = FALSE]
  saveRDS(tibble::as_tibble(d), out, compress = "xz")
  caps <- sum(!is.na(d$tagID))
  cat(sprintf("    saved %s: %d rows, %d captures, %.2f MB\n", s, nrow(d), caps, file.size(out)/1e6))
  summary_rows[[s]] <- data.frame(site = s, rows = nrow(d), captures = caps,
                                  mb = round(file.size(out)/1e6, 2))
}

if (length(summary_rows)) {
  res <- do.call(rbind, summary_rows)
  cat(sprintf("\nDone. %d new sites, total %.1f MB.\n", nrow(res), sum(res$mb)))
}
cat(sprintf("Bundle now has %d/%d sites.\n",
            length(list.files(out_dir, pattern = "\\.rds$")), length(sites)))
