# ===========================================================================
# refresh_data.R — build the bundled per-site beetle "database"
#
# Downloads each NEON site's ground-beetle record (DP1.10022.001), assembles it
# into the app's tidy long schema (siteID, plotID, collectDate, taxonID,
# scientificName, individualCount, trapnights) via assemble_beetles(), and
# xz-compresses one .rds per site into data/sites/<SITE>.rds.
#
# RESUMABLE: skips sites whose .rds already exists. Delete a file to re-pull it.
# Run from the project root:   Rscript scripts/refresh_data.R
#
# Verify table/column names once before a full run:
#   names(neonUtilities::loadByProduct("DP1.10022.001", site="KONZ",
#         startdate="2018-05", enddate="2018-09", check.size="F"))
# ===========================================================================

options(timeout = 1800)
suppressMessages({
  library(neonUtilities)
  library(dplyr)
  library(tibble)
})

source("R/site_metadata.R")  # canonical site list
source("R/helpers.R")        # assemble_beetles()

out_dir <- "data/sites"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

start_d <- "2013-01"
end_d   <- format(Sys.Date(), "%Y-%m")
sites   <- neon_sites$site

cat(sprintf("Refreshing %d sites (%s → %s) into %s/\n\n",
            length(sites), start_d, end_d, out_dir))

for (s in sites) {
  out <- file.path(out_dir, paste0(s, ".rds"))
  if (file.exists(out)) { cat(sprintf("• %-5s skip (exists, %.2f MB)\n", s, file.size(out)/1e6)); next }

  cat(sprintf("• %-5s downloading…\n", s))
  raw <- tryCatch(
    loadByProduct(dpID = "DP1.10022.001", site = s, startdate = start_d, enddate = end_d,
                  package = "basic", check.size = "F"),
    error = function(e) { cat(sprintf("    ERROR %s: %s\n", s, conditionMessage(e))); NULL })
  if (is.null(raw)) next

  d <- tryCatch(assemble_beetles(raw), error = function(e) {
    cat(sprintf("    assemble error %s: %s\n", s, conditionMessage(e))); NULL })
  if (is.null(d) || !nrow(d)) { cat(sprintf("    no carabid data for %s\n", s)); next }

  # materialize any ALTREP/arrow-backed columns to plain base vectors before save
  for (nm in names(d)) {
    col <- d[[nm]]
    d[[nm]] <- if (inherits(col, "Date")) structure(as.numeric(col[seq_along(col)]), class = "Date")
               else col[seq_along(col)]
  }
  saveRDS(tibble::as_tibble(d), out, compress = "xz")
  cat(sprintf("    saved %s: %d rows, %d species, %.2f MB\n",
              s, nrow(d), dplyr::n_distinct(d$scientificName), file.size(out)/1e6))
}

cat(sprintf("\nDone. Bundle now has %d site files.\n",
            length(list.files(out_dir, pattern = "\\.rds$"))))
