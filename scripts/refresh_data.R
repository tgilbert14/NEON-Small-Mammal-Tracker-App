# ===========================================================================
# refresh_data.R — build the bundled per-site "database"
#
# Downloads each NEON site's full small-mammal record (DP1.10072.001), trims it
# to the columns the app uses, and xz-compresses one .rds per site into
# data/sites/<SITE>.rds. Each trimmed+compressed site is tiny (~0.1–0.5 MB), so
# all ~47 fit in the app bundle and load instantly — no live download for users.
#
# RESUMABLE: skips sites whose .rds already exists. Delete a file to re-pull it.
#
# !! AFTER REBUILDING BUNDLES YOU MUST REPUBLISH !! The deployed app keeps serving
# the OLD data until you do. Posit Connect Cloud serves the *published* git
# snapshot, and manifest.json pins a CHECKSUM per bundled file — so a changed .rds
# whose checksum wasn't refreshed will not take effect (and can fail the deploy).
# Full refresh sequence, in order:
#   1. delete data/sites/*.rds (or just the sites to refresh) and re-run THIS script
#   2. Rscript scripts/write_manifest.R        # regenerate manifest.json checksums
#   3. git add data/ manifest.json && git commit
#   4. push, then republish on Connect Cloud (git-backed redeploy)
# The live app shows the new data only once step 4 finishes — a local rebuild
# alone changes nothing in production. (This bit us once: bundles looked updated
# locally but the deployed app didn't change until a republish.)
#
# Run from the project root:
#   Rscript scripts/refresh_data.R
# ===========================================================================

options(timeout = 1800)
suppressMessages({
  library(neonUtilities)
  library(dplyr)
  library(tibble)
  library(jsonlite)   # freshness-state marker (data/.refresh_state.json)
})

# NEON API token — set env var NEON_TOKEN to raise the anonymous rate limit.
.neon_token <- Sys.getenv("NEON_TOKEN", unset = NA_character_)
if (!is.na(.neon_token) && nchar(.neon_token) > 10) {
  cat("Using NEON API token (higher rate limits).\n")
} else {
  .neon_token <- NA_character_
}

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
    {
      args <- list(dpID = "DP1.10072.001", site = s, startdate = start_d, enddate = end_d,
                   package = "basic", check.size = "F")
      if (!is.na(.neon_token)) args$token <- .neon_token
      do.call(loadByProduct, args)
    },
    error = function(e) { cat(sprintf("    ERROR %s: %s\n", s, conditionMessage(e))); NULL })
  if (is.null(raw) || is.null(raw$mam_pertrapnight) || nrow(raw$mam_pertrapnight) == 0) {
    cat(sprintf("    no data for %s\n", s)); next
  }
  d <- raw$mam_pertrapnight
  d <- d[, intersect(keep, colnames(d)), drop = FALSE]
  d <- tibble::as_tibble(d)
  # Materialize any ALTREP columns (neonUtilities returns arrow-backed strings) to
  # plain base vectors BEFORE saving — otherwise the .rds carries an arrow ALTVEC
  # that reads back length-zero anywhere arrow can't unserialize it (older R, no
  # arrow loaded), silently emptying every character column. See fix_bundles_altrep.R.
  for (nm in names(d)) {
    col <- d[[nm]]
    d[[nm]] <- if (inherits(col, "Date")) structure(as.numeric(col[seq_along(col)]), class = "Date")
               else if (is.factor(col)) factor(as.character(col[seq_along(col)]))
               else col[seq_along(col)]
  }
  saveRDS(d, out, compress = "xz")
  caps <- sum(!is.na(d$tagID))
  cat(sprintf("    saved %s: %d rows, %d captures, %.2f MB\n", s, nrow(d), caps, file.size(out)/1e6))
  summary_rows[[s]] <- data.frame(site = s, rows = nrow(d), captures = caps,
                                  mb = round(file.size(out)/1e6, 2))
}

if (length(summary_rows)) {
  res <- do.call(rbind, summary_rows)
  cat(sprintf("\nDone. %d new sites, total %.1f MB.\n", nrow(res), sum(res$mb)))
}
n_ok <- length(list.files(out_dir, pattern = "\\.rds$"))
cat(sprintf("Bundle now has %d/%d sites.\n", n_ok, length(sites)))

# ---- freshness assertion (did the data actually advance?) ------------------
# A monthly re-pull that "succeeds" but brings back NO newer records is a SILENT
# stall — NEON didn't publish, the token rate-limited, or the date window was
# wrong — and we'd cheerfully redeploy the same data forever. So we record the
# freshest collectDate seen across ALL bundles and compare it to the last run's
# value (committed in data/.refresh_state.json). If it did NOT advance, we log
# LOUDLY (and the marker is committed so the bot's commit message can say so).
freshest_collect_date <- function(dir) {
  fs <- list.files(dir, pattern = "\\.rds$", full.names = TRUE)
  mx <- NA_character_
  for (f in fs) {
    d <- tryCatch(readRDS(f), error = function(e) NULL)
    if (is.null(d) || !"collectDate" %in% names(d)) next
    cd <- substr(as.character(d$collectDate), 1, 10)
    cd <- max(cd[!is.na(cd) & nzchar(cd)], na.rm = TRUE)
    if (length(cd) && (is.na(mx) || cd > mx)) mx <- cd
  }
  mx
}

state_path <- "data/.refresh_state.json"
prev_max <- NA_character_
if (file.exists(state_path)) {
  prev <- tryCatch(jsonlite::fromJSON(state_path), error = function(e) NULL)
  if (!is.null(prev) && !is.null(prev$freshest_collectDate))
    prev_max <- as.character(prev$freshest_collectDate)
}

new_max <- freshest_collect_date(out_dir)
advanced <- !is.na(new_max) && (is.na(prev_max) || new_max > prev_max)

if (is.na(new_max)) {
  cat("!! FRESHNESS: could not read any collectDate from the rebuilt bundle.\n")
} else if (advanced) {
  cat(sprintf("FRESHNESS OK: freshest record advanced %s -> %s.\n",
              ifelse(is.na(prev_max), "(none)", prev_max), new_max))
} else {
  cat(sprintf("!! FRESHNESS WARNING: freshest record did NOT advance (still %s). NEON may not have published new data, the API may have rate-limited, or the window is wrong. Investigate before trusting this redeploy.\n",
              new_max))
}

# Persist the marker (committed by the workflow) so the next run can compare AND
# so the commit step can report what actually changed.
tryCatch(
  jsonlite::write_json(
    list(freshest_collectDate = ifelse(is.na(new_max), NULL, new_max),
         previous_freshest    = ifelse(is.na(prev_max), NULL, prev_max),
         advanced             = isTRUE(advanced),
         sites_built          = n_ok,
         refreshed_at_utc     = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")),
    state_path, auto_unbox = TRUE, pretty = TRUE),
  error = function(e) cat(sprintf("(could not write %s: %s)\n", state_path, conditionMessage(e))))

# Mass-failure guard: the workflow `rm -f data/sites/*.rds` BEFORE this runs, then
# deploys + opens a data PR after. If a bad NEON-pull day left us with far too few
# bundles, stop() here so the job fails and neither the deploy nor the (now
# deletion-heavy) PR step runs — far safer than shipping/committing a shrunken set.
# Per-site failures are already skipped above; this only trips on a mass failure.
floor_n <- max(30L, as.integer(ceiling(0.75 * length(sites))))
if (n_ok < floor_n)
  stop(sprintf("Only %d/%d site bundles built (< %d) — aborting before deploy/PR so a mass NEON-pull failure can't ship or commit a shrunken dataset.",
               n_ok, length(sites), floor_n))
