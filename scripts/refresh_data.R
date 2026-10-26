# ===========================================================================
# refresh_data.R — build the bundled per-site "database"
#
# Downloads each NEON site's full small-mammal record (DP1.10072.001), trims it
# to the columns the app uses, and xz-compresses one .rds per site into the
# directory selected by SMT_SITE_OUT_DIR (data/sites by default). CI points that
# variable at an empty staging directory and swaps the result into data/sites
# only after this script proves the exact expected site set was built.
#
# RESUMABLE: skips sites whose .rds already exists. Delete a file to re-pull it.
#
# !! A LOCAL REBUILD IS NOT A RELEASE !! Connect serves the reviewed `main`
# snapshot, and manifest.json pins a checksum per bundled file. The supported flow
# is the producer/validator/publisher workflow in refresh-data.yml:
#   1. build all expected sites in an empty stage with THIS script
#   2. rebuild indexes and generate the manifest under pinned R/package inputs
#   3. verify exact sites, schema, indexes, checksums, helpers, and offline source
#   4. publish only the immutable candidate to a review branch (a reviewer opens the PR)
#   5. intentionally merge the reviewed PR; Connect republishes watched `main`
# Never hand-push a local partial refresh to production.
#
# Release pin: every site is pulled from NEON_RELEASE_PIN (scripts/producer_lib.R)
# with include.provisional = FALSE, and each site's tables must carry only that
# release. On success it writes data/bundle_meta.json (product, release, DOI,
# data window, freshness) with no wall-clock fields, so an unchanged rebuild is
# byte-identical.
#
# Run from the project root:
#   NEON_TOKEN=... Rscript scripts/refresh_data.R
# ===========================================================================

options(timeout = 1800)
suppressMessages({
  library(neonUtilities)
  library(dplyr)
  library(tibble)
  library(jsonlite)
})

source("scripts/producer_lib.R")
source("scripts/neon_product_meta.R")   # functions only; its main block does not run when sourced

# NEON data endpoints refuse anonymous requests, so the token is required.
.neon_token <- Sys.getenv("NEON_TOKEN", unset = "")
if (nchar(.neon_token) < 10)
  stop("NEON_TOKEN is missing or shorter than 10 characters. NEON data endpoints refuse anonymous requests; set the NEON_TOKEN secret or environment variable.", call. = FALSE)

# The citation is required: read the pinned release's DOI before the long pull.
.citation <- release_info(fetch_products(), NEON_RELEASE_PIN)
cat(sprintf("Pinned %s %s (DOI %s, generated %s)\n", NEON_PRODUCT, .citation$release,
            .citation$doi, .citation$release_generated))

source("R/site_metadata.R")  # for the canonical site list

out_dir <- Sys.getenv("SMT_SITE_OUT_DIR", unset = "data/sites")
if (!nzchar(out_dir)) stop("SMT_SITE_OUT_DIR must not be empty", call. = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
meta_path <- Sys.getenv("SMT_BUNDLE_META_PATH", unset = "data/bundle_meta.json")
report_dir <- Sys.getenv("SMT_REPORT_DIR", unset = "")

start_d <- "2013-01"
end_d   <- format(Sys.Date(), "%Y-%m")

sites <- neon_sites$site
cat(sprintf("Refreshing %d sites (%s to %s, %s) into %s/\n\n", length(sites), start_d, end_d,
            NEON_RELEASE_PIN, out_dir))

summary_rows <- list()
vocab <- list()
add_vocab <- function(field, tb, site) {
  if (is.null(tb) || !length(tb)) return()
  v <- names(tb); v[is.na(v)] <- "<NA>"
  vocab[[length(vocab) + 1L]] <<- data.frame(field = field, value = v, n = as.integer(tb), site = site)
}
fmt_tab <- function(tb) if (is.null(tb)) "(column absent)" else
  paste(sprintf("%s=%d", ifelse(is.na(names(tb)), "<NA>", names(tb)), as.integer(tb)), collapse = ", ")

for (s in sites) {
  out <- file.path(out_dir, paste0(s, ".rds"))
  if (file.exists(out)) { cat(sprintf("- %-5s skip (exists, %.2f MB)\n", s, file.size(out)/1e6)); next }

  cat(sprintf("- %-5s downloading\n", s))
  raw <- tryCatch(
    loadByProduct(dpID = NEON_PRODUCT, site = s, startdate = start_d, enddate = end_d,
                  package = "basic", release = NEON_RELEASE_PIN, include.provisional = FALSE,
                  check.size = FALSE, token = .neon_token),
    error = function(e) { cat(sprintf("    ERROR %s: %s\n", s, conditionMessage(e))); NULL })
  if (is.null(raw) || is.null(raw$mam_pertrapnight) || nrow(raw$mam_pertrapnight) == 0) {
    cat(sprintf("    no data for %s\n", s)); next
  }
  check_release(raw, NEON_RELEASE_PIN, s)
  res <- prepare_site_bundle(raw$mam_pertrapnight, raw$mam_perplotnight)
  d <- res$bundle; r <- res$report
  saveRDS(d, out, compress = "xz")
  caps <- sum(!is.na(d$tagID))
  cat(sprintf("    saved %s: %d rows, %d captures, %.2f MB\n", s, nrow(d), caps, file.size(out)/1e6))
  cat(sprintf("    perplotnight join: %d rows, %d joined, %d with no matching nightuid; carried [%s]%s\n",
              r$rows, r$joined, r$unmatched, paste(r$carried, collapse = ", "),
              if (length(r$absent)) sprintf("; absent [%s]", paste(r$absent, collapse = ", ")) else ""))
  if (length(r$conflicts))
    gh_annotation("warning", "Conflicting per-plot-night rows",
                  sprintf("%s: %d nightuid(s) with conflicting mam_perplotnight rows; disagreeing fields set to NA: %s",
                          s, length(r$conflicts), paste(utils::head(r$conflicts, 10), collapse = ", ")))
  cat(sprintf("    gridCompletion: %s\n", fmt_tab(r$gridCompletion)))
  cat(sprintf("    samplingImpractical: %s\n", fmt_tab(r$samplingImpractical)))
  cat(sprintf("    trapsSet vs distinct set traps (status 2-6): %d of %d plot-nights differ (%s)\n",
              r$traps_set$mismatched, r$traps_set$nights,
              if (is.na(r$traps_set$share)) "n/a" else sprintf("%.1f%%", 100 * r$traps_set$share)))
  cat(sprintf("    remarks scrub: %d email address(es) in %d row(s) replaced with %s\n",
              r$email_matches, r$email_rows, STAFF_TOKEN))
  add_vocab("gridCompletion", r$gridCompletion, s)
  add_vocab("samplingImpractical", r$samplingImpractical, s)
  summary_rows[[s]] <- data.frame(site = s, rows = nrow(d), captures = caps,
                                  unmatched = r$unmatched, emails = r$email_matches,
                                  traps_set_nights = r$traps_set$nights,
                                  traps_set_differ = r$traps_set$mismatched,
                                  mb = round(file.size(out)/1e6, 2))
  rm(raw, res, d); invisible(gc())
}

if (length(summary_rows)) {
  res <- do.call(rbind, summary_rows)
  cat(sprintf("\nDone. %d new sites, total %.1f MB.\n", nrow(res), sum(res$mb)))
  gh_summary(c("### Per-site producer log",
               "| site | rows | unmatched nightuid | emails scrubbed | trapsSet nights | trapsSet differs |",
               "|---|---:|---:|---:|---:|---:|",
               sprintf("| %s | %d | %d | %d | %d | %d |", res$site, res$rows, res$unmatched,
                       res$emails, res$traps_set_nights, res$traps_set_differ)))
}
if (length(vocab)) {
  v <- do.call(rbind, vocab)
  agg <- stats::aggregate(list(n = v$n), by = list(field = v$field, value = v$value), FUN = sum)
  ns  <- stats::aggregate(list(sites = v$site), by = list(field = v$field, value = v$value),
                          FUN = function(x) length(unique(x)))
  agg <- merge(agg, ns, by = c("field", "value"))
  agg <- agg[order(agg$field, agg$value, method = "radix"), ]
  cat("\nSuite-wide vocabulary (trap-night rows):\n"); print(agg, row.names = FALSE)
  gh_summary(c("### Per-plot-night vocabulary (trap-night rows, all sites)",
               "| field | value | rows | sites |", "|---|---|---:|---:|",
               sprintf("| %s | %s | %d | %d |", agg$field, agg$value, agg$n, agg$sites)))
}
n_ok <- length(list.files(out_dir, pattern = "\\.rds$"))
cat(sprintf("Bundle now has %d/%d sites.\n", n_ok, length(sites)))

# Exact site-set gate. A partial bundle is not a degraded success: downstream
# comparisons and denominators assume the canonical network opportunity set.
# Staging means this can fail without touching the committed known-good bundle.
built_sites <- sort(sub("\\.rds$", "", list.files(out_dir, pattern = "\\.rds$")))
expected_sites <- sort(unique(as.character(sites)))
missing_sites <- setdiff(expected_sites, built_sites)
extra_sites <- setdiff(built_sites, expected_sites)
if (!identical(built_sites, expected_sites))
  stop(sprintf(
    "Exact site-set gate failed: built %d/%d; missing=[%s]; extra=[%s]. Known-good data/sites was not replaced.",
    length(built_sites), length(expected_sites), paste(missing_sites, collapse = ","),
    paste(extra_sites, collapse = ",")), call. = FALSE)
cat(sprintf("EXACT SITE SET OK: %d/%d canonical mammal sites built.\n",
            length(built_sites), length(expected_sites)))

# ---- freshness marker, folded into data/bundle_meta.json ---------------------
# A re-pull that succeeds but brings back no newer records is a silent stall.
# Compare against the committed bundle_meta.json; stay non-fatal (the PR
# reviewer is the gate).
prev <- read_bundle_meta(meta_path)
prev_through <- if (!is.null(prev$data_through)) as.character(prev$data_through) else NA_character_
rng <- collect_date_range(out_dir)
meta <- bundle_meta(.citation$release, .citation$doi, .citation$release_generated,
                    rng[["from"]], rng[["through"]], length(built_sites), prev_through)
write_json_stable(meta, meta_path)
cat(sprintf("Wrote %s\n", meta_path))

fresh <- if (is.na(rng[["through"]])) {
  gh_annotation("warning", "Freshness did not advance", "Could not read any collectDate from the rebuilt bundle.")
  "Freshness: could not read any collectDate from the rebuilt bundle."
} else if (is.na(prev_through)) {
  gh_annotation("notice", "First stamped run", sprintf("data_through %s; no committed bundle_meta.json to compare against.", rng[["through"]]))
  sprintf("Freshness: first stamped run, data_through %s (%s).", rng[["through"]], NEON_RELEASE_PIN)
} else if (isTRUE(meta$advanced)) {
  sprintf("Freshness: data_through advanced %s to %s (%s).", prev_through, rng[["through"]], NEON_RELEASE_PIN)
} else {
  gh_annotation("warning", "Freshness did not advance",
                sprintf("data_through is still %s (previous %s) under %s. Expected while the release pin is unchanged; otherwise check the pull.",
                        rng[["through"]], prev_through, NEON_RELEASE_PIN))
  sprintf("Freshness: data_through did not advance (still %s, %s).", rng[["through"]], NEON_RELEASE_PIN)
}
cat(fresh, "\n")
gh_summary(paste("-", fresh))
if (nzchar(report_dir)) {
  dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)
  writeLines(fresh, file.path(report_dir, "freshness.txt"))
}
