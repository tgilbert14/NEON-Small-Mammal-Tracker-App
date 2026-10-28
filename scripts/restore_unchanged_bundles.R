# ===========================================================================
# restore_unchanged_bundles.R: canonical no-op for rebuilt .rds files.
#
# For every data/**/*.rds whose bytes differ from HEAD, read both copies,
# canonicalize them (materialize ALTREP, sort rows by all columns, reset row
# names) and, when identical(), restore the committed bytes with
# `git checkout HEAD -- <file>`. A row reorder, an xz/serialization-header
# difference or an R-version header change therefore never reaches review.
#
# Also writes a per-site row-delta summary (rows and max collectDate before and
# after) for the notify job:
#   $SMT_ROW_DELTA_DIR (default $RUNNER_TEMP/refresh-report, else tempdir())
#     row_delta.json  row_delta.md
# Run from the repository root: Rscript --vanilla scripts/restore_unchanged_bundles.R
# ===========================================================================

source("scripts/producer_lib.R")

git <- function(...) system2("git", c(...), stdout = TRUE, stderr = TRUE)

head_rds <- function(path) {
  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp))
  st <- system2("git", c("show", shQuote(paste0("HEAD:", path))), stdout = tmp, stderr = FALSE)
  if (!identical(as.integer(st), 0L)) return(NULL)
  tryCatch(readRDS(tmp), error = function(e) NULL)
}

# Returns a data.frame(path, status) with status in unchanged/restored/changed/new/deleted.
restore_unchanged <- function(root = "data") {
  tracked <- git("ls-files", "--", shQuote(root))
  tracked <- tracked[grepl("\\.rds$", tracked)]
  present <- list.files(root, pattern = "\\.rds$", recursive = TRUE, full.names = TRUE)
  present <- sub("^\\./", "", present)
  all <- sort(unique(c(tracked, present)))
  status <- vapply(all, function(f) {
    if (!f %in% tracked) return("new")
    if (!file.exists(f)) return("deleted")
    if (system2("git", c("diff", "--quiet", "HEAD", "--", shQuote(f))) == 0L) return("unchanged")
    a <- head_rds(f); b <- tryCatch(readRDS(f), error = function(e) NULL)
    if (!is.null(a) && !is.null(b) && equivalent_objects(a, b)) {
      git("checkout", "HEAD", "--", shQuote(f))
      return("restored")
    }
    "changed"
  }, character(1))
  data.frame(path = all, status = unname(status), stringsAsFactors = FALSE)
}

site_delta <- function(path) {
  info <- function(d) if (is.null(d) || !is.data.frame(d)) c(NA, NA) else {
    cd <- substr(as.character(d$collectDate), 1, 10); cd <- cd[!is.na(cd)]
    c(nrow(d), if (length(cd)) max(cd) else NA) }
  b <- info(head_rds(path))
  a <- info(if (file.exists(path)) tryCatch(readRDS(path), error = function(e) NULL) else NULL)
  list(site = sub("\\.rds$", "", basename(path)),
       rows_before = suppressWarnings(as.integer(b[1])), rows_after = suppressWarnings(as.integer(a[1])),
       max_collectDate_before = b[[2]], max_collectDate_after = a[[2]])
}

if (sys.nframe() == 0L) {
  res <- restore_unchanged("data")
  tab <- table(factor(res$status, c("unchanged", "restored", "changed", "new", "deleted")))
  cat(sprintf("restore_unchanged_bundles: %s\n",
              paste(sprintf("%s=%d", names(tab), as.integer(tab)), collapse = ", ")))
  for (i in which(res$status %in% c("changed", "new", "deleted")))
    cat(sprintf("  %s %s\n", res$status[i], res$path[i]))
  gh_summary(sprintf("- .rds canonical compare: %d byte-identical, %d restored (same content), %d changed, %d new, %d deleted.",
                     tab[["unchanged"]], tab[["restored"]], tab[["changed"]], tab[["new"]], tab[["deleted"]]))

  site_paths <- res$path[grepl("^data/sites/[^/]+\\.rds$", res$path)]
  deltas <- lapply(site_paths, function(p) c(site_delta(p), list(status = res$status[res$path == p])))
  out_dir <- Sys.getenv("SMT_ROW_DELTA_DIR", "")
  if (!nzchar(out_dir)) out_dir <- file.path(Sys.getenv("RUNNER_TEMP", tempdir()), "refresh-report")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  write_json_stable(list(counts = as.list(stats::setNames(as.integer(tab), names(tab))),
                         sites = deltas), file.path(out_dir, "row_delta.json"))
  moved <- Filter(function(z) !identical(z$rows_before, z$rows_after) ||
                    !identical(z$max_collectDate_before, z$max_collectDate_after), deltas)
  md <- if (!length(moved)) "Site bundles: rows unchanged, derived files only." else c(
    "| site | rows before | rows after | max collectDate before | after |", "|---|---:|---:|---|---|",
    vapply(moved, function(z) sprintf("| %s | %s | %s | %s | %s |", z$site,
      z$rows_before %||% NA, z$rows_after %||% NA, z$max_collectDate_before %||% NA,
      z$max_collectDate_after %||% NA), ""))
  writeLines(c(sprintf("Canonical compare: %d changed, %d new, %d deleted, %d restored as same content.",
                       tab[["changed"]], tab[["new"]], tab[["deleted"]], tab[["restored"]]), "", md),
             file.path(out_dir, "row_delta.md"))
  cat(sprintf("Wrote %s\n", file.path(out_dir, "row_delta.json")))
}
