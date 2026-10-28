# ===========================================================================
# neon_product_meta.R: NEON products-endpoint metadata for DP1.10072.001.
#
# Anonymous GET of https://data.neonscience.org/api/v0/products/DP1.10072.001.
#   * writes <out>/neon_issue_log.json (changeLogs sorted by id, stable bytes)
#   * diffs it against the committed copy and warns once per new entry
#   * release watch: warns and sets step output newer_release=<tag> when NEON
#     lists a release newer than the pin in data/bundle_meta.json
# If the endpoint is down the committed files stay as they are and the step
# passes with a warning.
#
# Functions are pure and sourced by refresh_data.R and test_producer.R; the
# main block only runs under `Rscript scripts/neon_product_meta.R`.
#   SMT_META_OUT_DIR       output directory (default data)
#   SMT_ISSUE_LOG_BASELINE committed copy to diff against (default data/neon_issue_log.json)
# ===========================================================================

if (!exists("NEON_PRODUCTS_URL")) {
  .here <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) NULL)
  if (is.null(.here)) {
    a <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
    .here <- if (length(a)) dirname(a) else "scripts"
  }
  source(file.path(.here, "producer_lib.R"))
}

fetch_products <- function(url = NEON_PRODUCTS_URL, tries = 4L, timeout = 60) {
  last <- NULL
  for (i in seq_len(tries)) {
    res <- tryCatch({
      h <- curl::new_handle(timeout = timeout, connecttimeout = 20, useragent = "neon-small-mammal-tracker")
      r <- curl::curl_fetch_memory(url, handle = h)
      if (r$status_code != 200) stop(sprintf("HTTP %d", r$status_code))
      jsonlite::fromJSON(rawToChar(r$content), simplifyVector = FALSE)
    }, error = function(e) { last <<- conditionMessage(e); NULL })
    if (!is.null(res) && !is.null(res$data)) return(res$data)
    if (i < tries) Sys.sleep(2^i)
  }
  stop(sprintf("NEON products endpoint unreachable after %d tries (%s): %s", tries, url, last), call. = FALSE)
}

.ymd <- function(x) if (is.null(x) || !length(x) || is.na(x) || !nzchar(x)) NULL else substr(as.character(x), 1, 10)
.str <- function(x) if (is.null(x) || !length(x) || is.na(x)) NULL else as.character(x)

issue_log_entries <- function(prod) {
  cl <- prod$changeLogs
  if (!length(cl)) return(list())
  e <- lapply(cl, function(z) list(
    id = as.integer(z$id), issueDate = .ymd(z$issueDate), resolvedDate = .ymd(z$resolvedDate),
    dateRangeStart = .ymd(z$dateRangeStart), dateRangeEnd = .ymd(z$dateRangeEnd),
    locationAffected = .str(z$locationAffected), issue = .str(z$issue),
    resolution = .str(z$resolution)))
  e[order(vapply(e, `[[`, integer(1), "id"))]
}

write_issue_log <- function(entries, path) {
  # an empty log is written as [] (toJSON of list() gives [])
  write_json_stable(entries, path)
}

read_issue_log <- function(path) {
  if (!file.exists(path)) return(NULL)
  tryCatch(jsonlite::fromJSON(path, simplifyVector = FALSE), error = function(e) NULL)
}

# New and changed entries of `new` relative to `old` (lists from issue_log_entries).
diff_issue_log <- function(old, new) {
  key <- function(z) as.character(z$id)
  om <- stats::setNames(old, vapply(old, key, ""))
  added <- list(); changed <- list()
  for (z in new) {
    k <- key(z)
    if (!k %in% names(om)) added[[length(added) + 1]] <- z
    else if (!identical(jsonlite::toJSON(om[[k]], auto_unbox = TRUE, null = "null"),
                        jsonlite::toJSON(z, auto_unbox = TRUE, null = "null")))
      changed[[length(changed) + 1]] <- z
  }
  list(added = added, changed = changed)
}

.release_year <- function(tag) suppressWarnings(as.integer(sub("^RELEASE-(\\d{4}).*$", "\\1", tag)))

release_info <- function(prod, pin = NEON_RELEASE_PIN) {
  for (r in prod$releases) if (identical(r$release, pin))
    return(list(release = pin, doi = .str(r$productDoi$url),
                release_generated = .ymd(r$generationDate)))
  stop(sprintf("NEON products endpoint does not list %s for %s.", pin, NEON_PRODUCT), call. = FALSE)
}

newer_releases <- function(prod, pin = NEON_RELEASE_PIN) {
  tags <- vapply(prod$releases, function(r) as.character(r$release), "")
  y <- .release_year(tags); py <- .release_year(pin)
  sort(tags[!is.na(y) & !is.na(py) & y > py])
}

run_product_meta <- function(out_dir = Sys.getenv("SMT_META_OUT_DIR", "data"),
                             baseline = Sys.getenv("SMT_ISSUE_LOG_BASELINE", "data/neon_issue_log.json"),
                             meta_path = "data/bundle_meta.json") {
  prod <- tryCatch(fetch_products(), error = function(e) {
    gh_annotation("warning", "NEON products endpoint down",
                  paste(conditionMessage(e), "Kept the committed issue log."))
    gh_summary(sprintf("- NEON products endpoint unreachable; kept the committed issue log (%s).", conditionMessage(e)))
    NULL })
  if (is.null(prod)) return(invisible(FALSE))
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  old <- read_issue_log(baseline)
  new <- issue_log_entries(prod)
  write_issue_log(new, file.path(out_dir, "neon_issue_log.json"))
  cat(sprintf("Issue log: %d entries written to %s\n", length(new), file.path(out_dir, "neon_issue_log.json")))
  gh_summary(c("### NEON issue log (DP1.10072.001)", sprintf("- %d entries", length(new))))
  if (is.null(old)) {
    gh_summary("- First issue-log snapshot; no committed copy to diff against.")
    gh_annotation("notice", "NEON issue log", sprintf("First snapshot: %d entries.", length(new)))
  } else {
    dd <- diff_issue_log(old, new)
    line <- function(z) sprintf("%s %s %s", z$id, z$locationAffected %||% "", substr(z$issue %||% "", 1, 80))
    for (z in dd$added) gh_annotation("warning", "NEON issue log", line(z))
    if (length(dd$added)) gh_summary(c("- New entries:", paste0("  - ", vapply(dd$added, line, ""))))
    if (length(dd$changed)) gh_summary(c("- Changed entries:", paste0("  - ", vapply(dd$changed, line, ""))))
    if (!length(dd$added) && !length(dd$changed)) gh_summary("- No new or changed entries.")
  }

  m <- read_bundle_meta(meta_path)
  pin <- if (!is.null(m$release) && nzchar(m$release)) m$release else NEON_RELEASE_PIN
  nr <- newer_releases(prod, pin)
  if (length(nr)) {
    newest <- nr[length(nr)]
    gh_annotation("warning", "Newer NEON release",
                  sprintf("NEON lists %s for %s; bundles are pinned to %s. Bump NEON_RELEASE_PIN in scripts/producer_lib.R.",
                          newest, NEON_PRODUCT, pin))
    gh_summary(sprintf("- Newer NEON release available: %s (pinned %s).", newest, pin))
    gh_output("newer_release", newest)
  } else {
    gh_summary(sprintf("- Release watch: %s is the newest listed release.", pin))
  }
  invisible(TRUE)
}

`%||%` <- function(a, b) if (is.null(a)) b else a

if (sys.nframe() == 0L) run_product_meta()
