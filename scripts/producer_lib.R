# ===========================================================================
# producer_lib.R: pure, testable pieces of the small-mammal bundle producer.
#
# Sourced by scripts/refresh_data.R (the NEON pull), scripts/test_producer.R
# (fixture tests, no network) and scripts/restore_unchanged_bundles.R. Nothing
# here touches the network or the file system except the explicit writers.
# ===========================================================================

NEON_PRODUCT       <- "DP1.10072.001"
NEON_RELEASE_PIN   <- "RELEASE-2026"   # bump deliberately; release_watch opens an issue when NEON ships a newer one
BUNDLE_SCHEMA      <- 2L                # 2 = per-plot-night join + remark scrub + total row order
NEON_PRODUCTS_URL  <- paste0("https://data.neonscience.org/api/v0/products/", NEON_PRODUCT)

# Per-plot-night fields carried onto every trap-night row by nightuid.
PERPLOTNIGHT_FIELDS <- c("gridCompletion", "samplingImpractical",
                         "mammalGridSamplingType", "eventID", "trapsSet")

BUNDLE_KEEP <- c("tagID","individualCode","taxonID","scientificName","taxonRank",
  "identificationQualifier","nativeStatusCode","plotID","trapCoordinate",
  "decimalLatitude","decimalLongitude","elevation","nlcdClass","namedLocation",
  "collectDate","nightuid","trapStatus","recapture","fate","hindfootLength",
  "earLength","tailLength","totalLength","weight","lifeStage","sex","testes",
  "nipples","pregnancyStatus","vagina","domainID","siteID","remarks",
  PERPLOTNIGHT_FIELDS)

BUNDLE_SORT_KEYS <- c("siteID", "plotID", "collectDate", "nightuid",
                      "trapCoordinate", "tagID")

EMAIL_RX    <- "[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}"
STAFF_TOKEN <- "[NEON staff]"

# ---- GitHub Actions output helpers (no-ops outside Actions) ----------------
gh_summary <- function(lines) {
  p <- Sys.getenv("GITHUB_STEP_SUMMARY", "")
  if (nzchar(p)) cat(paste0(lines, "\n"), file = p, append = TRUE, sep = "")
  invisible(lines)
}
gh_output <- function(name, value) {
  p <- Sys.getenv("GITHUB_OUTPUT", "")
  if (nzchar(p)) cat(sprintf("%s=%s\n", name, value), file = p, append = TRUE)
  invisible(value)
}
gh_annotation <- function(kind, title, msg) {
  msg <- gsub("[\r\n]+", " ", msg)
  cat(sprintf("::%s title=%s::%s\n", kind, title, msg))
}

# ---- column materialization and total order ---------------------------------
# neonUtilities returns arrow-backed ALTREP strings; saving them carries an
# ALTVEC that reads back empty where arrow is absent. Copy into base vectors.
materialize_columns <- function(d) {
  for (nm in names(d)) {
    col <- d[[nm]]
    d[[nm]] <- if (inherits(col, "Date")) structure(as.numeric(col[seq_along(col)]), class = "Date")
               else if (is.factor(col)) factor(as.character(col[seq_along(col)]))
               else col[seq_along(col)]
  }
  d
}

# Total row order: the named keys first, then every remaining column in the
# order given, radix (locale independent), NA last. Row names reset.
sort_rows <- function(d, keys, rest = setdiff(names(d), keys)) {
  cols <- c(intersect(keys, names(d)), intersect(rest, names(d)))
  if (!nrow(d) || !length(cols)) return(d)
  o <- do.call(order, c(unname(lapply(cols, function(cn) {
    x <- d[[cn]]; if (is.factor(x)) as.character(x) else unclass(x) })),
    list(method = "radix", na.last = TRUE)))
  d <- d[o, , drop = FALSE]
  attr(d, "row.names") <- .set_row_names(nrow(d))
  d
}

# ---- per-plot-night join ----------------------------------------------------
# One row per nightuid from mam_perplotnight, left-joined onto trap nights.
# Exact duplicate rows are collapsed; conflicting duplicates stop the build.
join_perplotnight <- function(trapnight, perplotnight) {
  if (is.null(perplotnight) || !nrow(perplotnight))
    stop("mam_perplotnight is missing or empty; cannot carry gridCompletion through.", call. = FALSE)
  if (!"nightuid" %in% names(perplotnight))
    stop("mam_perplotnight has no nightuid column.", call. = FALSE)
  carried <- intersect(PERPLOTNIGHT_FIELDS, names(perplotnight))
  overlap <- intersect(carried, names(trapnight))
  carried <- setdiff(carried, overlap)          # trap-night copy wins if NEON ever ships both
  ppn <- as.data.frame(perplotnight)[, c("nightuid", carried), drop = FALSE]
  ppn <- materialize_columns(ppn)
  ppn <- ppn[!is.na(ppn$nightuid), , drop = FALSE]
  ppn <- unique(ppn)
  dup <- unique(ppn$nightuid[duplicated(ppn$nightuid)])
  if (length(dup))
    stop(sprintf("mam_perplotnight has %d nightuid(s) with conflicting rows: %s",
                 length(dup), paste(utils::head(sort(dup), 10), collapse = ", ")), call. = FALSE)
  idx <- match(trapnight$nightuid, ppn$nightuid)
  out <- trapnight
  for (cn in carried) out[[cn]] <- ppn[[cn]][idx]
  list(data = out,
       report = list(rows = nrow(trapnight), joined = sum(!is.na(idx)),
                     unmatched = sum(is.na(idx)), carried = carried,
                     overlap = overlap,
                     absent = setdiff(PERPLOTNIGHT_FIELDS, c(carried, overlap))))
}

# Share of plot-nights where NEON's trapsSet differs from the distinct trap
# coordinates with a set trap (status codes 2 to 6) that night.
traps_set_mismatch <- function(d) {
  if (!all(c("trapsSet", "nightuid", "trapCoordinate", "trapStatus") %in% names(d)))
    return(list(nights = 0L, mismatched = 0L, share = NA_real_))
  set_trap <- grepl("^[2-6] - ", as.character(d$trapStatus)) & !is.na(d$nightuid)
  ours <- tapply(d$trapCoordinate[set_trap], d$nightuid[set_trap],
                 function(x) length(unique(x[!is.na(x)])))
  neon <- tapply(suppressWarnings(as.numeric(d$trapsSet)), d$nightuid,
                 function(x) x[!is.na(x)][1])
  neon <- neon[!is.na(neon)]
  if (!length(neon)) return(list(nights = 0L, mismatched = 0L, share = NA_real_))
  o <- ours[names(neon)]; o[is.na(o)] <- 0
  mism <- sum(o != neon)
  list(nights = length(neon), mismatched = as.integer(mism), share = mism / length(neon))
}

# ---- remark scrub -----------------------------------------------------------
scrub_remarks <- function(d) {
  if (!"remarks" %in% names(d)) return(list(data = d, matches = 0L, rows = 0L))
  r <- as.character(d$remarks)
  hit <- !is.na(r) & grepl(EMAIL_RX, r, perl = TRUE)
  m <- if (any(hit)) sum(vapply(gregexpr(EMAIL_RX, r[hit], perl = TRUE), length, integer(1))) else 0L
  r[hit] <- gsub(EMAIL_RX, STAFF_TOKEN, r[hit], perl = TRUE)
  d$remarks <- r
  list(data = d, matches = as.integer(m), rows = sum(hit))
}

# ---- the whole per-site transform ------------------------------------------
prepare_site_bundle <- function(trapnight, perplotnight, keep = BUNDLE_KEEP) {
  j <- join_perplotnight(as.data.frame(trapnight), perplotnight)
  tsm <- traps_set_mismatch(j$data)
  d <- j$data[, intersect(keep, names(j$data)), drop = FALSE]
  s <- scrub_remarks(d)
  d <- materialize_columns(tibble::as_tibble(s$data))
  d <- sort_rows(d, BUNDLE_SORT_KEYS, keep)
  d <- tibble::as_tibble(d)
  tab <- function(cn) if (cn %in% names(d)) table(d[[cn]], useNA = "ifany") else NULL
  list(bundle = d,
       report = c(j$report, list(email_matches = s$matches, email_rows = s$rows,
                                 traps_set = tsm,
                                 gridCompletion = tab("gridCompletion"),
                                 samplingImpractical = tab("samplingImpractical"))))
}

# ---- release confirmation ---------------------------------------------------
check_release <- function(raw, pin = NEON_RELEASE_PIN, site = "?") {
  cites <- grep("^citation_10072_", names(raw), value = TRUE)
  want  <- paste0("citation_10072_", pin)
  if (!identical(cites, want))
    stop(sprintf("%s: expected exactly one citation element %s, got [%s].",
                 site, want, paste(cites, collapse = ", ")), call. = FALSE)
  for (tb in c("mam_pertrapnight", "mam_perplotnight")) {
    x <- raw[[tb]]
    if (is.null(x)) next
    if (!"release" %in% names(x))
      stop(sprintf("%s: %s has no release column; cannot confirm %s.", site, tb, pin), call. = FALSE)
    rel <- unique(as.character(x$release))
    if (!identical(rel, pin))
      stop(sprintf("%s: %s carries release(s) [%s], expected only %s.",
                   site, tb, paste(rel, collapse = ", "), pin), call. = FALSE)
  }
  invisible(TRUE)
}

# ---- deterministic JSON -----------------------------------------------------
write_json_stable <- function(x, path) {
  txt <- jsonlite::toJSON(x, auto_unbox = TRUE, pretty = TRUE, null = "null",
                          na = "null", digits = NA)
  tmp <- paste0(path, ".tmp")
  writeLines(enc2utf8(as.character(txt)), tmp, useBytes = TRUE)
  file.rename(tmp, path)
  invisible(path)
}

bundle_meta <- function(release, doi, release_generated, data_from, data_through,
                        sites, previous_data_through) {
  prev <- if (length(previous_data_through) && !is.na(previous_data_through)) previous_data_through else NULL
  advanced <- !is.null(prev) && !is.na(data_through) && data_through > prev
  list(product = NEON_PRODUCT, release = release, doi = doi,
       release_generated = release_generated, bundle_schema = BUNDLE_SCHEMA,
       data_from = if (is.na(data_from)) NULL else data_from,
       data_through = if (is.na(data_through)) NULL else data_through,
       sites = as.integer(sites), previous_data_through = prev,
       advanced = isTRUE(advanced))
}

read_bundle_meta <- function(path) {
  if (!file.exists(path)) return(NULL)
  tryCatch(jsonlite::fromJSON(path, simplifyVector = TRUE), error = function(e) NULL)
}

# min/max collectDate (YYYY-MM-DD) across every bundle in a directory.
collect_date_range <- function(dir) {
  lo <- NA_character_; hi <- NA_character_
  for (f in list.files(dir, pattern = "\\.rds$", full.names = TRUE)) {
    d <- tryCatch(readRDS(f), error = function(e) NULL)
    if (is.null(d) || !"collectDate" %in% names(d)) next
    cd <- substr(as.character(d$collectDate), 1, 10)
    cd <- cd[!is.na(cd) & nzchar(cd)]
    if (!length(cd)) next
    if (is.na(lo) || min(cd) < lo) lo <- min(cd)
    if (is.na(hi) || max(cd) > hi) hi <- max(cd)
  }
  c(from = lo, through = hi)
}

# ---- canonical compare (restore_unchanged_bundles.R) -------------------------
canonicalize <- function(x) {
  if (is.data.frame(x)) {
    x <- materialize_columns(x)
    return(sort_rows(x, character(), names(x)))
  }
  if (is.list(x)) { a <- attributes(x); y <- lapply(x, canonicalize); attributes(y) <- a; return(y) }
  if (is.atomic(x) && length(x)) { a <- attributes(x); y <- x[seq_along(x)]; attributes(y) <- a; return(y) }
  x
}
equivalent_objects <- function(a, b) identical(canonicalize(a), canonicalize(b))
