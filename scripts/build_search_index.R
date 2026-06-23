# ===========================================================================
# build_search_index.R — precompute the "Search the network" index.
#
# Reads every COMMITTED site bundle (data/sites/<SITE>.rds) — no live NEON
# fetch — and writes one small tidy table to data/search_index.rds: one row per
# (species x site) with the display name, the site, a within-site MNKA index,
# total captures/individuals, and the year span at that site.
#
# MNKA here is the SAME honest unit the app's Population tab reports: Minimum
# Number Known Alive (Krebs 1966), computed per species by running the app's own
# mnka_series() on the cleaned bundle filtered to that species, then taking the
# PEAK month's site-wide MNKA (the max over months of the summed per-plot MNKA).
# It is a within-site index of how many of that species were known alive at once
# — NOT an absolute population and NOT comparable across sites as a ranking.
#
# The search tab loads THIS at startup (like site_index.rds) and filters it in
# memory — instant, no per-bundle scan on boot.
#
# Re-run after refresh_data.R changes the bundles:
#   "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" scripts/build_search_index.R
# ===========================================================================

suppressMessages({
  library(dplyr)
  library(tibble)
})
source("R/site_metadata.R")
source("R/helpers.R")

SITE_DIR <- "data/sites"
files <- list.files(SITE_DIR, pattern = "\\.rds$", full.names = TRUE)
if (length(files) == 0) stop("No bundles in ", SITE_DIR, " — run scripts/refresh_data.R first.")

cat(sprintf("Indexing %d bundled sites for the search index...\n", length(files)))

# Peak site-wide MNKA for ONE species at a site: run the shared mnka_series() on
# the cleaned bundle filtered to that species, sum per-plot MNKA within each
# month, and take the largest month. NA -> 0 (no estimable month).
species_peak_mnka <- function(clean_d, sci) {
  h <- clean_d[!is.na(clean_d$scientificName) & clean_d$scientificName == sci, , drop = FALSE]
  if (nrow(h) == 0) return(0L)
  ser <- tryCatch(mnka_series(h), error = function(e) NULL)
  if (is.null(ser) || nrow(ser) == 0) return(0L)
  by_month <- ser %>% dplyr::group_by(.data$ym) %>%
    dplyr::summarise(m = sum(.data$mnka, na.rm = TRUE), .groups = "drop")
  v <- suppressWarnings(max(by_month$m, na.rm = TRUE))
  if (!is.finite(v)) 0L else as.integer(round(v))
}

rows <- lapply(files, function(f) {
  code <- sub("\\.rds$", "", basename(f))
  d <- tryCatch(tibble::as_tibble(readRDS(f)), error = function(e) NULL)
  if (is.null(d) || !"tagID" %in% names(d)) return(NULL)

  cl <- tryCatch(clean_mam(d), error = function(e) NULL)
  if (is.null(cl) || nrow(cl) == 0) return(NULL)

  # species-level IDs only (drop genus-only "X sp." / "A/B") — same filter the
  # rest of the app uses, so the search list matches the richness counts.
  caps <- cl[!is.na(cl$tagID) & !is.na(cl$scientificName), , drop = FALSE]
  caps <- species_level_only(caps)
  if (nrow(caps) == 0) return(NULL)

  meta <- neon_sites[neon_sites$site == code, ]

  per_sp <- caps %>%
    dplyr::group_by(.data$scientificName) %>%
    dplyr::summarise(
      captures    = dplyr::n(),
      individuals = dplyr::n_distinct(.data$tagID),
      year_min    = suppressWarnings(min(.data$year, na.rm = TRUE)),
      year_max    = suppressWarnings(max(.data$year, na.rm = TRUE)),
      .groups = "drop")

  per_sp$mnka <- vapply(per_sp$scientificName,
                        function(s) species_peak_mnka(caps, s), integer(1))

  per_sp %>%
    dplyr::mutate(
      site        = code,
      name        = if (nrow(meta)) meta$name[1]   else code,
      state       = if (nrow(meta)) meta$state[1]  else NA_character_,
      domain      = if (nrow(meta)) meta$domain[1] else NA_character_,
      group_label = vapply(.data$scientificName, function(s) genus_group(s)$label, character(1)),
      emoji       = genus_emoji(.data$scientificName),
      nickname    = vapply(.data$scientificName,
                           function(s) species_nickname(s) %||% NA_character_, character(1)),
      year_min    = ifelse(is.finite(.data$year_min), as.integer(.data$year_min), NA_integer_),
      year_max    = ifelse(is.finite(.data$year_max), as.integer(.data$year_max), NA_integer_))
})

idx <- dplyr::bind_rows(rows)
# drop blank / genus-only "sp." rows (belt-and-suspenders; species_level_only
# already filters, but guard against any stray name)
idx <- idx[!is.na(idx$scientificName) & nzchar(idx$scientificName) &
           !grepl("\\bsp\\.?$", idx$scientificName), , drop = FALSE]
idx <- idx[order(idx$scientificName, -idx$mnka, -idx$individuals), ,  drop = FALSE]

# keep a tidy column order
idx <- idx[, c("scientificName", "nickname", "emoji", "group_label",
               "site", "name", "state", "domain",
               "mnka", "captures", "individuals", "year_min", "year_max")]

saveRDS(tibble::as_tibble(idx), "data/search_index.rds", compress = "xz")

n_sp <- length(unique(idx$scientificName))
sz   <- file.info("data/search_index.rds")$size
cat(sprintf("Wrote data/search_index.rds: %d (species x site) rows, %d species, %d sites, %.1f KB.\n",
            nrow(idx), n_sp, length(unique(idx$site)), sz / 1024))

# the 10 most widespread species (recorded at the most sites) — sanity check
top <- idx %>% dplyr::group_by(.data$scientificName) %>%
  dplyr::summarise(sites = dplyr::n(), inds = sum(.data$individuals),
                   peak_mnka = max(.data$mnka), .groups = "drop") %>%
  dplyr::arrange(-sites, -inds) %>% utils::head(10)
print(as.data.frame(top))
