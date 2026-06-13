# ===========================================================================
# build_site_index.R — precompute the national site-picker map's data.
#
# Reads every bundled site (data/sites/<SITE>.rds), computes the headline
# numbers the picker map needs (captures, individuals, species richness, the
# most-caught species + its ecological group/emoji, year span), and writes one
# tidy table to data/site_index.rds. The app loads THAT at startup — instant,
# no per-bundle scan on boot.
#
# Re-run after refresh_data.R changes the bundles:
#   "C:\Program Files\R\R-4.3.1\bin\Rscript.exe" scripts/build_site_index.R
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

cat(sprintf("Indexing %d bundled sites...\n", length(files)))

rows <- lapply(files, function(f) {
  code <- sub("\\.rds$", "", basename(f))
  d <- tryCatch(tibble::as_tibble(readRDS(f)), error = function(e) NULL)
  if (is.null(d) || !"tagID" %in% names(d)) return(NULL)

  caps <- d[!is.na(d$tagID), , drop = FALSE]
  n_cap <- nrow(caps)
  if (n_cap == 0) return(NULL)

  sp_tab   <- sort(table(caps$scientificName[!is.na(caps$scientificName)]), decreasing = TRUE)
  top_sp   <- if (length(sp_tab)) names(sp_tab)[1] else NA_character_
  grp      <- genus_group(top_sp)
  yrs      <- suppressWarnings(as.integer(format(
                as.Date(substr(as.character(caps$collectDate), 1, 10)), "%Y")))
  yrs      <- yrs[is.finite(yrs)]

  meta <- neon_sites[neon_sites$site == code, ]
  tibble::tibble(
    site        = code,
    name        = if (nrow(meta)) meta$name[1]   else code,
    state       = if (nrow(meta)) meta$state[1]  else NA_character_,
    domain      = if (nrow(meta)) meta$domain[1] else NA_character_,
    lat         = if (nrow(meta)) meta$lat[1]    else NA_real_,
    lng         = if (nrow(meta)) meta$lng[1]    else NA_real_,
    bio         = if (nrow(meta)) meta$bio[1]    else NA_character_,
    captures    = n_cap,
    individuals = length(unique(caps$tagID)),
    species     = length(unique(caps$scientificName[!is.na(caps$scientificName)])),
    top_species = top_sp %||% NA_character_,
    top_caps    = if (length(sp_tab)) as.integer(sp_tab[1]) else NA_integer_,
    nickname    = species_nickname(top_sp) %||% NA_character_,
    emoji       = genus_emoji(top_sp),
    group_key   = grp$key,
    group_label = grp$label,
    group_color = grp$color,
    year_min    = if (length(yrs)) min(yrs) else NA_integer_,
    year_max    = if (length(yrs)) max(yrs) else NA_integer_
  )
})

idx <- dplyr::bind_rows(rows)
idx <- idx[order(-idx$captures), ]
saveRDS(idx, "data/site_index.rds", compress = "xz")

cat(sprintf("Wrote data/site_index.rds: %d sites, %s captures total, %d species groups.\n",
            nrow(idx), format(sum(idx$captures), big.mark = ","),
            length(unique(idx$group_key))))
print(idx[, c("site","captures","species","top_species","group_label")], n = nrow(idx))
