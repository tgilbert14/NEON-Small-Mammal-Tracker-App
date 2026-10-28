#!/usr/bin/env Rscript
# Before/after measurements for the 2026-10-22 science-finish helper changes.
# "Before" = R/helpers.R and R/seasonal_env.R at main c1a229d (read with git
# show); "after" = the working tree. Run from the repo root:
#   Rscript --vanilla scripts/measure_science_finish.R
# Output is plain text; the numbers are recorded in
# docs/audit-fixes/2026-10-22-science-finish.md.
suppressPackageStartupMessages(library(dplyr))
BASE <- "c1a229d"
env_layers_expr <- Filter(function(e) is.call(e) && identical(e[[1]], as.name("<-")) &&
                            identical(e[[2]], as.name("ENV_LAYERS")), as.list(parse("global.R")))[[1]]
load_env <- function(helpers, seasonal) {
  e <- new.env(parent = globalenv())
  eval(env_layers_expr, e)
  sys.source(helpers, e); sys.source(seasonal, e)
  e
}
tmp <- tempfile(); dir.create(tmp)
system2("git", c("show", sprintf("%s:R/helpers.R", BASE)), stdout = file.path(tmp, "h.R"))
system2("git", c("show", sprintf("%s:R/seasonal_env.R", BASE)), stdout = file.path(tmp, "s.R"))
B <- load_env(file.path(tmp, "h.R"), file.path(tmp, "s.R"))
A <- load_env("R/helpers.R", "R/seasonal_env.R")
site <- function(s) A$clean_mam(readRDS(file.path("data/sites", paste0(s, ".rds"))))
envb <- function(s) { f <- file.path("data/env", paste0(s, ".rds")); if (file.exists(f)) readRDS(f) else NULL }
hdr <- function(x) cat("\n==== ", x, " ====\n", sep = "")

cache <- list(); get_site <- function(s) { if (is.null(cache[[s]])) cache[[s]] <<- site(s); cache[[s]] }
lbs <- list()
get_lb <- function(s, which) { k <- paste(s, which); if (is.null(lbs[[k]]))
  lbs[[k]] <<- (if (which == "B") B else A)$build_leaderboard(get_site(s)); lbs[[k]] }

# per-tag flags under one helper version, histories split once
flags_all <- function(E, s) {
  d <- get_site(s); lb <- get_lb(s, if (identical(E, B)) "B" else "A")
  keep <- A$mam_tag_marked(d$tagID) & !is.na(d$date)
  base <- as.data.frame(d[keep, intersect(c("tagID", "date", A$.HIST_COLS), names(d))])
  pcs <- split(base, base$tagID)
  li <- match(names(pcs), lb$tagID)
  out <- lapply(seq_along(pcs), function(k) {
    fl <- E$individual_qc_flags(A$.history_rows(pcs[[k]]), lb[li[k], , drop = FALSE])
    if (!length(fl)) return(NULL)
    data.frame(tagID = names(pcs)[k], level = vapply(fl, `[[`, "", "level"),
               text = vapply(fl, `[[`, "", "text"), stringsAsFactors = FALSE)
  })
  list(flags = do.call(rbind, out), n2 = sum(vapply(pcs, nrow, 1L) >= 2))
}
kind <- function(t) ifelse(grepl("^Life stage", t), "stage",
                    ifelse(grepl("^Weight changed", t), "weight",
                    ifelse(grepl("^This tag was recorded", t), "species", "other")))

hdr("1. first_stage and age prefix")
for (s in c("SRER", "HARV")) {
  b <- get_lb(s, "B"); a <- get_lb(s, "A"); a <- a[match(b$tagID, a$tagID), ]
  cat(sprintf("%s: animals %d; first_stage changed %d; age prefix (>= vs ~) changed %d; approx_age changed %d\n",
      s, nrow(b), sum(!(b$first_stage %in% a$first_stage & is.na(b$first_stage) == is.na(a$first_stage))),
      sum(b$age_is_minimum != a$age_is_minimum), sum(b$approx_age_years != a$approx_age_years)))
  cat("  before first_stage: "); print(table(b$first_stage, useNA = "ifany"))
  cat("  after  first_stage: "); print(table(a$first_stage, useNA = "ifany"))
}

hdr("2/3/6. stage regression, weight jump, multi-species flags")
for (s in c("SRER", "HARV", "ONAQ")) {
  fb <- flags_all(B, s); fa <- flags_all(A, s)
  tb <- table(kind(fb$flags$text), fb$flags$level); ta <- table(kind(fa$flags$text), fa$flags$level)
  cat(sprintf("%s (animals caught 2+ times: %d)\n  before:\n", s, fb$n2)); print(tb)
  cat("  after:\n"); print(ta)
}

hdr("4. richness with and without cf. species")
for (s in c("SRER", "MOAB", "TOOL", "HARV")) {
  d <- get_site(s)
  cb <- B$community_stats(d, get_lb(s, "B")); ca <- A$community_stats(d, get_lb(s, "A"))
  hb <- B$hill_numbers(d); ha <- A$hill_numbers(d)
  cat(sprintf("%s: species before %d after %d confirmed %d | Hill q0 %s -> %s, q1 %s -> %s\n",
      s, cb$species, ca$species, ca$species_confirmed, hb$q0, ha$q0, hb$q1, ha$q1))
}
moved <- character(0)
for (f in list.files("data/sites", pattern = "rds$")) {
  s <- sub("\\.rds$", "", f); x <- readRDS(file.path("data/sites", f))
  q <- tolower(trimws(x$identificationQualifier))
  if (!any(q %in% A$MAM_ID_QUALIFIER_NOT_SPECIES)) next
  h <- x[!is.na(x$tagID) & !is.na(x$scientificName), ]
  nb <- dplyr::n_distinct(B$species_level_only(h)$scientificName)
  na <- dplyr::n_distinct(A$species_level_only(h)$scientificName)
  moved <- c(moved, sprintf("%s rows dropped %d, richness %d -> %d", s,
      nrow(B$species_level_only(h)) - nrow(A$species_level_only(h)), nb, na))
}
cat("sites with cf./aff. genus or family rows:\n"); cat(paste0("  ", moved, "\n"), sep = "")

hdr("5/16. species accumulation band and Chao1 interval")
for (s in c("SRER", "MOAB", "BARR")) {
  d <- get_site(s); sb <- B$species_accum(d); sa <- A$species_accum(d)
  cat(sprintf("%s: sobs %d->%d f1 %d f2 %d chao1 %d->%d CI %d-%d -> %d-%d unstable %s | band mid-curve sd: %.2f -> %.2f\n",
      s, sb$sobs, sa$sobs, sa$f1, sa$f2, sb$chao1, sa$chao1, sb$chao_lo, sb$chao_hi,
      sa$chao_lo, sa$chao_hi, sa$unstable,
      { k <- nrow(sb$curve); i <- ceiling(k / 4); (sb$curve$hi[i] - sb$curve$richness[i]) },
      { k <- nrow(sa$curve); i <- ceiling(k / 4); (sa$curve$hi[i] - sa$curve$richness[i]) }))
}

hdr("7. site_qc_report")
for (s in c("SRER", "HARV")) {
  d <- get_site(s)
  tm <- system.time(r <- A$site_qc_report(d, get_lb(s, "A")))[["elapsed"]]
  cat(sprintf("%s: %d rows in %.1f s; tags flagged %d\n", s, nrow(r), tm, dplyr::n_distinct(r$tagID)))
  print(table(r$flag, r$level))
}

hdr("8. env_corr_pvalue observed statistic")
for (s in c("SRER", "HARV", "JORN")) {
  d <- get_site(s); ev <- envb(s)
  pb <- B$env_corr_pvalue(d, ev); pa <- A$env_corr_pvalue(d, ev)
  cb <- B$env_corr_all(d, ev)
  cat(sprintf("%s: before p %s (obs rounded |r| %s) | after p %s (obs |r| %.4f, %s lag %s) n_search %s n_months %s\n",
      s, pb$p, if (is.null(cb)) NA else max(abs(cb$r)), pa$p, abs(pa$r_obs), pa$layer, pa$lag,
      pa$n_search, pa$n_months))
}

hdr("9. annual response for the seasonal card")
for (s in c("SRER", "HARV")) {
  d <- get_site(s)
  ac <- d %>% filter(!is.na(.data$ym)) %>% mutate(yr = as.integer(substr(.data$ym, 1, 4))) %>%
    group_by(.data$yr) %>% summarise(cap = sum(.data$is_capture), tn = sum(.data$trap_effort, na.rm = TRUE), .groups = "drop")
  ac <- ac[ac$tn > 0, ]
  ai <- A$annual_cpue_index(d)
  tab <- merge(data.frame(year = ac$yr, pooled_cpue = round(100 * ac$cap / ac$tn, 2)),
               transform(ai, index = round(index, 2)), by = "year", all = TRUE)
  cat(s, "\n"); print(tab[, c("year", "pooled_cpue", "index", "n_months", "trap_nights", "ok")], row.names = FALSE)
  ev <- envb(s); bio <- A$seasonal_biome(s)
  lb_ <- A$seasonal_driver_links(ev, data.frame(year = ac$yr, value = 100 * ac$cap / ac$tn), biome = bio)
  la_ <- A$seasonal_driver_links(ev, A$annual_cpue_index(d, ok_only = TRUE), biome = bio)
  cat("  links before (pooled):\n"); print(lb_[, c("driver", "lag", "r", "n", "p", "p_adj")], row.names = FALSE)
  cat("  links after (index, ok years):\n"); print(la_[, c("driver", "lag", "r", "n", "p", "p_adj")], row.names = FALSE)
}

hdr("11/12. MNKA edges and clamped bouts")
for (s in c("SRER", "HARV")) {
  d <- get_site(s); mn <- A$mnka_series(d); cc <- A$closed_capture_series(d)
  cat(sprintf("%s: plot-months %d, edge %d; estimable bouts %d, clamped %d; gap-break rows (gap>2 mo) %d\n",
      s, nrow(mn), sum(mn$edge), cc$n_estimable, cc$n_clamped,
      nrow(A$insert_gap_breaks(mn)) - nrow(mn)))
}
