# ===========================================================================
# refresh_env_data.R — build the bundled per-site ENVIRONMENTAL overlays
#
# Companion to refresh_data.R. For each NEON site, downloads the co-located
# environmental data products, AGGREGATES each to one value per calendar month,
# and writes a tiny data/env/<SITE>.rds (a few KB) — the "compare with
# environment" layers the app draws behind the population & seasonality charts.
#
# Output schema (one row per site-month), matching global.R ENV_LAYERS:
#   siteID, ym ("YYYY-MM"), date (first of month),
#   precip_mm   (monthly SUM,  DP1.00044.001 weighing-gauge precipitation)
#   temp_c/min/max (monthly MEAN/MIN/MAX, DP1.00002.001 single-aspirated air temp)
#   flowering_pct (monthly STATUS yes-share, "Open flowers",  DP1.10055.001)
#   greenup_pct   (monthly STATUS yes-share, early leaf-out bundle, DP1.10055.001)
#   fruiting_pct  (monthly STATUS yes-share, "Fruits" exact,   DP1.10055.001)
#   <col>_n       (distinct individuals behind each phenology share; <5 -> share NA)
#   source = "neon"
#
# RESUMABLE: skips sites whose .rds already exists. Delete one to re-pull it.
# Run from the project root:   Rscript scripts/refresh_env_data.R
#
# IMPORTANT — verify table/column names once before a full run:
#   neonUtilities::loadByProduct("DP1.00044.001", site="JORN",
#       startdate="2018-07", enddate="2018-09", check.size="F") |> names()
# NEON occasionally renames published tables; the matchers below are deliberately
# pattern-based and defensive so a rename degrades to "layer missing", not a crash.
# Sensor products also return MANY sub-streams (tower heights, soil depths/
# positions) — we keep the shallowest/lowest level so downloads stay small.
# ===========================================================================

options(timeout = 3600)
suppressMessages({
  library(neonUtilities)
  library(dplyr)
  library(tibble)
})

# NEON API token — set env var NEON_TOKEN to raise the anonymous rate limit.
# Free account + token: https://data.neonscience.org  (Profile → API token)
.neon_token <- Sys.getenv("NEON_TOKEN", unset = NA_character_)
if (!is.na(.neon_token) && nchar(.neon_token) > 10) {
  cat("Using NEON API token (higher rate limits).\n")
} else {
  .neon_token <- NA_character_
  cat("No NEON_TOKEN set — anonymous rate limits apply.\n")
}

source("R/site_metadata.R")  # canonical site list

out_dir <- "data/env"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

start_d <- "2013-01"
end_d   <- format(Sys.Date(), "%Y-%m")
sites   <- neon_sites$site
# Optional CLI site subset, for parallel/sharded builds (resumable; skips files
# that already exist):  Rscript scripts/refresh_env_data.R JORN SRER HARV ...
.args <- commandArgs(trailingOnly = TRUE)
if (length(.args)) sites <- intersect(sites, .args)

# Monthly "top-up" mode: SMT_ENV_RECENT_MONTHS=N narrows the pull to the last N
# months and MERGES the result into each existing bundle — so the CI refresh
# keeps env current (new months as the mammal record extends) cheaply, instead
# of re-pulling 13 years. Leave it unset for a full (offline) build.
.recent_n <- suppressWarnings(as.integer(Sys.getenv("SMT_ENV_RECENT_MONTHS", "")))
.recent   <- !is.na(.recent_n) && .recent_n > 0
if (.recent) {
  start_d <- format(seq(as.Date(paste0(end_d, "-01")),
                        by = sprintf("-%d months", .recent_n), length.out = 2)[2], "%Y-%m")
  cat(sprintf("Recent-refresh mode: last %d months (%s -> %s)\n", .recent_n, start_d, end_d))
}

# ---- generic helpers ------------------------------------------------------

# Pull the first table in a loadByProduct() result whose name matches `tbl_rx`.
pick_table <- function(dl, tbl_rx) {
  if (is.null(dl)) return(NULL)
  nm <- grep(tbl_rx, names(dl), value = TRUE)
  if (!length(nm)) return(NULL)
  tibble::as_tibble(dl[[nm[1]]])
}

# First column matching `col_rx`, coerced numeric.
pick_col <- function(tb, col_rx) {
  if (is.null(tb) || !nrow(tb)) return(NULL)
  nm <- grep(col_rx, names(tb), value = TRUE)
  if (!length(nm)) return(NULL)
  suppressWarnings(as.numeric(tb[[nm[1]]]))
}

# A "YYYY-MM" key from whatever date/time column a table carries.
month_key <- function(tb) {
  dc <- grep("endDateTime|startDateTime|^date$|collectDate|^endDate$", names(tb), value = TRUE)
  if (!length(dc)) return(rep(NA_character_, nrow(tb)))
  substr(as.character(tb[[dc[1]]]), 1, 7)
}

# Roll a single value column up to monthly via `fun` (sum/mean/min/max).
monthly <- function(tb, col_rx, fun) {
  if (is.null(tb) || !nrow(tb)) return(NULL)
  v <- pick_col(tb, col_rx); if (is.null(v)) return(NULL)
  ym <- month_key(tb)
  ok <- !is.na(ym) & !is.na(v)
  if (!any(ok)) return(NULL)
  stats::aggregate(list(value = v[ok]), by = list(ym = ym[ok]), FUN = fun)
}

# Monthly STATUS yes-share for a phenophase group (DP1.10055.001 phe_statusintensity),
# built defensibly per the phenology review:
#  - only status yes/no count (uncertain/blank dropped from BOTH num & denom — the
#    old fruit code folded them in as 0, biasing the share down);
#  - grain = individual x month (a high-cadence month doesn't over-weight): an
#    individual counts "yes" if seen in-phenophase at >=1 bout that month;
#  - returns a companion n (distinct individuals); months with n<5 -> share NA.
# Returns a data.frame(ym, share, n) or NULL when the phenophase isn't recorded.
pheno_share <- function(pht, name_rx) {
  if (is.null(pht) || !nrow(pht)) return(NULL)
  if (!all(c("phenophaseName", "phenophaseStatus", "individualID") %in% names(pht))) return(NULL)
  st   <- tolower(trimws(as.character(pht$phenophaseStatus)))
  keep <- grepl(name_rx, pht$phenophaseName) & st %in% c("yes", "no")
  if (!any(keep)) return(NULL)
  d <- tibble::tibble(individualID = pht$individualID[keep],
                      ym  = month_key(pht[keep, , drop = FALSE]),
                      yes = as.integer(st[keep] == "yes"))
  d <- d[!is.na(d$ym), , drop = FALSE]
  if (!nrow(d)) return(NULL)
  im <- d %>% dplyr::group_by(.data$individualID, .data$ym) %>%
    dplyr::summarise(yes = max(.data$yes), .groups = "drop")
  mo <- im %>% dplyr::group_by(.data$ym) %>%
    dplyr::summarise(share = 100 * mean(.data$yes), n = dplyr::n(), .groups = "drop")
  mo$share[mo$n < 5] <- NA_real_
  as.data.frame(mo)
}

safe_load <- function(dpID, site, timeIndex = NULL) {
  # timeIndex (e.g. 30) restricts a sensor product to ONE averaging interval,
  # so we don't download the high-volume 1-min tables we'd only discard.
  args <- list(dpID = dpID, site = site, startdate = start_d, enddate = end_d,
               package = "basic", check.size = "F")
  if (!is.null(timeIndex)) args$timeIndex <- timeIndex
  if (!is.na(.neon_token))  args$token     <- .neon_token
  tryCatch(do.call(loadByProduct, args),
    error = function(e) { cat(sprintf("      ! %s: %s\n", dpID, conditionMessage(e))); NULL })
}

# ---- per-site build -------------------------------------------------------

build_site_env <- function(site) {
  # full monthly skeleton across the whole window
  months <- format(seq(as.Date(paste0(start_d, "-01")),
                       as.Date(paste0(end_d, "-01")), by = "month"), "%Y-%m")
  out <- tibble::tibble(siteID = site, ym = months,
                        date = as.Date(paste0(months, "-01")))
  join1 <- function(out, m, name) {
    if (is.null(m)) { out[[name]] <- NA_real_; return(out) }
    names(m)[names(m) == "value"] <- name
    dplyr::left_join(out, m, by = "ym")
  }

  # 1) precipitation — weighing gauge, DAILY table, monthly SUM (mm)
  pr <- safe_load("DP1.00044.001", site)
  # NEON publishes precip as WEIPRE_* (weighing gauge), PRIPRE_* (primary) or
  # SECPRE_* (secondary tipping bucket); prefer the DAILY table, fall back to
  # 60/30-min. Sum to a monthly total (mm). Some arid sites (e.g. JORN) have no
  # precip deployment at all -> stays NA, which the UI handles gracefully.
  prt <- pick_table(pr, "(WEIPRE|PRIPRE|SECPRE)_daily|wss_daily_precip|.*daily.*[Pp]recip")
  if (is.null(prt)) prt <- pick_table(pr, "(WEIPRE|PRIPRE|SECPRE)_(60|30)min|.*[Pp]recip")
  out <- join1(out, monthly(prt, "[Pp]recipBulk|secPrecipBulk|priPrecipBulk", sum), "precip_mm")

  # 2) air temperature — single aspirated, 30-min ONLY (timeIndex=30 skips the
  #    high-volume 1-min table we'd discard anyway); keep one tower level
  at <- safe_load("DP1.00002.001", site, timeIndex = 30)
  att <- pick_table(at, "SAAT_30min|saat.*30")
  if (!is.null(att) && "verticalPosition" %in% names(att))
    att <- att[att$verticalPosition == min(att$verticalPosition, na.rm = TRUE), ]
  out <- join1(out, monthly(att, "tempSingleMean", mean), "temp_c")
  out <- join1(out, monthly(att, "tempSingleMinimum", min),  "temp_min")
  out <- join1(out, monthly(att, "tempSingleMaximum", max),  "temp_max")

  # (Relative humidity DP1.00098.001 and soil water content DP1.00094.001 are
  #  intentionally NOT built — soil water especially is a very-high-volume 30-min
  #  product. The bundled overlays are precip + temperature + fruiting; ENV_LAYERS
  #  in global.R lists exactly those three.)

  # 3) plant phenology — DP1.10055.001 phe_statusintensity. Three monthly STATUS
  #    yes-share signals (via pheno_share, with the yes/no filter, individual x
  #    month grain, and n<5 -> NA guardrails):
  #      flowering_pct  "Open flowers"        — seed-crop precursor (arid LEAD driver)
  #      greenup_pct    early leaf-out bundle  — precip-pulse proxy / forage (arid LEAD)
  #      fruiting_pct   "Fruits" (exact)      — mast signal (mesic/forest LEAD)
  #    Arid sites (SRER/JORN) have NO Fruits but rich flowers + green-up, so this
  #    gives them a real phenology signal the old fruit-only build missed. Each
  #    layer also gets a <col>_n companion (distinct individuals behind the share).
  ph  <- safe_load("DP1.10055.001", site)
  pht <- pick_table(ph, "phe_statusintensity")
  join_pheno <- function(out, rx, col) {
    sh <- pheno_share(pht, rx)
    if (is.null(sh)) { out[[col]] <- NA_real_; out[[paste0(col, "_n")]] <- NA_integer_; return(out) }
    j <- dplyr::left_join(out["ym"], sh, by = "ym")
    out[[col]]               <- j$share
    out[[paste0(col, "_n")]] <- j$n
    out
  }
  out <- join_pheno(out, "^Open flowers$", "flowering_pct")
  out <- join_pheno(out, "[Bb]reaking leaf buds|[Ee]merging needles|[Yy]oung (leaves|needles)|[Ii]ncreasing leaf size|[Ii]nitial growth", "greenup_pct")
  out <- join_pheno(out, "^Fruits$", "fruiting_pct")

  out$source <- "neon"
  # drop months with no data at all (keeps files lean)
  keep_cols <- c("precip_mm", "temp_c", "temp_min", "temp_max",
                 "flowering_pct", "greenup_pct", "fruiting_pct")
  has_any <- rowSums(!is.na(out[keep_cols])) > 0
  out[has_any, , drop = FALSE]
}

# ---- run ------------------------------------------------------------------

cat(sprintf("Refreshing environmental overlays for %d sites (%s → %s) into %s/\n\n",
            length(sites), start_d, end_d, out_dir))

for (s in sites) {
  f <- file.path(out_dir, paste0(s, ".rds"))
  # full build skips a site that's already bundled; top-up mode always refreshes
  if (file.exists(f) && !.recent) { cat(sprintf("• %-5s skip (exists, %.1f KB)\n", s, file.size(f)/1e3)); next }
  cat(sprintf("• %-5s building%s…\n", s, if (.recent) " (recent top-up)" else ""))
  env <- tryCatch(build_site_env(s), error = function(e) {
    cat(sprintf("    ERROR %s: %s\n", s, conditionMessage(e))); NULL })
  if (is.null(env) || !nrow(env)) { cat(sprintf("    no env data for %s\n", s)); next }
  # top-up: merge the freshly-pulled recent months into the existing bundle —
  # drop the months we just re-pulled, keep everything older, then append.
  if (.recent && file.exists(f)) {
    prev <- tryCatch(tibble::as_tibble(readRDS(f)), error = function(e) NULL)
    if (!is.null(prev) && nrow(prev)) {
      prev <- prev[!(prev$ym %in% env$ym), , drop = FALSE]
      env  <- dplyr::bind_rows(prev, env)
      env  <- env[order(env$ym), , drop = FALSE]
    }
  }
  saveRDS(tibble::as_tibble(env), f, compress = "xz")
  cat(sprintf("    saved %s: %d months, %.1f KB\n", s, nrow(env), file.size(f)/1e3))
}

cat(sprintf("\nDone. Bundle now has %d site env files.\n",
            length(list.files(out_dir, pattern = "\\.rds$"))))
