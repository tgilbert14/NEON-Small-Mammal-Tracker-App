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
#   rh_pct      (monthly MEAN, DP1.00098.001 relative humidity)
#   vswc_pct    (monthly MEAN, DP1.00094.001 soil water content, as % volume)
#   fruiting_pct(monthly MEAN % of phenology individuals in fruit, DP1.10055.001)
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

source("R/site_metadata.R")  # canonical site list

out_dir <- "data/env"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

start_d <- "2013-01"
end_d   <- format(Sys.Date(), "%Y-%m")
sites   <- neon_sites$site

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

safe_load <- function(dpID, site) {
  tryCatch(
    loadByProduct(dpID = dpID, site = site, startdate = start_d, enddate = end_d,
                  package = "basic", check.size = "F"),
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
  prt <- pick_table(pr, "wss_daily_precip|.*daily.*[Pp]recip|PRIPRE")
  out <- join1(out, monthly(prt, "[Pp]recipBulk|secPrecipBulk|priPrecipBulk|[Pp]recip", sum), "precip_mm")

  # 2) air temperature — single aspirated, 30-min; keep one tower level
  at <- safe_load("DP1.00002.001", site)
  att <- pick_table(at, "SAAT_30min|saat.*30")
  if (!is.null(att) && "verticalPosition" %in% names(att))
    att <- att[att$verticalPosition == min(att$verticalPosition, na.rm = TRUE), ]
  out <- join1(out, monthly(att, "tempSingleMean", mean), "temp_c")
  out <- join1(out, monthly(att, "tempSingleMinimum", min),  "temp_min")
  out <- join1(out, monthly(att, "tempSingleMaximum", max),  "temp_max")

  # 3) relative humidity — DP1.00098.001, 30-min mean
  rh <- safe_load("DP1.00098.001", site)
  rht <- pick_table(rh, "RH_30min|rh.*30")
  if (!is.null(rht) && "verticalPosition" %in% names(rht))
    rht <- rht[rht$verticalPosition == min(rht$verticalPosition, na.rm = TRUE), ]
  out <- join1(out, monthly(rht, "RHMean", mean), "rh_pct")

  # 4) soil water content — DP1.00094.001; shallow depth/one position, cap artifact
  sm <- safe_load("DP1.00094.001", site)
  smt <- pick_table(sm, "SWS_30_minute|swc.*30|soilWaterContent")
  if (!is.null(smt)) {
    vv <- pick_col(smt, "VSWCMean|soilWaterContent")
    if (!is.null(vv)) {
      vv[vv > 0.6 | vv < 0] <- NA          # documented high-VSWC artifact guard
      smt$.vswc <- vv * 100                 # fraction -> % volume
      m <- monthly(smt, "\\.vswc", mean)
      out$vswc_pct <- if (is.null(m)) NA_real_ else
        dplyr::left_join(out["ym"], setNames(m, c("ym","v")), by = "ym")$v
    } else out$vswc_pct <- NA_real_
  } else out$vswc_pct <- NA_real_

  # 5) plant phenology — DP1.10055.001; monthly % of records in "Fruits" = yes
  ph <- safe_load("DP1.10055.001", site)
  pht <- pick_table(ph, "phe_statusintensity")
  if (!is.null(pht) && all(c("phenophaseName", "phenophaseStatus") %in% names(pht))) {
    fr <- pht[grepl("[Ff]ruit", pht$phenophaseName), ]
    if (nrow(fr)) {
      fr$.yes <- as.integer(grepl("^yes", tolower(fr$phenophaseStatus)))
      fr$ym <- month_key(fr)
      m <- stats::aggregate(list(value = fr$.yes), by = list(ym = fr$ym),
                            FUN = function(x) 100 * mean(x, na.rm = TRUE))
      out$fruiting_pct <- dplyr::left_join(out["ym"], setNames(m, c("ym","v")), by = "ym")$v
    } else out$fruiting_pct <- NA_real_
  } else out$fruiting_pct <- NA_real_

  out$source <- "neon"
  # drop months with no data at all (keeps files lean)
  keep_cols <- c("precip_mm","temp_c","temp_min","temp_max","rh_pct","vswc_pct","fruiting_pct")
  has_any <- rowSums(!is.na(out[keep_cols])) > 0
  out[has_any, , drop = FALSE]
}

# ---- run ------------------------------------------------------------------

cat(sprintf("Refreshing environmental overlays for %d sites (%s → %s) into %s/\n\n",
            length(sites), start_d, end_d, out_dir))

for (s in sites) {
  f <- file.path(out_dir, paste0(s, ".rds"))
  if (file.exists(f)) { cat(sprintf("• %-5s skip (exists, %.1f KB)\n", s, file.size(f)/1e3)); next }
  cat(sprintf("• %-5s building…\n", s))
  env <- tryCatch(build_site_env(s), error = function(e) {
    cat(sprintf("    ERROR %s: %s\n", s, conditionMessage(e))); NULL })
  if (is.null(env) || !nrow(env)) { cat(sprintf("    no env data for %s\n", s)); next }
  saveRDS(tibble::as_tibble(env), f, compress = "xz")
  cat(sprintf("    saved %s: %d months, %.1f KB\n", s, nrow(env), file.size(f)/1e3))
}

cat(sprintf("\nDone. Bundle now has %d site env files.\n",
            length(list.files(out_dir, pattern = "\\.rds$"))))
