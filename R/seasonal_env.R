# ===========================================================================
# seasonal_env.R — the Driver Cascade's SEASONAL-AGGREGATE driver read, in-app.
#
# WHY: the monthly env scan (env_corr_scan) correlates DESEASONALIZED MONTHLY
# catch against MONTHLY precip. A "big summer monsoon -> next year's seed-eater
# boom" link is a Jul -> next-May relationship the monthly scan cannot see — the
# monsoon's signal is averaged out of the very annual cycle the scan subtracts.
# The fix (Cass / NEON Driver Cascade): aggregate the driver by SEASON (monsoon
# Jul-Sep, winter Oct-Mar, spring temp Mar-May), one value per YEAR, and
# correlate at an ANNUAL resolution at the prior's stated lag. At SRER this
# recovers monsoon(t)->CPUE(t+1) r=+0.72 (n=7) where the annual read shows +0.20.
#
# Rules are lifted VERBATIM from NEON-Driver-Cascade ann_env_seasonal() +
# cascade_helpers (lag_pairs / exp_adj_p) so the app and the cascade never
# disagree. DO NOT deseasonalize here: an annual seasonal aggregate already
# collapses the within-year cycle to one number per year — there is no monthly
# cycle left to remove, and the between-year variation IS the signal.
#
# This per-site annual correlation is small-n (n ~ 6-10 years). It is SUGGESTIVE,
# never a verdict; the honest evidence pools many sites in the Driver Cascade app.
# ===========================================================================

SEASON_LABELS <- c(
  precip_monsoon = "Summer monsoon rain (Jul-Sep)",
  precip_winter  = "Winter rain (Oct-Mar)",
  temp_spring    = "Spring temperature (Mar-May)")

# ---- one value per YEAR for each seasonal driver, from a monthly env bundle ----
# (date|ym, precip_mm, temp_c). NA-gates + plausibility filter + the water-year
# winter key + the spring-temp MAD outlier QC are exactly ann_env_seasonal's.
seasonal_aggregates <- function(env) {
  if (is.null(env) || !nrow(env)) return(NULL)
  e <- as.data.frame(env)
  e$date <- if ("date" %in% names(e)) as.Date(e$date) else as.Date(paste0(e$ym, "-01"))
  e$year <- suppressWarnings(as.integer(format(e$date, "%Y")))
  e$mo   <- suppressWarnings(as.integer(format(e$date, "%m")))
  e <- e[is.finite(e$year) & is.finite(e$mo), , drop = FALSE]; if (!nrow(e)) return(NULL)
  pr <- suppressWarnings(as.numeric(e$precip_mm)); tc <- suppressWarnings(as.numeric(e$temp_c))
  pr[!(is.finite(pr) & pr >= 0 & pr < 2000)] <- NA
  tc[!(is.finite(tc) & tc > -40 & tc <  50)] <- NA
  e$precip_mm <- pr; e$temp_c <- tc
  e$wy <- ifelse(e$mo >= 10, e$year + 1L, e$year)   # Oct-Dec credited to the year winter ENDS
  agg <- function(rows, key, col, fun, need) {
    s <- e[rows, , drop = FALSE]; if (!nrow(s)) return(data.frame(year = integer(0), v = numeric(0)))
    sp <- split(s[[col]], s[[key]])
    vals <- vapply(sp, function(v) if (sum(!is.na(v)) >= need) fun(v) else NA_real_, numeric(1))
    data.frame(year = as.integer(names(vals)), v = unname(vals))
  }
  win <- agg(e$mo %in% c(10,11,12,1,2,3), "wy",   "precip_mm", function(v) round(sum(v,  na.rm = TRUE), 1), 5)
  mon <- agg(e$mo %in% c(7,8,9),          "year", "precip_mm", function(v) round(sum(v,  na.rm = TRUE), 1), 3)
  spr <- agg(e$mo %in% c(3,4,5),          "year", "temp_c",    function(v) round(mean(v, na.rm = TRUE), 2), 2)
  names(win)[2] <- "precip_winter"; names(mon)[2] <- "precip_monsoon"; names(spr)[2] <- "temp_spring"
  out <- Reduce(function(a, b) merge(a, b, by = "year", all = TRUE), list(win, mon, spr))
  tv <- out$temp_spring[is.finite(out$temp_spring)]
  if (length(tv) >= 4) { med <- stats::median(tv); thr <- max(6, 3 * stats::mad(tv))
    out$temp_spring[is.finite(out$temp_spring) & abs(out$temp_spring - med) > thr] <- NA_real_ }
  out
}

# pair driver[year t] with response[year t+lag] — cascade lag_pairs(), verbatim.
.seas_lag_pairs <- function(ann, from, lag) {
  drv <- data.frame(year = ann$year,        x = ann[[from]])
  rsp <- data.frame(year = ann$year - lag,   y = ann$value)
  m <- merge(drv, rsp, by = "year")
  m[is.finite(m$x) & is.finite(m$y), , drop = FALSE]
}

# simple permutation null p for ONE link (shuffle the response) — cascade link_stat().
# This is the per-link p the cascade reports for a STATED prior (the biome dictates
# which seasonal driver leads, so the expected driver is not a search). seed 1 = stable.
.seas_simple_p <- function(m, r, nperm = 2000) {
  if (nrow(m) < 6 || !is.finite(r)) return(NA_real_)
  set.seed(1L)
  perm <- replicate(nperm, suppressWarnings(stats::cor(m$x, sample(m$y))))
  round(mean(abs(perm) >= abs(r) - 1e-9, na.rm = TRUE), 3)
}

# best-of-K circular-shift adjusted p for the SELECTED (col,lag) — cascade exp_adj_p().
# Shifts the response (preserves serial structure), re-scans every candidate combo,
# takes max|r|; penalizes the season search AND annual autocorrelation. seed 7 = stable.
.seas_adj_p <- function(ann, combos, observed_r, nperm = 2000) {
  y <- ann$value; ny <- length(y)
  if (ny < 4 || !is.finite(observed_r)) return(NA_real_)
  scan_max <- function(a) {
    rs <- vapply(combos, function(cb) {
      m <- .seas_lag_pairs(a, cb$col, cb$lag)
      if (nrow(m) >= 3) { r <- suppressWarnings(stats::cor(m$x, m$y)); if (is.finite(r)) abs(r) else NA_real_ } else NA_real_
    }, numeric(1))
    if (all(is.na(rs))) NA_real_ else max(rs, na.rm = TRUE)
  }
  set.seed(7L)
  perm_max <- replicate(nperm, { k <- sample.int(ny - 1L, 1L)
    a2 <- ann; a2$value <- y[((seq_len(ny) - 1L + k) %% ny) + 1L]; scan_max(a2) })
  round(mean(perm_max >= abs(observed_r) - 1e-9, na.rm = TRUE), 3)
}

# the site's limiting-resource class — matches the cascade's biome_class() (desert /
# sagebrush -> water-limited). The water-limited NEON site set is small + stable.
.WATER_LIMITED <- c("JORN", "MOAB", "ONAQ", "SRER", "YELL")
seasonal_biome <- function(site, bio = NULL) {
  if (!is.null(bio) && nzchar(bio) && grepl("desert|sagebrush|semi-desert", tolower(bio))) return("water-limited")
  if (!is.null(site) && toupper(site) %in% .WATER_LIMITED) return("water-limited")
  "temperature-limited"
}

# which seasonal driver LEADS at this biome (reuses the cascade's swap logic).
.seas_expected <- function(driver, biome) {
  if (is.null(biome) || is.na(biome) || !nzchar(biome)) return(TRUE)
  if (grepl("water",  biome, ignore.case = TRUE)) return(driver %in% c("precip_monsoon", "precip_winter"))
  if (grepl("temp",   biome, ignore.case = TRUE)) return(driver %in% c("temp_spring"))
  TRUE
}

# ---- the shared API every env-panel app calls -----------------------------
# env      : the site's monthly env bundle (data/env/<SITE>.rds)
# response : data.frame(year=<int>, value=<num>) — the APP's own annual metric
# biome    : "water-limited" | "temperature-limited" (NULL => all expected)
# lags     : stated prior lag per driver (monsoon=1 for mammals; the caller may
#            override e.g. monsoon=0 for a fast within-season responder like beetles)
# Returns a data.frame ranked (expected first, then |r|): driver,label,lag,r,n,p,
# sign,expected.  n-gate: n<3 dropped; n in 3-5 => r but p=NA (exploratory).
seasonal_driver_links <- function(env, response, biome = NULL,
                                  lags = c(precip_monsoon = 1L, precip_winter = 0L, temp_spring = 0L),
                                  nperm = 2000) {
  agg <- seasonal_aggregates(env); if (is.null(agg) || !nrow(agg)) return(NULL)
  if (is.null(response) || !nrow(response)) return(NULL)
  resp <- data.frame(year = suppressWarnings(as.integer(response$year)),
                     value = suppressWarnings(as.numeric(response$value)))
  resp <- resp[is.finite(resp$year) & is.finite(resp$value), , drop = FALSE]
  ann  <- merge(agg, resp, by = "year", all = TRUE)
  drivers <- intersect(names(lags), names(ann))
  combos  <- lapply(drivers, function(d) list(col = d, lag = as.integer(lags[[d]])))
  rows <- lapply(drivers, function(d) {
    L <- as.integer(lags[[d]]); m <- .seas_lag_pairs(ann, d, L); n <- nrow(m)
    if (n < 3) return(NULL)
    r <- suppressWarnings(stats::cor(m$x, m$y)); if (!is.finite(r)) return(NULL)
    # p   = simple per-link permutation null (the cascade's per-link p for a stated prior)
    # p_adj = best-of-K circular-shift null (penalizes testing several seasons; conservative)
    p     <- if (n >= 6) .seas_simple_p(m, r, nperm) else NA_real_
    p_adj <- if (n >= 6) .seas_adj_p(ann, combos, r, nperm) else NA_real_
    data.frame(driver = d, label = unname(SEASON_LABELS[d]) %||% d, lag = L,
               r = round(r, 2), n = n, p = p, p_adj = p_adj, sign = sign(r),
               expected = .seas_expected(d, biome), stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, Filter(Negate(is.null), rows)); if (is.null(out) || !nrow(out)) return(NULL)
  out[order(!out$expected, -abs(out$r)), , drop = FALSE]
}
