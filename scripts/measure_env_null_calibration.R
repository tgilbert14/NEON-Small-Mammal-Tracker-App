#!/usr/bin/env Rscript
# Type I calibration of env_corr_pvalue() (best |r| over driver x lag, with a
# circular-shift null). For each site the response is a synthetic AR(1) series
# generated on the full monthly calendar between the site's first and last
# sampled month and then kept only on the months the site actually sampled
# (the real gap pattern), so it is independent of every driver by construction.
# The drivers are the site's real monthly env bundle (data/env/<SITE>.rds).
# P(p < alpha) should be close to alpha for a calibrated test.
# Run from the repo root (slow; run in the background):
#   Rscript --vanilla scripts/measure_env_null_calibration.R [runs] [nperm]
# Defaults: 200 runs per cell, nperm = 500 (the app's default).
suppressPackageStartupMessages(library(dplyr))
args <- commandArgs(trailingOnly = TRUE)
runs <- if (length(args) >= 1) as.integer(args[1]) else 200L
nperm <- if (length(args) >= 2) as.integer(args[2]) else 500L
eval(Filter(function(e) is.call(e) && identical(e[[1]], as.name("<-")) &&
              identical(e[[2]], as.name("ENV_LAYERS")), as.list(parse("global.R")))[[1]])
source("R/helpers.R", chdir = TRUE)
sites <- c("SRER", "HARV", "JORN"); rhos <- c(0, 0.8)
res <- list()
for (s in sites) {
  d <- clean_mam(readRDS(file.path("data/sites", paste0(s, ".rds"))))
  env <- readRDS(file.path("data/env", paste0(s, ".rds")))
  sampled <- .mam_cpue_series(d)$date
  cal <- seq(min(sampled), max(sampled), by = "month")
  keep <- match(sampled, cal)
  for (rho in rhos) {
    set.seed(1000L + match(s, sites) * 10L + round(rho * 10))
    p <- vapply(seq_len(runs), function(i) {
      y <- if (rho == 0) stats::rnorm(length(cal)) else
        as.numeric(stats::arima.sim(list(ar = rho), n = length(cal)))
      out <- env_corr_pvalue(NULL, env, nperm = nperm,
                             series = data.frame(date = cal[keep], cpue = y[keep]),
                             seed = 11L + i)
      if (is.null(out)) NA_real_ else out$p
    }, numeric(1))
    ok <- is.finite(p)
    res[[length(res) + 1L]] <- data.frame(
      site = s, rho = rho, months = length(sampled), span_months = length(cal),
      runs = sum(ok), p_lt_05 = mean(p[ok] < 0.05), p_lt_02 = mean(p[ok] < 0.02),
      se_05 = sqrt(0.05 * 0.95 / sum(ok)))
    cat(sprintf("%s rho %.1f: months %d of %d, runs %d, P(p<0.05) %.3f, P(p<0.02) %.3f\n",
                s, rho, length(sampled), length(cal), sum(ok),
                mean(p[ok] < 0.05), mean(p[ok] < 0.02)))
  }
}
tab <- do.call(rbind, res)
print(tab, row.names = FALSE)
saveRDS(tab, file.path(tempdir(), "env_null_calibration.rds"))
