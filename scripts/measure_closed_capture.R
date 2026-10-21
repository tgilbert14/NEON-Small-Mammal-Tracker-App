#!/usr/bin/env Rscript
# Per-site closed-capture summary over every committed bundle in data/sites.
#
# Usage (from the repository root):
#   Rscript --vanilla scripts/measure_closed_capture.R [--helpers=R/helpers.R] [--out=FILE.csv]
#
# Default helpers are the current R/helpers.R; pass --helpers=<path> to run an
# older copy of the estimator (for example R/helpers.R exported from an earlier
# commit) for a before/after comparison (the 2026-10-20 columns need the
# current helpers). Default output is
# docs/audit-fixes/2026-10-18-closed-capture-by-site.csv. The script reads only
# committed bundles and makes no network calls.

args <- commandArgs(trailingOnly = TRUE)
opt <- function(name, default) {
  hit <- grep(sprintf("^--%s=", name), args, value = TRUE)
  if (length(hit)) sub(sprintf("^--%s=", name), "", hit[[1L]]) else default
}
helpers <- normalizePath(opt("helpers", "R/helpers.R"))
out <- opt("out", "docs/audit-fixes/2026-10-18-closed-capture-by-site.csv")
sites_dir <- normalizePath("data/sites")
suppressMessages(source(helpers, chdir = TRUE))

rows <- lapply(sort(list.files(sites_dir, pattern = "[.]rds$", full.names = TRUE)), function(f) {
  site <- sub("[.]rds$", "", basename(f))
  d <- clean_mam(readRDS(f))
  b <- bout_closed_capture(d)
  cc <- closed_capture_series(d, b)
  cs <- community_stats(d)
  # bouts that trapped but caught no tagged animal (kept since 2026-10-20)
  zero <- if (is.null(b)) logical(0) else
    b$mnka_marked == 0 & b$n_unmarked == 0 & b$n_unmarked_recap == 0
  # estimable bouts with k >= 3 trapped nights but only 2 nights carrying a
  # tagged capture: Chapman under the old night rule, Schnabel now
  ch2sch <- 0L
  if (!is.null(b) && nrow(b)) {
    capn <- unique(d[mam_tag_present(d$tagID) & !is.na(d$plotID) & !is.na(d$date),
                     c("plotID", "date")])
    bb <- b[order(b$plotID, b$start), ]
    nxt <- ave(as.numeric(bb$start), bb$plotID, FUN = function(x) c(x[-1], Inf))
    ncap <- vapply(seq_len(nrow(bb)), function(i)
      sum(capn$plotID == bb$plotID[i] & capn$date >= bb$start[i] & as.numeric(capn$date) < nxt[i]),
      integer(1))
    ch2sch <- sum(bb$k >= 3 & ncap == 2 &
                    bb$status %in% c("ok", "low-precision", "detection near-complete") & is.finite(bb$N))
  }
  est <- if (is.null(b)) b else
    b[b$status %in% c("ok", "low-precision", "detection near-complete") & is.finite(b$N), ]
  s <- cc$series
  peak <- if (!is.null(est) && nrow(est)) est[which.max(est$N), ] else NULL
  has_m <- !is.null(est) && nrow(est) > 0 && "N_marked" %in% names(est)
  data.frame(
    site = site,
    n_bouts = if (is.null(cc)) 0L else cc$n_bouts,
    n_single = if (is.null(cc)) 0L else cc$n_single,
    n_estimable = if (is.null(cc)) 0L else cc$n_estimable,
    sum_N_estimable = if (is.null(est)) 0 else round(sum(est$N), 1),
    mean_p = if (is.null(cc)) NA_real_ else cc$mean_p,
    mean_detect = if (is.null(cc)) NA_real_ else cc$mean_detect,
    # completeness on ear-tagged animals only (no untagged add-on)
    mean_detect_marked = if (!has_m) NA_real_ else
      round(stats::weighted.mean(pmin(est$mnka_marked / est$N_marked, 1), est$N_marked), 3),
    # share of estimable bouts clamped to MNKA (status "detection near-complete")
    clamp_share = if (is.null(est) || !nrow(est)) NA_real_ else
      round(mean(est$status == "detection near-complete"), 3),
    addon_share = if (!has_m) NA_real_ else round(sum(est$n_unmarked) / sum(est$N), 3),
    n_months = if (is.null(s)) 0L else nrow(s),
    mean_monthly_N = if (is.null(s) || !nrow(s)) NA_real_ else round(mean(s$N), 1),
    n_design_breaks = if (is.null(s) || !"design_break" %in% names(s)) NA_integer_ else
      sum(s$design_break, na.rm = TRUE),
    n_unmarked_total = if (is.null(cc) || is.null(cc$n_unmarked_total)) NA_real_ else cc$n_unmarked_total,
    n_dead_total = if (is.null(cc) || is.null(cc$n_dead_total)) NA_real_ else cc$n_dead_total,
    peak_bout = if (is.null(peak)) NA_character_ else peak$boutID,
    peak_bout_N = if (is.null(peak)) NA_real_ else peak$N,
    individuals_marked = cs$individuals,
    untagged_captures = cs$untagged_captures %||% NA_integer_,
    n_bouts_trapped = if (is.null(b)) 0L else nrow(b),
    n_zero_capture_bouts = sum(zero),
    n_chapman_to_schnabel = ch2sch,
    stringsAsFactors = FALSE)
})
res <- do.call(rbind, rows)
dir.create(dirname(out), showWarnings = FALSE, recursive = TRUE)
utils::write.csv(res, out, row.names = FALSE)
cat(sprintf("wrote %s (%d sites)\n", out, nrow(res)))
print(res[res$site %in% c("SRER", "ABBY", "HARV", "JORN"), ], row.names = FALSE)
