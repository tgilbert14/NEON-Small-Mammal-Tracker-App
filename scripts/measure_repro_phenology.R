#!/usr/bin/env Rscript

# Audit tool: print the monthly reproductive-phenology vectors that the
# Phenology tab and phenoInsight banner read (repro_by_month()), so a change to
# flag_repro()'s token matching can be compared before/after on real bundles.
#
# Usage (repo root):
#   Rscript --vanilla scripts/measure_repro_phenology.R [helpers.R] [SITE ...]
# The optional first argument points at an alternate helpers file (e.g. a copy
# of `git show HEAD:R/helpers.R` saved inside R/) to measure the pre-change
# behaviour. Sites default to SRER, ABBY and HARV.

args <- commandArgs(trailingOnly = TRUE)
helpers <- "R/helpers.R"
if (length(args) && grepl("[.]R$", args[1])) { helpers <- args[1]; args <- args[-1] }
sites <- if (length(args)) args else c("SRER", "ABBY", "HARV")
source(helpers, chdir = TRUE)

cat(sprintf("helpers: %s\n", helpers))
for (s in sites) {
  bm <- repro_by_month(clean_mam(readRDS(file.path("data", "sites", paste0(s, ".rds")))))
  cat(sprintf("\n%s\n", s))
  if (is.null(bm)) { cat("  (no adult rows)\n"); next }
  out <- data.frame(mon = month.abb[bm$mon], females = bm$females, pf = bm$pf,
                    males = bm$males, pm = bm$pm)
  print(out, row.names = FALSE)
}
