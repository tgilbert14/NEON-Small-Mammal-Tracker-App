# ===========================================================================
# fix_bundles_altrep.R  —  one-time repair for non-portable site bundles.
#
# The bundles were saved with arrow ALTREP string columns (arrow::array_string
# _vector). Any R that can't unserialize those (no arrow loaded, older R) reads
# every character column back as a LENGTH-ZERO vector — silently emptying tagID,
# scientificName, plotID, etc. This rewrites each bundle with PLAIN base-R
# vectors so the .rds files are portable to any environment.
#
# RUN WITH R that can read the originals (the writer R + arrow):
#   "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" scripts/fix_bundles_altrep.R
# ===========================================================================
suppressWarnings(suppressMessages(library(arrow)))   # needed to materialize the ALTVEC on read
ROOT <- getwd()
SITE_DIR <- file.path(ROOT, "data", "sites")

# Force a column to a plain materialized base vector (strips arrow/any ALTREP),
# preserving Date and factor types.
plainize <- function(x) {
  if (inherits(x, "Date"))   return(structure(as.numeric(x[seq_along(x)]), class = "Date"))
  if (inherits(x, "POSIXct")) return(structure(as.numeric(x[seq_along(x)]), class = class(x), tzone = attr(x, "tzone")))
  if (is.factor(x))          return(factor(as.character(x[seq_along(x)])))
  x[seq_along(x)]
}
fix_one <- function(path) {
  d <- readRDS(path)
  for (nm in names(d)) d[[nm]] <- plainize(d[[nm]])
  d <- tibble::as_tibble(d)
  saveRDS(d, path, compress = "xz")
  zero <- sum(vapply(d, length, integer(1)) == 0)
  sprintf("%-28s rows=%6d cols=%d zero-len=%d", basename(path), nrow(d), ncol(d), zero)
}

files <- c(list.files(SITE_DIR, pattern = "\\.rds$", full.names = TRUE),
           list.files(file.path(ROOT, "data-sample"), pattern = "\\.rds$", full.names = TRUE))
cat("Repairing", length(files), "bundle(s)...\n")
for (f in files) {
  res <- tryCatch(fix_one(f), error = function(e) paste("ERROR", basename(f), conditionMessage(e)))
  cat(res, "\n")
}
cat("Done.\n")
