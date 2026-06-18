# ===========================================================================
# write_manifest.R — (re)generate manifest.json for a lean, bundle-only
# Posit Connect Cloud deploy (git-backed).
#
# Bundles ONLY what the running app needs: global/ui/server + R/ + www/ + the
# precomputed indexes (data/*.rds) + the per-site bundles (data/sites/*.rds) +
# the demo sample. It does NOT bundle scripts/, docs/, rsconnect/, or the README.
#
# neonUtilities is intentionally EXCLUDED — it's referenced dynamically in
# global.R (.NEON_PKG) so the dependency scanner never pins it, keeping the
# deploy lean (no wasm build; live-pull-on-cold-worker is a hang risk). The
# deployed app is bundle-only; the optional live-fetch still works in local dev.
#
# Run with an R that has the app's runtime packages (R 4.3.1 here has them all):
#   "C:\Program Files\R\R-4.3.1\bin\Rscript.exe" scripts/write_manifest.R
# Re-run whenever runtime dependencies change, then commit manifest.json.
# ===========================================================================
suppressMessages(library(rsconnect))

appFiles <- c(
  "global.R", "ui.R", "server.R",
  list.files("R", pattern = "\\.R$", full.names = TRUE),
  list.files("www", recursive = TRUE, full.names = TRUE),
  Sys.glob("data/*.rds"),                                       # precomputed indexes
  list.files("data/sites", pattern = "\\.rds$", full.names = TRUE),
  list.files("data/env",   pattern = "\\.rds$", full.names = TRUE),   # env overlays
  list.files("data-sample", pattern = "\\.rds$", full.names = TRUE)
)
appFiles <- unique(appFiles[file.exists(appFiles)])

cat(sprintf("Writing manifest for %d files (%d site bundles)...\n",
            length(appFiles), length(list.files("data/sites", pattern = "\\.rds$"))))
rsconnect::writeManifest(appDir = ".", appFiles = appFiles)

# quick self-check
m <- readLines("manifest.json", warn = FALSE)
pkgs <- gsub('.*"([^"]+)": \\{', "\\1",
             grep('^\\s*"[A-Za-z0-9.]+": \\{\\s*$', m, value = TRUE))
cat(sprintf("manifest.json written: %d packages.\n", sum(grepl('"Source"', m))))
if (any(grepl("neonUtilities", m))) cat("WARNING: neonUtilities leaked into the manifest!\n") else
  cat("OK: neonUtilities is NOT in the manifest (lean bundle-only build).\n")
