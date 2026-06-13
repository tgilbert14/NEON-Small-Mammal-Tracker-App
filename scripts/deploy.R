# Deploy the NEON Small Mammal Tracker — bundles only the files the app needs
# (incl. the per-site data bundle + the precomputed national indexes) and pushes
# to shinyapps.io. Run: Rscript scripts/deploy.R
# NOTE: shinyapps.io retires 2026-12-31; the planned move is Connect Cloud
# (git-backed, needs a manifest.json via rsconnect::writeManifest()).
suppressMessages(library(rsconnect))
files <- c("global.R", "ui.R", "server.R",
           list.files("R", full.names = TRUE),
           list.files("www", recursive = TRUE, full.names = TRUE),
           list.files("data-sample", full.names = TRUE),
           # precomputed national indexes (picker map + species ranges) live in
           # data/ directly — must ship or the landing map is blank in prod
           Sys.glob("data/*.rds"),
           list.files("data/sites", pattern = "\\.rds$", full.names = TRUE))
files <- unique(files[file.exists(files)])
cat("Bundling", length(files), "files (",
    length(list.files("data/sites", pattern = "\\.rds$")), "bundled sites )\n")
deployApp(appDir = ".", appFiles = files,
          appName = "RatTrapHistory", account = "t-lama", server = "shinyapps.io",
          forceUpdate = TRUE, launch.browser = FALSE, logLevel = "normal")
cat("\nDEPLOY_DONE\n")
