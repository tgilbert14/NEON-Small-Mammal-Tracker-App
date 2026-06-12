# Deploy EcoPlot — bundles only the files the app needs (incl. the per-site
# data bundle) and pushes to shinyapps.io. Run: Rscript scripts/deploy.R
suppressMessages(library(rsconnect))
files <- c("global.R", "ui.R", "server.R",
           list.files("R", full.names = TRUE),
           list.files("www", recursive = TRUE, full.names = TRUE),
           list.files("data-sample", full.names = TRUE),
           list.files("data/sites", pattern = "\\.rds$", full.names = TRUE))
files <- files[file.exists(files)]
cat("Bundling", length(files), "files (",
    length(list.files("data/sites", pattern = "\\.rds$")), "bundled sites )\n")
deployApp(appDir = ".", appFiles = files,
          appName = "RatTrapHistory", account = "t-lama", server = "shinyapps.io",
          forceUpdate = TRUE, launch.browser = FALSE, logLevel = "normal")
cat("\nDEPLOY_DONE\n")
