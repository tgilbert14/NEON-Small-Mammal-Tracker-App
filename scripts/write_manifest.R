# ===========================================================================
# write_manifest.R — (re)generate a LEAN manifest.json for a bundle-only
# Posit Connect Cloud deploy (git-backed).
#
# Bundles ONLY what the running app needs: global/ui/server + R/ + www/ + the
# precomputed indexes (data/*.rds) + the per-site bundles (data/sites/*.rds) +
# the env overlays + the demo sample. It does NOT bundle scripts/, docs/,
# rsconnect/, or the README.
#
# neonUtilities is intentionally EXCLUDED — it's referenced dynamically in
# global.R (.NEON_PKG) so the dependency scanner *shouldn't* pin it. But
# rsconnect::writeManifest() also snapshots the installed library and, because
# neonUtilities is installed in the dev environment, it leaks BOTH neonUtilities
# AND its heavy unique dependency `arrow` into the manifest. So after writing we
# PRUNE the live-fetch-only packages and then drop any package left UNREACHABLE
# from the real runtime set (a dependency-closure prune — this is what keeps
# `arrow` out without hard-coding every transitive name). `data.table` is KEPT:
# it is a genuine hard Import of `plotly`, a runtime package, so Connect needs it.
#
# Run with an R that has the app's runtime packages:
#   "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" scripts/write_manifest.R
# Re-run whenever runtime dependencies change, then commit manifest.json.
# ===========================================================================
suppressMessages({
  library(rsconnect)
  library(jsonlite)
})

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

# ---- prune the live-fetch-only packages (lean bundle-only build) -----------
# Packages the running, bundle-only app genuinely loads (global.R's library()
# block + R/report_pdf.R). neonUtilities is NOT here — it's lazy/optional.
RUNTIME_PKGS <- c(
  "shiny","bslib","bsicons","dplyr","tidyr","stringr","tibble","plotly",
  "leaflet","DT","shinyjs","shinycssloaders","RColorBrewer","htmltools",
  "ggplot2"   # report_pdf.R (grid/grDevices ship with R)
)
# Always strip these even if writeManifest pinned them — they are ONLY needed by
# the optional live NEON pull, which never runs in the deployed bundle.
DROP_PKGS <- c("neonUtilities")

mani <- jsonlite::fromJSON("manifest.json", simplifyVector = FALSE)
pkgs <- mani$packages

# Build the dependency graph from the manifest's own DESCRIPTION metadata, then
# keep only packages REACHABLE from RUNTIME_PKGS (after removing DROP_PKGS). This
# walks Imports + Depends + LinkingTo, so a package pulled in solely by a dropped
# live-fetch package (e.g. `arrow`, unique to neonUtilities) falls out cleanly,
# while a shared transitive dep (e.g. `data.table`, needed by plotly) is kept.
dep_names <- function(info) {
  d <- info$description
  if (is.null(d)) return(character(0))
  fields <- paste(c(d$Imports, d$Depends, d$LinkingTo), collapse = ",")
  fields <- gsub("[\r\n]", " ", fields)                 # version constraints wrap lines
  toks <- unlist(strsplit(fields, ","))
  # take ONLY the leading package-name token (drop any " (>= x.y)" version spec,
  # which can wrap across a newline — so strip from the first space/paren on)
  toks <- trimws(sub("[ (].*$", "", trimws(toks)))
  toks <- toks[nzchar(toks) & toks != "R"]
  intersect(toks, names(pkgs))                          # only packages in the manifest
}

reachable <- character(0)
frontier  <- setdiff(intersect(RUNTIME_PKGS, names(pkgs)), DROP_PKGS)
while (length(frontier)) {
  reachable <- union(reachable, frontier)
  nxt <- unique(unlist(lapply(frontier, function(p) dep_names(pkgs[[p]]))))
  nxt <- setdiff(nxt, c(reachable, DROP_PKGS))
  frontier <- nxt
}

removed <- setdiff(names(pkgs), reachable)
if (length(removed)) {
  cat(sprintf("Pruning %d unreachable / live-fetch-only package(s): %s\n",
              length(removed), paste(sort(removed), collapse = ", ")))
  mani$packages <- pkgs[reachable]
  jsonlite::write_json(mani, "manifest.json", auto_unbox = TRUE, pretty = TRUE,
                       null = "null")
}

# ---- force the RSPM LINUX BINARY mirror (jammy) for the package repo --------
# use-public-rspm / rsconnect record the platform-agnostic repo URL
# (packagemanager.posit.co/cran/latest), which Connect Cloud resolves to SOURCE on
# Linux — so terra/sf (via leaflet -> raster -> terra) compile from source and FAIL
# against the build image's GDAL 3.4.1 (terra >= 1.8 needs GDAL >= 3.5). Rewrite the
# repo to the __linux__/jammy binary path (Ubuntu 22.04) so Connect installs
# precompiled binaries and skips the GDAL build. Deterministic text pass, runs last,
# so it sticks no matter how the repo was recorded above (CI or local).
mtxt <- readLines("manifest.json", warn = FALSE)
mtxt <- gsub("https://packagemanager.posit.co/cran/latest",
             "https://packagemanager.posit.co/cran/__linux__/jammy/latest", mtxt, fixed = TRUE)
mtxt <- gsub("https://cloud.r-project.org",
             "https://packagemanager.posit.co/cran/__linux__/jammy/latest", mtxt, fixed = TRUE)
writeLines(mtxt, "manifest.json")
cat("Repo set to RSPM jammy mirror.\n")

# ---- pin terra to the last release before the GDAL-3.8 multidim code --------
# The jammy repo above is NECESSARY BUT NOT SUFFICIENT: Connect Cloud compiles terra
# from source regardless of the repo, against its system GDAL 3.4.1 (Ubuntu jammy).
# terra's multidimensional support (gdal_multidimensional.cpp, which calls the 3-arg
# GDALMDArray::AsClassicDataset — a GDAL 3.8 overload, unguarded in released versions)
# landed in terra 1.8-54, so every terra >= 1.8-54 FAILS to compile on GDAL 3.4.1.
# Pin terra to 1.8-50 (last release before 1.8-54): no GDAL-3.8 code -> compiles on
# 3.4.1, and still satisfies raster's `terra (>= 1.8-5)`. terra/raster are install-only
# deps (leaflet -> raster -> terra; the app uses leaflet for maps and never calls
# terra), so an older terra has ZERO runtime impact.
TERRA_PIN <- "1.8-50"
mm <- jsonlite::fromJSON("manifest.json", simplifyVector = FALSE)
if (!is.null(mm$packages$terra)) {
  mm$packages$terra$description$Version <- TERRA_PIN
  if (!is.null(mm$packages$terra$description$RemoteSha)) mm$packages$terra$description$RemoteSha <- TERRA_PIN
  jsonlite::write_json(mm, "manifest.json", auto_unbox = TRUE, pretty = TRUE, null = "null")
  cat(sprintf("Pinned terra to %s (pre-GDAL-3.8-multidim; compiles on Connect's GDAL 3.4.1).\n", TERRA_PIN))
}

# ---- hard gate: a leaked heavy package must NEVER commit silently ----------
m   <- jsonlite::fromJSON("manifest.json", simplifyVector = FALSE)
keys <- names(m$packages)
cat(sprintf("manifest.json written: %d packages.\n", length(keys)))

# `data.table` is a legitimate hard Import of plotly, so its mere presence is
# fine — but it is ALSO a neonUtilities dependency, so it only counts as a LEAK
# if it rode in WITHOUT a real runtime owner. Here, if neonUtilities/arrow are
# gone (gated below) and plotly is present, data.table is legitimately kept.
leaked <- intersect(c("neonUtilities", "arrow"), keys)
dt_leak <- ("data.table" %in% keys) && !("plotly" %in% keys)   # only a leak if plotly isn't the owner
if (dt_leak) leaked <- c(leaked, "data.table")

if (length(leaked)) {
  stop(sprintf(
    "LEAN-MANIFEST GATE FAILED: heavy live-fetch package(s) leaked into manifest.json: %s. The deployed app is bundle-only and must stay lean. Do NOT commit this manifest.",
    paste(leaked, collapse = ", ")), call. = FALSE)
}
cat("OK: manifest is lean — no neonUtilities / arrow leak (data.table kept only as plotly's runtime dep).\n")
