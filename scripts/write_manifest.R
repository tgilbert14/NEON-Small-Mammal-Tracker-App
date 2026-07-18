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

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

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

# ---- freeze ordinary packages to the dated Posit jammy snapshot ------------
# rsconnect can record floating or platform-agnostic repository URLs. Replace
# those URLs with one dated jammy snapshot so ordinary R dependencies are
# reproducible. This is repository provenance, not a promise that Connect will
# install a binary: native geospatial packages are separately installed from
# exact CRAN source tarballs and verified below.
mtxt <- readLines("manifest.json", warn = FALSE)
RSPM_SNAPSHOT <- "https://packagemanager.posit.co/cran/__linux__/jammy/2026-07-15"
mtxt <- gsub("https://packagemanager.posit.co/cran/latest",
             RSPM_SNAPSHOT, mtxt, fixed = TRUE)
mtxt <- gsub("https://packagemanager.posit.co/cran/__linux__/jammy/latest",
             RSPM_SNAPSHOT, mtxt, fixed = TRUE)
mtxt <- gsub("https://cloud.r-project.org",
             RSPM_SNAPSHOT, mtxt, fixed = TRUE)
writeLines(mtxt, "manifest.json")
cat(sprintf("Repo frozen to RSPM jammy snapshot %s.\n", RSPM_SNAPSHOT))

# ---- VERIFY the installed geospatial closure + R version -------------------
# ROOT-CAUSE FIX for the recurring "worked fine, then start-up error, republish
# fixes it" outage. Refresh regenerates the manifest in a review branch; a later
# approved merge can trigger Connect Cloud to republish the verified closure.
# rsconnect::writeManifest() snapshots what is actually installed in the fresh
# GitHub runner. CI and refresh therefore install the declared versions from exact
# CRAN tarball URLs before this script runs.
# So an untouched app "spontaneously" breaks whenever a monthly refresh floats a
# package (or R) forward to a version that won't SOURCE-COMPILE on Connect's build
# image (Ubuntu jammy: GDAL 3.4.1, GEOS 3.10, PROJ 8.2, Abseil ~2022). The build
# dies mid-restore, the container recycles to a broken state ("start-up error"),
# and a manual republish only helps transiently.
#
# leaflet (the picker map) drags in the ENTIRE native geospatial stack, all of
# which Connect may need to compile FROM SOURCE regardless of the snapshot lane:
#     leaflet -> raster -> terra            (terra >= 1.8-54 needs GDAL 3.8)
#     leaflet -> sf      -> s2, units, ...   (s2 >= ... needs newer Abseil)
# Pinning ONLY terra (the first landmine we hit) left sf/s2/units/wk/classInt AND
# the R `platform` version free to float on the next refresh — which is exactly
# how it kept re-breaking. So the workflows install the whole known-good closure
# and this script verifies it plus the R version. These are all
# install-only deps (the app uses leaflet only for markers/tiles; it never calls
# terra::/sf::/s2::), so freezing older versions has ZERO runtime impact.
#
# To intentionally move a pin, update both workflow URL lists and this gate, then
# confirm the actual package compiles on jammy's system libraries.
GEO_PINS <- c(
  terra    = "1.8-50",   # last release before the unguarded GDAL-3.8 multidim code (1.8-54)
  sf       = "1.1-1",    # proven on jammy GDAL 3.4.1 / GEOS 3.10 / PROJ 8.2
  s2       = "1.1.11",   # bundles its own Abseil; jammy's system Abseil is too old
  units    = "1.0-1",
  wk       = "0.9.5",
  classInt = "0.4-11",
  raster   = "3.6-32",   # satisfied by terra 1.8-50 (needs terra >= 1.8-5)
  sp       = "2.2-1"
)
GEO_URLS <- c(
  terra    = "https://cran.r-project.org/src/contrib/Archive/terra/terra_1.8-50.tar.gz",
  sf       = "https://cran.r-project.org/src/contrib/sf_1.1-1.tar.gz",
  s2       = "https://cran.r-project.org/src/contrib/s2_1.1.11.tar.gz",
  units    = "https://cran.r-project.org/src/contrib/units_1.0-1.tar.gz",
  wk       = "https://cran.r-project.org/src/contrib/wk_0.9.5.tar.gz",
  classInt = "https://cran.r-project.org/src/contrib/classInt_0.4-11.tar.gz",
  raster   = "https://cran.r-project.org/src/contrib/raster_3.6-32.tar.gz",
  sp       = "https://cran.r-project.org/src/contrib/sp_2.2-1.tar.gz"
)

# Source-built package DESCRIPTION files contain a wall-clock `Built` timestamp.
# That field changes on every otherwise-identical validator run and is not package
# identity, provenance, compatibility, or an install input. Remove it only for the
# exact URL-pinned closure. rsconnect also represents these direct installs as a URL
# source with DESCRIPTION's symbolic `CRAN` repository label. Connect Cloud requires
# the deployable CRAN lane plus an absolute repository URL so it can resolve current
# and archived versions. Canonicalize those two top-level fields while retaining each
# exact installation tarball in RemotePkgRef.
canonical <- jsonlite::fromJSON("manifest.json", simplifyVector = FALSE)
for (pkg in names(GEO_PINS)) {
  if (!is.null(canonical$packages[[pkg]]$description)) {
    canonical$packages[[pkg]]$description$Built <- NULL
    canonical$packages[[pkg]]$Source <- "CRAN"
    canonical$packages[[pkg]]$Repository <- "https://cran.r-project.org"
  }
}
jsonlite::write_json(canonical, "manifest.json", auto_unbox = TRUE, pretty = TRUE,
                     null = "null")
cat("Canonicalized the CRAN deployment lane and non-semantic Built timestamps for the exact URL package closure.\n")

# Pin the R version too: a runner R bump (seen: 4.5.2 -> 4.6.0) changes the
# whole build image and can invalidate binary/source assumptions on republish.
R_PLATFORM_PIN <- "4.5.2"

# ---- hard gate: installed versions MUST be present and correct --------------
# A refresh must never ship a floated or fabricated geospatial version. This gate
# checks writeManifest output as generated; it does not mutate platform, Version,
# or RemoteSha fields.
chk <- jsonlite::fromJSON("manifest.json", simplifyVector = FALSE)
bad <- character(0)
if (is.null(chk$platform) || !identical(chk$platform, R_PLATFORM_PIN))
  bad <- c(bad, sprintf("platform=%s (want actual %s)",
                       if (is.null(chk$platform)) "<missing>" else as.character(chk$platform),
                       R_PLATFORM_PIN))
repo_by_pkg <- vapply(chk$packages, function(x) {
  repo <- x$Repository
  if (is.null(repo) || length(repo) != 1L || is.na(repo)) "" else as.character(repo)
}, character(1), USE.NAMES = TRUE)

# Packages installed from exact `url::` CRAN tarballs retain their exact URL origin
# in DESCRIPTION remote metadata. The deployable manifest must use the CRAN source
# lane and an absolute CRAN repository URL because Connect uses those top-level fields
# to select current/archive resolution and a network location;
# ordinary dependencies resolved from the dated Posit snapshot must record that
# snapshot URL. Reject crossed lanes, blank/third values, or a URL that differs by
# even one character from the declared build input.
geo_repo <- repo_by_pkg[intersect(names(GEO_PINS), names(repo_by_pkg))]
runtime_repo <- repo_by_pkg[setdiff(names(repo_by_pkg), names(GEO_PINS))]
if (length(geo_repo) != length(GEO_PINS) ||
    any(geo_repo != "https://cran.r-project.org"))
  bad <- c(bad, sprintf("geospatial repositories=[%s] (want absolute CRAN URL for exact URL installs)",
                       paste(unique(unname(geo_repo)), collapse = ",")))
if (length(runtime_repo) == 0L || any(runtime_repo != RSPM_SNAPSHOT))
  bad <- c(bad, sprintf("ordinary package repositories=[%s] (want dated snapshot %s)",
                       paste(unique(unname(runtime_repo)), collapse = ","), RSPM_SNAPSHOT))
for (pkg in names(GEO_PINS)) {
  if (is.null(chk$packages[[pkg]])) {
    bad <- c(bad, sprintf("%s=<missing> (want %s)",
                         pkg, unname(GEO_PINS[[pkg]])))
    next
  }
  got <- chk$packages[[pkg]]$description$Version
  if (!identical(got, unname(GEO_PINS[[pkg]])))
    bad <- c(bad, sprintf("%s=%s (want actual %s)",
                         pkg, got, unname(GEO_PINS[[pkg]])))
  source <- as.character(chk$packages[[pkg]]$Source %||% "")
  remote_type <- as.character(chk$packages[[pkg]]$description$RemoteType %||% "")
  remote_ref <- as.character(chk$packages[[pkg]]$description$RemotePkgRef %||% "")
  built <- as.character(chk$packages[[pkg]]$description$Built %||% "")
  expected_ref <- paste0("url::", unname(GEO_URLS[[pkg]]))
  if (!identical(source, "CRAN") || !identical(remote_type, "url") ||
      !identical(remote_ref, expected_ref) || nzchar(built))
    bad <- c(bad, sprintf(
      "%s origin Source=%s RemoteType=%s RemotePkgRef=%s Built=%s (want exact %s and no non-semantic build timestamp)",
      pkg, source, remote_type, remote_ref, built, expected_ref))
}
if (length(bad)) {
  stop(sprintf(
    "GEO-PROVENANCE GATE FAILED: the generated manifest does not describe the actually installed known-good package/R closure: %s. Do NOT commit/push this manifest.",
    paste(bad, collapse = "; ")), call. = FALSE)
}
cat("OK: generated manifest records the actual known-good R + geospatial closure.\n")

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
