#!/usr/bin/env Rscript

# Fixture tests for the bundle producer (scripts/producer_lib.R,
# scripts/neon_product_meta.R, scripts/restore_unchanged_bundles.R and the pure
# helpers in scripts/refresh_env_data.R). No network. Reads the committed BART
# and HARV bundles. Run from the repository root:
#   Rscript --vanilla scripts/test_producer.R

suppressMessages({ library(tibble); library(jsonlite) })
source("scripts/producer_lib.R")
source("scripts/neon_product_meta.R")
source("R/helpers.R", chdir = TRUE)   # mam_multi_trap_marker()

passed <- 0L
check <- function(ok, label) {
  if (!isTRUE(ok)) stop(sprintf("FAIL: %s", label), call. = FALSE)
  passed <<- passed + 1L
  cat(sprintf("PASS %02d: %s\n", passed, label))
}
expect_error <- function(expr, rx) {
  e <- tryCatch({ force(expr); NULL }, error = function(e) conditionMessage(e))
  !is.null(e) && grepl(rx, e)
}
tmp <- tempfile("test_producer_"); dir.create(tmp)
on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

synthetic_ppn <- function(d) {
  n <- sort(unique(d$nightuid[!is.na(d$nightuid)]))
  data.frame(nightuid = n,
             gridCompletion = rep(c("synthetic-A", "synthetic-B", NA), length.out = length(n)),
             samplingImpractical = rep(c("OK", NA), length.out = length(n)),
             eventID = paste0("E", seq_along(n)), trapsSet = 100L, stringsAsFactors = FALSE)
}

# ---- item 4: determinism -----------------------------------------------------
bart <- readRDS("data/sites/BART.rds")
ppn  <- synthetic_ppn(bart)
md5s <- character()
for (seed in 1:3) {
  set.seed(seed)
  sh <- as.data.frame(bart)[sample(nrow(bart)), sample(ncol(bart)), drop = FALSE]
  p <- ppn[sample(nrow(ppn)), sample(ncol(ppn)), drop = FALSE]
  out <- prepare_site_bundle(sh, p)$bundle
  f <- file.path(tmp, sprintf("BART_%d.rds", seed))
  saveRDS(out, f, compress = "xz")
  md5s[seed] <- unname(tools::md5sum(f))
}
cat(sprintf("      BART shuffled 3 ways -> md5 %s\n", paste(md5s, collapse = " ")))
check(length(unique(md5s)) == 1L, "prepare_site_bundle gives byte-identical .rds from 3 shuffles (rows and columns)")
res <- prepare_site_bundle(bart, ppn)
check(identical(names(res$bundle), intersect(BUNDLE_KEEP, c(names(bart), names(ppn)))),
      "bundle columns follow BUNDLE_KEEP order with the per-plot-night fields appended")
check(!is.unsorted(res$bundle$plotID, strictly = FALSE) && inherits(res$bundle, "tbl_df"),
      "bundle is a tibble sorted by siteID then plotID")

# ---- item 2: per-plot-night join ----------------------------------------------
check(res$report$unmatched == sum(is.na(bart$nightuid)) &&
        res$report$joined == nrow(bart) - sum(is.na(bart$nightuid)),
      "join reports rows joined and rows with no matching nightuid")
check(identical(as.integer(table(res$bundle$gridCompletion, useNA = "ifany")),
                as.integer(res$report$gridCompletion)), "gridCompletion table is logged from the joined bundle")
part <- ppn[-(1:5), ]
r2 <- join_perplotnight(as.data.frame(bart), part)$report
check(r2$unmatched == sum(bart$nightuid %in% ppn$nightuid[1:5]) + sum(is.na(bart$nightuid)),
      "nightuids missing from mam_perplotnight count as unmatched")
dupe <- rbind(ppn, transform(ppn[1, ], gridCompletion = "conflict"))
check(expect_error(join_perplotnight(as.data.frame(bart), dupe), ppn$nightuid[1]),
      "conflicting duplicate nightuid stops and names the nightuid")
check(join_perplotnight(as.data.frame(bart), rbind(ppn, ppn[1, ]))$report$joined == res$report$joined,
      "exact duplicate perplotnight rows collapse without stopping")
check(expect_error(join_perplotnight(as.data.frame(bart), NULL), "mam_perplotnight"),
      "missing mam_perplotnight stops the build")
tn <- data.frame(nightuid = c("a", "a", "a", "b", "b"), trapCoordinate = c("A1", "A2", "A2", "B1", "B2"),
                 trapStatus = c("5 - capture", "6 - trap set and empty", "4 - more than 1 capture in one trap",
                                "1 - trap not set", "6 - trap set and empty"),
                 trapsSet = c(2L, 2L, 2L, 2L, 2L))
tsm <- traps_set_mismatch(tn)
check(tsm$nights == 2L && tsm$mismatched == 1L && tsm$share == 0.5,
      "trapsSet mismatch counts distinct set traps (status 2 to 6) per plot-night")

# ---- item 3: email scrub and double-trap markers --------------------------------
# Emails sit in BART and HARV remarks; the reviewed double-trap markers sit in
# SRER and JERC. Scrub every committed bundle and compare both counts.
tot <- c(emails_left = 0L, email_rows = 0L, before = 0L, after = 0L, na_changed = 0L)
for (f in list.files("data/sites", pattern = "\\.rds$", full.names = TRUE)) {
  d <- readRDS(f); sc <- scrub_remarks(d)
  b0 <- sum(mam_multi_trap_marker(d$remarks) > 0); a0 <- sum(mam_multi_trap_marker(sc$data$remarks) > 0)
  if (sc$rows || b0) cat(sprintf("      %s: %d address(es) in %d row(s) scrubbed; double-trap markers %d before, %d after\n",
                                 sub("\\.rds$", "", basename(f)), sc$matches, sc$rows, b0, a0))
  tot <- tot + c(sum(grepl(EMAIL_RX, sc$data$remarks, perl = TRUE)), sc$rows, b0, a0,
                 sum(is.na(d$remarks) != is.na(sc$data$remarks)))
}
check(tot[["email_rows"]] == 212L && tot[["emails_left"]] == 0L,
      "212 remark rows carried email addresses and none remain after the scrub")
check(tot[["before"]] > 0L && tot[["before"]] == tot[["after"]],
      sprintf("double-trap markers preserved through the scrub (%d before, %d after)", tot[["before"]], tot[["after"]]))
check(tot[["na_changed"]] == 0L, "scrub leaves NA remarks NA")

# ---- item 1: release confirmation ---------------------------------------------
ok_raw <- list(mam_pertrapnight = data.frame(release = rep("RELEASE-2026", 2)),
               mam_perplotnight = data.frame(release = "RELEASE-2026"),
               `citation_10072_RELEASE-2026` = "cite", check.names = FALSE)
names(ok_raw)[3] <- "citation_10072_RELEASE-2026"
check(isTRUE(check_release(ok_raw)), "pinned release passes")
bad <- ok_raw; bad$mam_pertrapnight$release[2] <- "PROVISIONAL"
check(expect_error(check_release(bad, site = "SRER"), "PROVISIONAL"), "a provisional row stops the build")
bad <- ok_raw; bad[["citation_10072_PROVISIONAL"]] <- "x"
check(expect_error(check_release(bad), "citation"), "an extra citation element stops the build")

# ---- item 7: bundle_meta.json writer -------------------------------------------
m <- bundle_meta("RELEASE-2026", "https://doi.org/10.48443/a83h-tb34", "2026-01-23",
                 "2013-06-04", "2025-11-20", 46L, NA_character_)
f1 <- file.path(tmp, "m1.json"); f2 <- file.path(tmp, "m2.json")
write_json_stable(m, f1); Sys.sleep(1.1); write_json_stable(m, f2)
check(identical(unname(tools::md5sum(f1)), unname(tools::md5sum(f2))), "bundle_meta.json bytes are stable across writes")
txt <- readLines(f1)
check(identical(names(fromJSON(f1)), c("product", "release", "doi", "release_generated", "bundle_schema",
                                       "data_from", "data_through", "sites", "previous_data_through", "advanced")) &&
        any(grepl('"previous_data_through": null', txt)) && !any(grepl("T[0-9]{2}:[0-9]{2}", txt)),
      "bundle_meta.json keys in fixed order, null previous on first run, no timestamps")
check(isTRUE(bundle_meta("R", "d", "g", "a", "2025-12-01", 46L, "2025-11-20")$advanced) &&
        !isTRUE(bundle_meta("R", "d", "g", "a", "2025-11-20", 46L, "2025-11-20")$advanced),
      "advanced is true only when data_through moves past the previous stamp")

# ---- item 8: issue log ----------------------------------------------------------
prod <- fromJSON("scripts/fixtures/products_DP1.10072.001.json", simplifyVector = FALSE)$data
ent <- issue_log_entries(prod)
check(identical(vapply(ent, `[[`, integer(1), "id"), sort(vapply(ent, `[[`, integer(1), "id"))) &&
        all(vapply(ent, function(z) grepl("^\\d{4}-\\d{2}-\\d{2}$", z$issueDate), logical(1))),
      "issue log sorted by id with YYYY-MM-DD dates")
g1 <- file.path(tmp, "i1.json"); g2 <- file.path(tmp, "i2.json")
write_issue_log(ent, g1)
prod_shuffled <- prod; prod_shuffled$changeLogs <- rev(prod$changeLogs)
write_issue_log(issue_log_entries(prod_shuffled), g2)
check(identical(unname(tools::md5sum(g1)), unname(tools::md5sum(g2))), "issue log bytes do not depend on endpoint order")
old <- read_issue_log(g1)[-1]
old[[1]]$resolution <- "edited"
dd <- diff_issue_log(old, ent)
check(length(dd$added) == 1L && dd$added[[1]]$id == ent[[1]]$id && length(dd$changed) == 1L,
      "issue-log diff finds the new entry and the changed entry")
check(length(diff_issue_log(read_issue_log(g1), ent)$added) == 0L, "an unchanged log diffs empty")
ri <- release_info(prod, "RELEASE-2026")
check(identical(ri$doi, "https://doi.org/10.48443/a83h-tb34") && identical(ri$release_generated, "2026-01-23"),
      "pinned release DOI and generation date read from the products endpoint")
check(length(newer_releases(prod, "RELEASE-2026")) == 0L &&
        identical(newer_releases(prod, "RELEASE-2024"), c("RELEASE-2025", "RELEASE-2026")),
      "release watch lists only releases newer than the pin")

# ---- item 5: canonical compare --------------------------------------------------
small <- bart[1:500, ]
check(equivalent_objects(small, small[sample(nrow(small)), ]), "a re-sorted copy counts as unchanged")
chg <- small; chg$weight[which(!is.na(chg$weight))[1]] <- -1L
check(!equivalent_objects(small, chg), "a changed value counts as changed")
check(!equivalent_objects(small, small[-1, ]), "a dropped row counts as changed")

# restore_unchanged in a scratch git repo: reorder and serialization header -> restored
repo <- file.path(tmp, "repo"); dir.create(file.path(repo, "data", "sites"), recursive = TRUE)
lib <- normalizePath("scripts/producer_lib.R")
restore_src <- normalizePath("scripts/restore_unchanged_bundles.R")
old_wd <- setwd(repo)
system2("git", c("init", "-q")); system2("git", c("config", "user.email", "t@example.invalid"))
system2("git", c("config", "user.name", "t"))
saveRDS(small, "data/sites/AAAA.rds", compress = "xz")
saveRDS(small, "data/sites/BBBB.rds", compress = "xz")
system2("git", c("add", "-A")); system2("git", c("commit", "-qm", "base"))
committed <- unname(tools::md5sum("data/sites/AAAA.rds"))
saveRDS(small[rev(seq_len(nrow(small))), ], "data/sites/AAAA.rds", compress = "gzip", version = 2)
saveRDS(chg, "data/sites/BBBB.rds", compress = "xz")
dir.create("scripts"); invisible(file.copy(lib, "scripts/producer_lib.R"))
Sys.setenv(SMT_ROW_DELTA_DIR = file.path(tmp, "report"))
out <- system2(file.path(R.home("bin"), "Rscript"), c("--vanilla", shQuote(restore_src)), stdout = TRUE, stderr = TRUE)
cat(paste0("      ", out, "\n"), sep = "")
check(identical(unname(tools::md5sum("data/sites/AAAA.rds")), committed),
      "reordered copy saved with a different serialization header is restored to committed bytes")
check(system2("git", c("diff", "--quiet", "--", "data/sites/BBBB.rds")) != 0L,
      "a real value change is kept")
rd <- fromJSON(file.path(tmp, "report", "row_delta.json"))
check(rd$counts$restored == 1L && rd$counts$changed == 1L && nrow(rd$sites) == 2L,
      "row-delta JSON counts restored and changed files per site")
setwd(old_wd)

# ---- item 14: env precipitation coverage and order ------------------------------
env_src <- parse("scripts/refresh_env_data.R")
want <- c("pick_col", "pick_table", "precip_monthly", "ENV_COLS", "canonical_env")
env_ns <- new.env()
for (e in env_src) if (is.call(e) && identical(e[[1]], as.name("<-")) && as.character(e[[2]]) %in% want)
  eval(e, env_ns)
days <- seq(as.Date("2024-02-01"), as.Date("2024-03-31"), by = "day")
tb <- tibble(startDateTime = paste(days, "00:00:00"), precipBulk = 1)
tb$precipBulk[format(days, "%Y-%m") == "2024-03"][1:4] <- NA     # March: 27 of 31 valid (< 0.9 x 31)
tb$precipBulk[format(days, "%Y-%m") == "2024-02"][1:2] <- NA     # Feb: 27 of 29 valid (>= 0.9 x 29)
pm <- env_ns$precip_monthly(tb)
check(identical(pm$n_days, c(27L, 27L)) && pm$value[1] == 27 && is.na(pm$value[2]),
      "precip_mm kept at >= 90% valid days and set NA below, with precip_mm_n_days")
sub <- tibble(startDateTime = c("2024-02-01 00:00:00", "2024-02-01 00:30:00", "2024-02-02 00:00:00"),
              priPrecipBulk = c(0.5, NA, 0))
check(env_ns$precip_monthly(sub)$n_days == 2L, "sub-daily rows count a day valid when any interval has a value")
e1 <- readRDS("data/env/SRER.rds")
ce <- env_ns$canonical_env(e1[sample(nrow(e1)), ])
check(identical(names(ce)[1:5], c("siteID", "ym", "date", "precip_mm", "precip_mm_n_days")) &&
        !is.unsorted(ce$ym) && all(is.na(ce$precip_mm_n_days)),
      "env bundles sort by ym, carry precip_mm_n_days (NA for history months) in fixed column order")
ea <- file.path(tmp, "ea.rds"); eb <- file.path(tmp, "eb.rds")
saveRDS(env_ns$canonical_env(e1[sample(nrow(e1)), ]), ea, compress = "xz")
saveRDS(env_ns$canonical_env(e1[sample(nrow(e1)), ]), eb, compress = "xz")
check(identical(unname(tools::md5sum(ea)), unname(tools::md5sum(eb))), "env bundle bytes do not depend on row order")

cat(sprintf("\nAll %d producer checks passed.\n", passed))
