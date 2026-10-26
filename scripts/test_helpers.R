#!/usr/bin/env Rscript

# Fail-closed fixture tests for the pure scientific contracts in R/helpers.R.
# These tests use no network. All but the closing closed-capture regression block
# use no committed production bundle (that block reads SRER, HARV and ABBY once
# each, as the before/after record for the 2026-10-18 estimator fix). Run from the
# repository root with `Rscript scripts/test_helpers.R` in the pinned app runtime.

args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grepl("^--file=", args)])
root <- if (length(file_arg)) normalizePath(file.path(dirname(file_arg), "..")) else
  normalizePath(".")
source(file.path(root, "R", "helpers.R"), chdir = TRUE)

passed <- 0L
check <- function(ok, label) {
  if (!isTRUE(ok)) stop(sprintf("FAIL: %s", label), call. = FALSE)
  passed <<- passed + 1L
  cat(sprintf("PASS %02d: %s\n", passed, label))
}
expect_error <- function(expr) {
  inherits(try(force(expr), silent = TRUE), "try-error")
}

expected_tokens <- c(
  "1 - trap not set",
  "2 - trap disturbed/door closed but empty",
  "3 - trap door open or closed w/ spoor left",
  "4 - more than 1 capture in one trap",
  "5 - capture", "6 - trap set and empty")
check(identical(names(MAM_TRAP_STATUS_EFFORT), expected_tokens) &&
        identical(unname(MAM_TRAP_STATUS_EFFORT), c(0, 0.5, 0.5, 1, 1, 1)) &&
        identical(mam_trap_status_effort(expected_tokens),
                  c(0, 0.5, 0.5, 1, 1, 1)),
      "six exact trap-status tokens map to reviewed effort weights")

bad_tokens <- c(
  "1 - trap not set garbage", "3 - trap door open w/ spoor left",
  "4 - >1 capture in one trap", "7 - trap set and empty", "", NA_character_)
check(all(vapply(bad_tokens,
                 function(x) expect_error(mam_trap_status_effort(x)),
                 logical(1))),
      "unknown and non-exact trap-status tokens fail closed")

fixture <- data.frame(
  collectDate = rep("2020-06-01", 8L),
  nightuid = c("MULTI", "MULTI", "DOUBLE", "DOUBLE",
               "PLACEHOLDER", "PLACEHOLDER", "REPEAT", "REPEAT"),
  plotID = rep("TEST_001", 8L),
  trapCoordinate = c("A1", "A1", "B1", "B1", "X10", "X10", "C1", "C2"),
  trapStatus = c(
    "4 - more than 1 capture in one trap", "5 - capture",
    "6 - trap set and empty", "1 - trap not set",
    "5 - capture", "6 - trap set and empty",
    "5 - capture", "5 - capture"),
  tagID = c("M1", "M2", NA, NA, "P1", NA, "REPEAT", "REPEAT"),
  remarks = c(
    NA, NA,
    rep("double trap method (two traps set at each location)", 2L),
    NA, NA, NA, NA),
  stringsAsFactors = FALSE)
resolved <- mam_resolve_effort_rows(
  fixture, rep(2020L, nrow(fixture)), "mammal fixture")
event_rules <- unique(resolved[c("trap_event", "trap_effort_rule")])
rule_counts <- table(event_rules$trap_effort_rule)
check(nrow(resolved) == nrow(fixture) &&
        sum(resolved$trap_effort) == 6 &&
        unname(rule_counts["canonical-multi-capture-one-trap"]) == 1L &&
        unname(rule_counts["reviewed-double-trap-rows"]) == 1L &&
        unname(rule_counts["placeholder-row-level"]) == 2L &&
        unname(rule_counts["canonical-single"]) == 2L,
      "resolver separates multi-capture, double-trap, placeholder, and singleton events")

multi <- resolved$trap_effort_rule == "canonical-multi-capture-one-trap"
check(identical(resolved$trap_effort[multi], c(1, 0)) &&
        identical(resolved$trap_effort_owner[multi], c(TRUE, FALSE)) &&
        all(resolved$trap_event_source_rows[multi] == 2L),
      "one-trap multi-capture keeps both animal rows but allocates effort once")

double <- resolved$trap_effort_rule == "reviewed-double-trap-rows"
check(identical(resolved$trap_effort[double], c(1, 0)) &&
        all(resolved$trap_effort_owner[double]) &&
        all(resolved$trap_event_source_rows[double] == 2L),
      "reviewed double-trap rows retain their individual status weights")

check(sum(mam_tag_present(fixture$tagID)) == 5L &&
        length(unique(trimws(fixture$tagID[mam_tag_present(fixture$tagID)]))) == 4L &&
        !mam_tag_present(NA_character_) && !mam_tag_present("  "),
      "capture identity requires a nonblank tag and preserves repeated tags at distinct events")

check(identical(MAM_GRID_COORDINATE_RE, "^[A-J](?:[1-9]|10)$") &&
        identical(MAM_PLACEHOLDER_COORDINATE_RE,
                  "^(?:[A-J]X|X(?:[1-9]|10)|XX)$") &&
        identical(MAM_REVIEWED_MULTI_TRAP_MARKERS, c(
          "trap accidentally double set",
          "double trap method (two traps set at each location)")) &&
        identical(unname(mam_multi_trap_marker(c(
          "TRAP ACCIDENTALLY DOUBLE SET after deployment",
          "double trap method (two traps set at each location) were used",
          "trap double set"))), c(1L, 2L, 0L)),
      "coordinate grammar and two reviewed remark markers are exact")

duplicate_fixture <- function(status, tag, remarks = NA_character_,
                              coordinate = "A1") {
  n <- length(status)
  data.frame(
    collectDate = rep("2020-06-01", n),
    nightuid = rep("AMBIGUOUS", n), plotID = rep("TEST_001", n),
    trapCoordinate = rep(coordinate, n), trapStatus = status,
    tagID = tag, remarks = rep(remarks, length.out = n),
    stringsAsFactors = FALSE)
}

ambiguous <- list(
  duplicate_fixture(rep("5 - capture", 2L), c("A", "B")),
  duplicate_fixture(c("6 - trap set and empty", "1 - trap not set"), c(NA, NA)),
  duplicate_fixture(rep("4 - more than 1 capture in one trap", 2L), c("A", "A")),
  duplicate_fixture(c("4 - more than 1 capture in one trap", "5 - capture"), c("A", NA)),
  duplicate_fixture(c("4 - more than 1 capture in one trap", "5 - capture"), c("A", "B"),
                    "trap accidentally double set"),
  duplicate_fixture(rep("5 - capture", 3L), c("A", "B", "C"),
                    "trap accidentally double set"),
  duplicate_fixture(rep("5 - capture", 2L), c("A", "A"),
                    "trap accidentally double set"),
  within(duplicate_fixture(
    rep("4 - more than 1 capture in one trap", 2L), c("A", "B")), {
      collectDate <- c("2020-06-01", "2020-06-02")
    }),
  duplicate_fixture("5 - capture", "A", coordinate = "K1"),
  duplicate_fixture("5 - capture", "A", coordinate = "a1"),
  duplicate_fixture(rep("5 - capture", 2L), c("A", "B"), "trap double set"))
check(all(vapply(ambiguous, function(x)
  expect_error(mam_resolve_effort_rows(
    x, rep(2020L, nrow(x)), "ambiguous mammal fixture")), logical(1))),
  "unreviewed duplicate, key, coordinate, and marker ambiguity fails closed")

missing_key <- fixture
missing_key$nightuid[1] <- NA_character_
check(expect_error(mam_resolve_effort_rows(
        missing_key, rep(2020L, nrow(missing_key)), "missing-key fixture")) &&
      expect_error(mam_resolve_effort_rows(
        fixture, rep(NA_integer_, nrow(fixture)), "missing-year fixture")),
      "incomplete event keys and years fail closed")

# Add the non-effort columns clean_mam() expects. It is allowed to fill optional
# app columns with NA, but the seven effort-contract columns above must resolve.
fixture$siteID <- "TEST"
fixture$scientificName <- c(
  "Species alpha", "Species beta", NA, NA,
  "Species alpha", NA, "Species alpha", "Species alpha")
clean <- clean_mam(fixture)
check(nrow(clean) == 8L && sum(clean$trap_effort) == 6 &&
        sum(clean$is_capture) == 5L &&
        sum(clean$trap_effort_rule == "canonical-multi-capture-one-trap") == 2L &&
        abs(100 * sum(clean$is_capture) / sum(clean$trap_effort) - 83.3333333333333) < 1e-12,
      "clean_mam publishes event-audited effort and reproducible CPUE support")

alpha <- mnka_series(clean, scientific_name = "Species alpha")
beta <- mnka_series(clean, scientific_name = "Species beta")
check(nrow(alpha) == 1L && nrow(beta) == 1L &&
        alpha$trap_nights[[1L]] == 6 && beta$trap_nights[[1L]] == 6 &&
        alpha$captures[[1L]] == 4L && beta$captures[[1L]] == 1L &&
        alpha$cpue[[1L]] == round(100 * 4 / 6, 1) &&
        beta$cpue[[1L]] == round(100 * 1 / 6, 1),
      "species MNKA/CPUE uses species captures with full opportunity-complete effort")


# ---- closed-capture estimator: real-tag marks, deaths, unmarked add-on -------
# NEON SOP C.8 placeholder IDs are capture events, not animals.
tag_cases <- c("NEON.MAM.D14.O007A520160628", "NEON.MAM.D01.O001F1",
               "NEON.MAM.D14.L2669", "OL1001", NA)
check(identical(mam_is_placeholder_tag(tag_cases), c(TRUE, TRUE, FALSE, FALSE, FALSE)) &&
        identical(mam_tag_marked(tag_cases), c(FALSE, FALSE, TRUE, TRUE, FALSE)) &&
        !any(mam_is_placeholder_tag(c("NEON.MAM.D01.OL1001", "NEON.MAM.D01.OR22", ""))),
      "placeholder regex matches SOP C.8 IDs (long and short form) and never a real ear tag")

cc_rows <- function(plot, dates, tags, fate = "processed", recapture = "N") {
  dates <- as.Date(dates)
  data.frame(plotID = plot, date = dates, tagID = tags, ym = format(dates, "%Y-%m"),
             fate = fate, recapture = recapture, stringsAsFactors = FALSE)
}
# One 3-night bout. Real tags: A (n1,n2), B (n1,n3), D (n1,n2,n3), C/E/F/G new on
# n2, H/I new on n3, X caught n1 and dies. Hand-worked real-tag statistics:
#   n1 C=4 U=4 D=1 M=0 | n2 C=6 U=4 R=2 M=3 | n3 C=4 U=2 R=2 M=7
#   sumC=14 sumR=4 num=6*3+4*7=46 N_marked=11.5 mnka_marked=10
# (with X alive, M would be 4 and 8 and num 56).
cc_bout <- function(plot = "TEST_001", shift = 0, sfx = "") {
  n <- as.Date(c("2020-06-01", "2020-06-02", "2020-06-03")) + shift
  real <- rbind(
    cc_rows(plot, n[c(1, 1, 1)], paste0(c("A", "B", "D"), sfx)),
    cc_rows(plot, n[1], paste0("X", sfx), fate = "dead"),
    cc_rows(plot, n[c(2, 2, 2, 2, 2, 2)], paste0(c("A", "D", "C", "E", "F", "G"), sfx), recapture = "U"),
    cc_rows(plot, n[c(3, 3, 3, 3)], paste0(c("B", "D", "H", "I"), sfx), recapture = "Y"))
  ph <- rbind(
    cc_rows(plot, n[1], sprintf("NEON.MAM.D14.O001A1%s", format(n[1], "%Y%m%d"))),
    cc_rows(plot, n[2], sprintf("NEON.MAM.D14.O001B2%s", format(n[2], "%Y%m%d")), recapture = "U"),
    cc_rows(plot, n[3], sprintf("NEON.MAM.D14.O001C3%s", format(n[3], "%Y%m%d")), recapture = "Y"))
  list(real = real, all = rbind(real, ph))
}
fx <- cc_bout()
b1 <- bout_closed_capture(fx$all)
b0 <- bout_closed_capture(fx$real)
check(nrow(b1) == 1L && b1$k == 3L && b1$sumR == 4L && b1$sumC == 14L &&
        b1$num == 46 && b1$N_marked == 11.5 && b1$mnka_marked == 10L &&
        b1$n_dead == 1L && b1$status == "low-precision",
      "Schnabel bout counts only real-tag recaptures and removes the dead animal from M")
check(b1$n_unmarked == 2L && b1$n_unmarked_recap == 1L &&
        b1$N == b1$N_marked + 2 && b1$mnka == b1$mnka_marked + 2 &&
        b1$p == round(b1$sumC / (b1$k * b1$N_marked), 3),
      "untagged captures add once as known alive (recapture Y adds nothing); p-hat uses N_marked")
pl <- recap_poisson_limits(4)
check(b1$lo >= b1$mnka && is.finite(b1$hi) && b1$hi > b1$N &&
        abs(b1$hi - (round(46 / pl[["lo"]], 1) + 2)) < 1e-9,
      "Schnabel bout limits are Krebs Poisson limits on recaptures, floored at MNKA")
check(identical(b0$k, b1$k) && identical(b0$N_marked, b1$N_marked) &&
        identical(b0$p, b1$p) && identical(b0$sumR, b1$sumR) && identical(b0$num, b1$num) &&
        b0$n_unmarked == 0L,
      "placeholder rows never change the marked statistics of a bout")

# Chapman (k = 2): ten caught night 1 (one dies, so M = 9), night 2 recaptures
# four and catches six new: N = (9+1)(10+1)/(4+1) - 1 = 21.
ch <- rbind(cc_rows("TEST_002", "2020-06-10", sprintf("T%02d", 1:9)),
            cc_rows("TEST_002", "2020-06-10", "T10", fate = "dead"),
            cc_rows("TEST_002", "2020-06-11", sprintf("T%02d", c(1:4, 11:16))))
bc <- bout_closed_capture(ch)
check(nrow(bc) == 1L && bc$k == 2L && bc$sumR == 4L && bc$N_marked == 21 &&
        bc$mnka == 16L && bc$lo < bc$N && bc$N < bc$hi && bc$lo >= bc$mnka &&
        abs(bc$hi - round(110 / (pl[["lo"]] + 1) - 1, 1)) < 1e-9,
      "Chapman bout uses M net of deaths and Poisson limits on R")

# Death on a recapture night. Night 1 catches A-H (8 new). A is recaptured on
# night 2 and dies; B and C are recaptured on night 3. M = 0, 8, 7, so
# num = 1*8 + 2*7 = 22 and sumR = 3 (raw N 7.3). Keeping A in M on night 3 would
# give num 24 (raw N 8.0). The raw estimate is below the 8 known alive, so the
# bout is clamped to MNKA.
dd <- rbind(cc_rows("TEST_004", "2020-06-01", LETTERS[1:8]),
            cc_rows("TEST_004", "2020-06-02", "A", fate = "dead"),
            cc_rows("TEST_004", "2020-06-03", c("B", "C")))
bd <- bout_closed_capture(dd)
check(bd$k == 3L && bd$num == 22 && bd$sumR == 3L && round(bd$num / bd$sumR, 1) == 7.3 &&
        bd$n_dead == 1L && bd$N_marked == 8 && bd$status == "detection near-complete",
      "an animal that dies on its recapture night leaves M from the next night")

# High recapture: five animals on all three nights plus one new on night 3 gives
# num = 5*5 + 6*5 = 55, sumR = 10, raw N 5.5 below the 6 known alive.
hr <- rbind(cc_rows("TEST_005", rep(c("2020-06-01", "2020-06-02", "2020-06-03"), each = 5),
                    rep(sprintf("K%d", 1:5), 3)),
            cc_rows("TEST_005", "2020-06-03", "K6"),
            cc_rows("TEST_005", "2020-06-02", "NEON.MAM.D14.O005A120200602"))
bh <- bout_closed_capture(hr)
check(bh$num == 55 && bh$sumR == 10L && bh$status == "detection near-complete" &&
        bh$N_marked == bh$mnka_marked && bh$mnka == 7L && bh$lo == bh$mnka && bh$N == 7,
      "a raw Schnabel N below MNKA is clamped to MNKA, with lo at MNKA after the add-on")

# sumR >= 50 switches Schnabel to normal limits in the 1/N domain: night 1
# catches 100, nights 2 and 3 each recapture 25 and catch 75 new.
night_tags <- list(sprintf("Q%03d", 1:100),
                   c(sprintf("Q%03d", 1:25), sprintf("Q%03d", 101:175)),
                   c(sprintf("Q%03d", 26:50), sprintf("Q%03d", 176:250)))
big <- do.call(rbind, lapply(1:3, function(i)
  cc_rows("TEST_006", as.Date("2020-06-01") + i - 1, night_tags[[i]])))
bb <- bout_closed_capture(big)
v_inv <- 50 / 27500^2
ci_inv <- 1 / (27500 / 50) + c(1, -1) * 1.96 * sqrt(v_inv)
check(bb$sumR == 50L && bb$num == 27500 && bb$N_marked == 550 &&
        abs(bb$lo - round(1 / ci_inv[1], 1)) < 1e-9 && abs(bb$hi - round(1 / ci_inv[2], 1)) < 1e-9 &&
        abs(bb$hi - round(27500 / recap_poisson_limits(50)[["lo"]], 1)) > 1,
      "Schnabel bouts with sumR >= 50 use 1/N-normal limits, not Poisson")

# Roll-up (3-night bouts unless noted): June has TEST_001 twice (two bouts) and
# TEST_002 once; July has TEST_001 alone (one grid instead of two, same nights:
# not a break); August has TEST_003 alone (a different plot: not a break);
# September has a 2-night Chapman bout (modal nights change: a break); October
# has one 3-night and one 2-night bout (a tie goes to the larger k: a break).
shift_ch <- function(days, sfx, plot = "TEST_002") {
  x <- ch; x$plotID <- plot; x$date <- x$date + days
  x$ym <- format(x$date, "%Y-%m"); x$tagID <- paste0(x$tagID, sfx); x
}
roll <- rbind(fx$all, cc_bout(shift = 19, sfx = "2")$all, cc_bout("TEST_002", shift = 9, sfx = "b")$all,
              cc_bout(shift = 30, sfx = "3")$all, cc_bout("TEST_003", shift = 61, sfx = "4")$all,
              shift_ch(92, "s"), cc_bout(shift = 122, sfx = "5")$all, shift_ch(122, "o"))
rb <- bout_closed_capture(roll)
rs <- closed_capture_series(roll, rb)$series
jun <- rs[rs$ym == "2020-06", ]
p1 <- mean(rb$N[rb$plotID == "TEST_001" & rb$ym == "2020-06"])
p2 <- rb$N[rb$plotID == "TEST_002" & rb$ym == "2020-06"]
check(nrow(rs) == 5L && abs(jun$N - mean(c(p1, p2))) < 1e-9 &&
        jun$n_grids == 2L && jun$n_bouts == 3L && jun$lo >= jun$mnka && jun$hi >= jun$N &&
        identical(as.integer(rs$k_mode), c(3L, 3L, 3L, 2L, 3L)) &&
        identical(rs$design_break, c(FALSE, FALSE, FALSE, TRUE, TRUE)) &&
        rs$grids[rs$ym == "2020-08"] == "TEST_003",
      "monthly N-hat is a mean per grid; the line breaks only where modal nights per bout change")
# Chao (1989) log-normal interval for the three-bout June month, by hand
jb <- rb[rb$ym == "2020-06", ]
j1 <- jb[jb$plotID == "TEST_001", ]; j2 <- jb[jb$plotID == "TEST_002", ]
v_j <- (sum(j1$varN) / 4 + j2$varN) / 4
m_j <- (mean(j1$mnka) + j2$mnka) / 2
n_j <- (mean(j1$N) + j2$N) / 2
f0_j <- n_j - m_j
K_j <- exp(1.96 * sqrt(log(1 + v_j / f0_j^2)))
check(f0_j > 0 && v_j > 0 && abs(jun$varN - v_j) < 1e-6 &&
        abs(jun$lo - (m_j + f0_j / K_j)) < 1e-6 && abs(jun$hi - (m_j + f0_j * K_j)) < 1e-6,
      "a multi-bout month uses the Chao log-normal interval exactly")
check(abs(jun$unmarked - (mean(j1$n_unmarked) + j2$n_unmarked) / 2) < 1e-9 &&
        abs(jun$N - (jun$N_marked + jun$unmarked)) < 1e-9,
      "monthly N-hat splits into tagged N_marked plus the untagged add-on")
jul <- rs[rs$ym == "2020-07", ]; bj <- rb[rb$ym == "2020-07", ]
aug <- rs[rs$ym == "2020-08", ]; ba <- rb[rb$ym == "2020-08", ]
check(nrow(bj) == 1L && identical(jul$lo, bj$lo) && identical(jul$hi, bj$hi) &&
        identical(aug$lo, ba$lo) && identical(aug$hi, ba$hi),
      "a single-bout month keeps that bout's own lo and hi exactly")

# ---- regression on committed bundles (before/after record) ------------------
# docs/audit-fixes/2026-10-18-closed-capture-before-after.md. Each bundle is
# loaded once.
reg_site <- function(site) {
  d <- clean_mam(readRDS(file.path(root, "data", "sites", paste0(site, ".rds"))))
  b <- bout_closed_capture(d)
  list(d = d, b = b, cc = closed_capture_series(d, b))
}
near <- function(x, target, rel) is.finite(x) && abs(x - target) <= rel * abs(target)
est_sum <- function(r) {
  e <- r$b[r$b$status %in% c("ok", "low-precision", "detection near-complete") & is.finite(r$b$N), ]
  sum(e$N)
}
harv <- reg_site("HARV")
h3 <- harv$b[harv$b$boutID == "HARV_001#3", ]
check(nrow(h3) == 1L && near(h3$N, 42.67, 0.10),
      sprintf("HARV_001#3 N-hat on real tags is about 43 (was about 116), got %.1f", h3$N))
# Recorded on the RELEASE-2026 schema-2 bundles (refresh run 40346798927): bouts on
# grids NEON marks incomplete (gridCompletion) are held out of the estimates.
check(near(est_sum(harv), 7050, 0.02) && near(harv$cc$mean_detect, 0.827, 0.005) &&
        near(harv$cc$mean_p, 0.450, 0.005) && harv$cc$n_unmarked_total > 0 && harv$cc$n_bouts == 372L &&
        harv$cc$n_incomplete == 5L &&
        sum(harv$b$sumC == 0 & harv$b$n_unmarked == 0 & harv$b$n_unmarked_recap == 0) == 15L,
      "HARV sum of bout N-hat, completeness and pooled p-hat match the recorded values")
srer <- reg_site("SRER")
check(sum(mam_is_placeholder_tag(srer$d$tagID)) == 413L &&
        sum(mam_tag_present(srer$d$tagID)) == 9962L &&
        near(est_sum(srer), 4669, 0.02) && near(srer$cc$mean_detect, 0.857, 0.005) &&
        near(srer$cc$mean_p, 0.494, 0.005) && srer$cc$n_bouts == 229L && srer$cc$n_incomplete == 9L &&
        community_stats(srer$d)$individuals == 3563L,
      "SRER placeholder count, trapped-night bouts, real-tag estimates and ear-tagged individuals match the recorded values")
abby <- reg_site("ABBY")
s_all <- rbind(harv$cc$series[, c("N", "lo", "hi", "mnka")],
               srer$cc$series[, c("N", "lo", "hi", "mnka")],
               abby$cc$series[, c("N", "lo", "hi", "mnka")])
check(near(est_sum(abby), 2599, 0.02) && near(abby$cc$mean_p, 0.453, 0.005) &&
        all(s_all$lo <= s_all$N & s_all$N <= s_all$hi & s_all$lo >= s_all$mnka) &&
        !anyNA(s_all),
      "ABBY sum matches and every monthly interval brackets N-hat above MNKA")
# ---- flag_repro(): exact, NA-safe, case-insensitive SOP token matching ------
# NEON.DOC.000481vQ tokens. Substring matching once read "not pregnant" as
# pregnant, "nonenlarged" as enlarged and "not plugged" as plugged, and missed
# the estrous token "swollen" -- so nearly every adult female scored as
# reproductive. Each row below pins one token to its label.
repro_fx <- data.frame(
  testes          = c("nonscrotal",   NA,        NA,  "scrotal", NA,         NA,          NA,        NA,       NA,     "SCROTAL", NA),
  pregnancyStatus = c("not pregnant", "not",     NA,  NA,        "pregnant", NA,          NA,        NA,       NA,     NA,        ""),
  nipples         = c("nonenlarged",  NA,        NA,  NA,        NA,         "enlarged",  NA,        NA,       NA,     NA,        "unknown"),
  vagina          = c("not plugged",  "neither", NA,  NA,        NA,         NA,          "swollen", "plugged", "both", NA,        "Swollen "),
  stringsAsFactors = FALSE)
repro_out <- flag_repro(repro_fx)$repro
check(identical(repro_out[1:3], rep("non-reproductive", 3)),
      "flag_repro: not pregnant / not / nonenlarged / not plugged / neither / all-NA stay non-reproductive")
check(identical(repro_out[4], "breeding male") && identical(repro_out[10], "breeding male"),
      "flag_repro: testes scrotal (any case) is a breeding male; nonscrotal is not")
check(identical(repro_out[5], "pregnant female"),
      "flag_repro: pregnancyStatus pregnant is a pregnant female")
check(identical(repro_out[6:9], rep("lactating/receptive female", 4)),
      "flag_repro: nipples enlarged and vagina swollen/plugged/both are lactating/receptive")
check(identical(repro_out[11], "lactating/receptive female"),
      "flag_repro: mixed-case, padded 'Swollen ' still matches the estrous token")
repro_zero <- flag_repro(repro_fx[0, , drop = FALSE])
check(is.data.frame(repro_zero) && nrow(repro_zero) == 0L && "repro" %in% names(repro_zero),
      "flag_repro: a zero-row frame returns zero rows with a repro column")
repro_nocol <- flag_repro(data.frame(tagID = c("a", "b"), stringsAsFactors = FALSE))
check(identical(repro_nocol$repro, rep("non-reproductive", 2)),
      "flag_repro: frames missing the reproductive columns fall through to non-reproductive")

# ---- individual_qc_flags(): unknown/NA life stages must not crash the card --
# A named-vector lookup on "unknown"/NA produced NA names, which data.frame()
# took as row names ("row names contain missing values").
qc_hist <- function(stage) data.frame(
  date = as.Date(c("2020-03-01", "2020-04-01", "2020-05-01")),
  plotID = "SRER_001", trapCoordinate = c("A1", "A2", "A1"),
  lifeStage = stage, sex = "F", scientificName = "Chaetodipus penicillatus",
  weight = c(14, 15, 15), hindfootLength = c(22, 22, 22), tailLength = NA_real_,
  earLength = NA_real_, totalLength = NA_real_, recapture = c("N", "Y", "Y"),
  fate = "released", stringsAsFactors = FALSE)
qc_row <- data.frame(tagID = "T1", spatial_conflict = FALSE, career_days = 61,
                     id_uncertain = FALSE, stringsAsFactors = FALSE)
qc_a <- try(individual_qc_flags(qc_hist(c("subadult", "unknown", "adult")), qc_row), silent = TRUE)
check(is.list(qc_a) && !inherits(qc_a, "try-error"),
      "individual_qc_flags: an 'unknown' life stage in the history does not error")
qc_b <- try(individual_qc_flags(qc_hist(c(NA, "adult", "adult")), qc_row), silent = TRUE)
check(is.list(qc_b) && !inherits(qc_b, "try-error"),
      "individual_qc_flags: an NA life stage in the history does not error")


# ---- k counts trapped nights, not nights with a tagged capture (2026-10-20) --
# Fixtures go through clean_mam() so trap_effort comes from the effort contract.
# Night 1 marks KA-KF (6); night 2 has only an empty-but-set trap; night 3
# recaptures KA-KD and marks KG. Schnabel (k = 3): M = 0, 6, 6 and
# num = 5 * 6 = 30, sumR = 4, N = 7.5. Chapman (k = 2): (6+1)(5+1)/(4+1) - 1 = 7.4.
k_fixture <- function(night2_status, plot = "TEST_K01") {
  n <- c("2021-07-01", "2021-07-02", "2021-07-03")
  tags <- c(sprintf("K%s", LETTERS[1:6]), NA, sprintf("K%s", LETTERS[c(1:4, 7)]))
  nights <- c(rep(n[1], 6), n[2], rep(n[3], 5))
  status <- c(rep("5 - capture", 6), night2_status, rep("5 - capture", 5))
  clean_mam(data.frame(
    collectDate = nights, nightuid = paste0("N", match(nights, n)),
    plotID = plot, trapCoordinate = c(sprintf("A%d", 1:6), "B1", sprintf("A%d", 1:5)),
    trapStatus = status, tagID = tags, remarks = NA_character_, siteID = "TEST",
    scientificName = ifelse(is.na(tags), NA, "Species alpha"),
    recapture = ifelse(nights == n[3] & !is.na(tags) & tags != "KG", "Y", "N"),
    fate = ifelse(is.na(tags), NA, "released"), stringsAsFactors = FALSE))
}
ke <- bout_closed_capture(k_fixture("6 - trap set and empty"))
check(nrow(ke) == 1L && ke$k == 3L && ke$sumR == 4L && ke$num == 30 &&
        ke$N_marked == round(30 / 4, 1) && ke$N == 7.5 && ke$mnka_marked == 7L &&
        ke$p == round(ke$sumC / (3 * ke$N_marked), 3),
      "an empty-but-set middle night counts in k: the bout is Schnabel (k = 3), not Chapman")
kn <- bout_closed_capture(k_fixture("1 - trap not set"))
check(nrow(kn) == 1L && kn$k == 2L && kn$sumR == 4L &&
        kn$N_marked == round((6 + 1) * (5 + 1) / (4 + 1) - 1, 1),
      "a not-set middle night (effort 0) does not count: the bout is Chapman (k = 2)")
# A bout that trapped three nights and caught nothing stays in the table.
z_rows <- data.frame(
  collectDate = c("2021-08-01", "2021-08-02", "2021-08-03"), nightuid = c("Z1", "Z2", "Z3"),
  plotID = "TEST_K02", trapCoordinate = "A1", trapStatus = "6 - trap set and empty",
  tagID = NA_character_, remarks = NA_character_, siteID = "TEST",
  scientificName = NA_character_, stringsAsFactors = FALSE)
zd <- clean_mam(z_rows)
zb <- bout_closed_capture(zd)
zs <- tryCatch(closed_capture_series(zd, zb), error = function(e) e)
check(nrow(zb) == 1L && zb$k == 3L && zb$sumC == 0L && zb$mnka_marked == 0L &&
        zb$n_unmarked == 0L && zb$status == "insufficient recaptures" && is.na(zb$N) &&
        !inherits(zs, "error") && zs$n_estimable == 0 && zs$n_bouts == 1L,
      "a zero-capture trapped bout is kept (k = 3, sumC = 0) and the roll-up tolerates it")
# A capture on a plot-night with no effort breaks the effort contract and stops.
kb <- k_fixture("6 - trap set and empty")
kb$trap_effort[kb$date == as.Date("2021-07-03")] <- 0
check(expect_error(bout_closed_capture(kb)),
      "a capture on a night with no trap effort stops instead of joining no bout")

# ---- individuals count real ear tags only (2026-10-20) -----------------------
# Two marked tags each caught twice plus three placeholder capture events of one
# species; a second species appears only as one placeholder row.
ph_tag <- function(coord, day) sprintf("NEON.MAM.D14.O001%s202108%02d", coord, day)
mi_raw <- data.frame(
  collectDate = c("2021-08-01", "2021-08-01", "2021-08-02", "2021-08-02",
                  "2021-08-01", "2021-08-02", "2021-08-02", "2021-08-02"),
  nightuid = c("M1", "M1", "M2", "M2", "M1", "M2", "M2", "M2"),
  plotID = "TEST_M01",
  trapCoordinate = c("A1", "A2", "A1", "A2", "A3", "A3", "A4", "A5"),
  trapStatus = "5 - capture",
  tagID = c("RA", "RB", "RA", "RB", ph_tag("A3", 1), ph_tag("A3", 2), ph_tag("A4", 2), ph_tag("A5", 2)),
  remarks = NA_character_, siteID = "TEST",
  scientificName = c(rep("Species gamma", 7), "Species delta"),
  recapture = c("N", "N", "Y", "Y", "N", "N", "N", "N"),
  stringsAsFactors = FALSE)
mi7 <- clean_mam(mi_raw[1:7, ])
cs7 <- community_stats(mi7)
ss7 <- species_summary(mi7)
lb7 <- build_leaderboard(mi7)
check(cs7$individuals == 2L && cs7$untagged_captures == 3L && cs7$total_captures == 7L &&
        nrow(ss7) == 1L && ss7$individuals == 2L && ss7$untagged_captures == 3L &&
        ss7$captures == 7L && nrow(lb7) == 2L && setequal(lb7$tagID, c("RA", "RB")),
      "individuals, species individuals and the leaderboard count real ear tags only")
mi8 <- clean_mam(mi_raw)
sa8 <- species_accum(mi8)
hn8 <- hill_numbers(mi8)
cs8 <- community_stats(mi8)
check(sa8$sobs == 2L && sa8$f1 == 0L && sa8$f2 == 1L && cs8$species == 2L &&
        cs8$individuals == 2L && cs8$untagged_captures == 4L &&
        hn8$n_ind == 2L && hn8$q0 == 1,
      "Chao1 f1/f2 and Hill abundance ignore placeholders; richness keeps a placeholder-only species")

# ==== 2026-10-22 science finish ==============================================
source(file.path(root, "R", "seasonal_env.R"), chdir = TRUE)
sf_raw <- function(dates, tags, stage = "adult", sp = "Species alpha", plot = "TEST_S01",
                   coord = NULL, extra = list()) {
  n <- length(dates)
  df <- data.frame(collectDate = dates, nightuid = paste0("N", seq_len(n)), plotID = plot,
                   trapCoordinate = if (is.null(coord)) sprintf("A%d", ((seq_len(n) - 1) %% 10) + 1) else coord,
                   trapStatus = "5 - capture", tagID = tags, remarks = NA_character_, siteID = "TEST",
                   scientificName = sp, lifeStage = stage, recapture = "N", fate = "released",
                   stringsAsFactors = FALSE)
  for (k in names(extra)) df[[k]] <- extra[[k]]
  df
}

# item 1: first_stage is the stage at the earliest capture, not the modal stage
fs <- clean_mam(sf_raw(c("2021-08-10", "2021-06-01", "2021-09-01", "2021-07-01"), "F1",
                       stage = c("adult", "juvenile", "adult", "adult")))
fl <- build_leaderboard(fs)
check(fl$first_stage == "juvenile" && fl$lifeStage == "adult" && fl$age_offset_days == 30 &&
        !fl$age_is_minimum,
      "leaderboard first_stage is the earliest capture's stage (unsorted rows, modal stage adult)")

# item 2: stage regression tiers
sr <- function(st) individual_qc_flags(qc_hist(st), qc_row)
lv <- function(fl, code) vapply(Filter(function(f) identical(f$code, code), fl), `[[`, "", "level")
f_as <- sr(c("adult", "subadult", "adult")); f_aj <- sr(c("adult", "juvenile", "juvenile"))
f_2x <- sr(c("adult", "subadult", "subadult"))
f_two <- individual_qc_flags(transform(rbind(qc_hist(c("adult", "subadult", "adult")),
                                             qc_hist(c("subadult", "subadult", "subadult"))[1, ]),
                                       date = as.Date(c("2020-03-01", "2020-04-01", "2020-05-01", "2020-06-01"))), qc_row)
check(identical(lv(f_as, "stage_regression"), "warn") && identical(lv(f_2x, "stage_regression"), "warn") &&
        identical(lv(f_aj, "stage_regression"), "high") && identical(lv(f_two, "stage_regression"), "high") &&
        grepl("field judgment", f_as[[1]]$text) &&
        any(grepl("An animal cannot get younger, so one of these records is wrong.", vapply(f_aj, `[[`, "", "text"), fixed = TRUE)) &&
        !any(grepl("biologically impossible", c(vapply(f_aj, `[[`, "", "text"), vapply(f_as, `[[`, "", "text")))),
      "stage regression: one adult-to-subadult step is warn; two levels or two regressions are high")

# item 3: weight jump skips reproductive pairs, exact tokens only
wj <- function(w, preg = NA, nip = NA) {
  h <- qc_hist(c("adult", "adult", "adult"))[1:2, ]
  h$date <- as.Date(c("2020-03-01", "2020-03-11")); h$weight <- w
  h$pregnancyStatus <- preg; h$nipples <- nip; h
}
has_wj <- function(h) length(lv(individual_qc_flags(h, qc_row), "weight_jump")) > 0
check(has_wj(wj(c(20, 28))) && !has_wj(wj(c(20, 28), preg = c("not", "pregnant"))) &&
        !has_wj(wj(c(20, 28), nip = c("enlarged", "nonenlarged"))) &&
        has_wj(wj(c(20, 28), nip = c("nonenlarged", "nonenlarged"))) &&
        has_wj(wj(c(20, 28), preg = c("not pregnant", "not pregnant"))) &&
        !has_wj(wj(c(30, 20), preg = c("pregnant", "not"))) &&
        has_wj(wj(c(30, 20), preg = c("not", "pregnant"))),
      "adult weight jump: gain with a reproductive capture or loss after pregnancy is skipped (exact tokens)")

# item 4: cf./aff. genus and family are not species-level; cf. species is
qf <- data.frame(scientificName = c("A b", "A c", "D e", "F g", "H i", "J sp."),
                 taxonRank = c("species", "species", "species", "species", "species", "genus"),
                 identificationQualifier = c(NA, "cf. species", "cf. genus", "cf. family", "aff. genus", NA),
                 stringsAsFactors = FALSE)
check(identical(species_level_only(qf)$scientificName, c("A b", "A c")) &&
        identical(species_confirmed_only(qf)$scientificName, "A b") &&
        identical(species_level_only(qf[, 1:2])$scientificName, c("A b", "A c", "D e", "F g", "H i")),
      "species_level_only drops cf./aff. genus and cf. family; species_confirmed_only also drops cf. species")
cq <- clean_mam(sf_raw(c("2021-06-01", "2021-06-02", "2021-06-03"), c("Q1", "Q2", "Q3"),
                       sp = c("A b", "A c", "D e"),
                       extra = list(identificationQualifier = c(NA, "cf. species", "cf. genus"))))
cqs <- community_stats(cq)
check(cqs$species == 2L && cqs$species_confirmed == 1L,
      "community_stats: species counts cf. species, species_confirmed does not")

# item 5: seeded shuffles, reproducible, RNG untouched, k = 1 works
acc <- clean_mam(sf_raw(sprintf("2021-%02d-01", 1:8), sprintf("S%d", 1:8),
                        sp = c("A a", "A b", "A a", "A c", "A b", "A d", "A a", "A e")))
set.seed(99); before <- .Random.seed
a1 <- species_accum(acc); a2 <- species_accum(acc)
check(identical(a1$curve, a2$curve) && identical(.Random.seed, before) &&
        a1$curve$richness[8] == 5 && all(a1$curve$lo <= a1$curve$richness),
      "species_accum band is reproducible and leaves the session RNG state unchanged")
rm(".Random.seed", envir = .GlobalEnv)
a3 <- species_accum(acc)
check(!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE),
      "species_accum does not create an RNG state when none existed")
a_k1 <- species_accum(clean_mam(sf_raw("2021-06-01", "S1")))
check(nrow(a_k1$curve) == 1L && a_k1$curve$richness == 1 && a_k1$sobs == 1L,
      "species_accum works with a single bout (k = 1)")

# item 16: Chao1 interval at f2 = 0 (hand computed). Species A has 3 marked
# animals, B, C, D one each: sobs 4, f1 3, f2 0, Chao1 = 4 + 3*2/2 = 7.
# var = 3*2/2 + 3*(2*3-1)^2/4 - 3^4/(4*7) = 3 + 18.75 - 2.892857 = 18.857143
# K = exp(1.96*sqrt(log(1 + 18.857143/9))) = 8.0317; lo = 4 + 3/K = 4.37,
# hi = 4 + 3K = 28.10 -> chao_lo 4, chao_hi 29 (the old form gave 26).
c0 <- clean_mam(sf_raw(sprintf("2021-06-%02d", 1:6), sprintf("C%d", 1:6),
                       sp = c("A a", "A a", "A a", "B b", "C c", "D d")))
s0 <- species_accum(c0)
Kh <- exp(1.96 * sqrt(log(1 + (3 + 18.75 - 81 / 28) / 9)))
check(s0$f1 == 3L && s0$f2 == 0L && s0$chao1 == 7 && s0$chao_lo == round(4 + 3 / Kh) &&
        s0$chao_hi == ceiling(4 + 3 * Kh) && s0$chao_hi == 29 && s0$unstable,
      "Chao1 f2 = 0 interval uses the bias-corrected variance (hand-computed 4 to 29)")

# item 6: multi-species ID counts species-level names; cross-genus is warn
ms <- clean_mam(sf_raw(c("2021-06-01", "2021-06-02", "2021-06-03", "2021-06-04"),
                       c("M1", "M1", "M2", "M2"), sp = c("A a", "A sp.", "A a", "B b")))
mlb <- build_leaderboard(ms)
check(mlb$n_species_ids[mlb$tagID == "M1"] == 1L && !mlb$id_uncertain[mlb$tagID == "M1"] &&
        mlb$n_species_ids[mlb$tagID == "M2"] == 2L && mlb$n_genus_ids[mlb$tagID == "M2"] == 2L,
      "n_species_ids ignores genus-only names; n_genus_ids counts genera")
mrow <- function(ng) data.frame(tagID = "T1", spatial_conflict = FALSE, career_days = 61,
                                id_uncertain = TRUE, n_genus_ids = ng, stringsAsFactors = FALSE)
check(identical(lv(individual_qc_flags(qc_hist("adult"), mrow(2L)), "multi_species_id"), "warn") &&
        identical(lv(individual_qc_flags(qc_hist("adult"), mrow(1L)), "multi_species_id"), "info"),
      "multi-species flag: cross-genus is warn, within-genus stays info")

# item 7: QC report, alias, per-capture column
qd <- clean_mam(sf_raw(sprintf("2021-%02d-01", c(3, 4, 5, 6, 7, 3, 4)),
                       c("Q1", "Q1", "Q1", "Q2", "Q2", "Q3", "Q3"),
                       stage = c("adult", "juvenile", "adult", "adult", "adult", "adult", "adult"),
                       extra = list(weight = c(20, 20, 21, 20, 21, 20, 200),
                                    sex = c("F", "F", "F", "M", "F", "F", "F"))))
qr <- site_qc_report(qd)
check(identical(names(qr), c("flag", "level", "tagID", "date", "plotID", "scientificName",
                             "measure", "value", "rule")) &&
        all(qr$level %in% c("high", "warn", "info")) &&
        any(qr$flag == "stage_regression" & qr$tagID == "Q1" & qr$level == "high") &&
        any(qr$flag == "sex_flip" & qr$tagID == "Q2") &&
        any(qr$flag == "measurement_outlier" & qr$tagID == "Q3" & qr$value == "200") &&
        identical(mammal_qc, individual_qc_flags),
      "site_qc_report: one row per flagged record, levels high/warn/info, three sources")
qh <- individual_history(qd, "Q1")
qh <- capture_qc_flags(qh, individual_qc_flags(qh, build_leaderboard(qd)[1, ]), qd)
qh3 <- individual_history(qd, "Q3")
qh3 <- capture_qc_flags(qh3, list(), qd)
check(identical(qh$qc_flags, c("stage_regression (high)", "stage_regression (high)", "")) &&
        identical(qh3$qc_flags, c("", "measurement_outlier_weight (warn)")) &&
        all(c("pregnancyStatus", "nipples") %in% names(qh)),
      "capture_qc_flags writes per-capture flags; history carries reproductive columns")

# item 8: observed statistic is the unrounded best |r| of the same scan
if (!exists("ENV_LAYERS")) ENV_LAYERS <- list(precip = list(col = "precip_mm", label = "Precipitation",
                                                            unit = "mm/mo", color = "#2f7fb5"))
cd <- seq(as.Date("2016-01-01"), by = "month", length.out = 36)
set.seed(5); yv <- rnorm(36); xv <- 0.5 * yv + rnorm(36)
ev_fx <- data.frame(date = cd, precip_mm = xv)
for (nm in setdiff(vapply(ENV_LAYERS, `[[`, "", "col"), "precip_mm")) ev_fx[[nm]] <- NA_real_
st <- .Random.seed
ep <- env_corr_pvalue(NULL, ev_fx, max_lag = 0, nperm = 50, series = data.frame(date = cd, cpue = yv))
ds <- function(v) v - as.numeric(tapply(v, as.integer(format(cd, "%m")), mean)[as.character(as.integer(format(cd, "%m")))])
check(abs(ep$r_obs - stats::cor(ds(yv), ds(xv))) < 1e-12 && ep$r_obs != round(ep$r_obs, 2) &&
        is.finite(ep$p) && identical(.Random.seed, st),
      "env_corr_pvalue compares the null with the unrounded observed r and restores the RNG")

# item 9: month-adjusted annual index
ai_d <- clean_mam(sf_raw(c("2020-05-01", "2020-06-01", "2021-05-01", "2021-05-01"),
                         c("Y1", NA, "Y2", "Y3"),
                         coord = c("A1", "A1", "A1", "A2")))
ai_d$trap_effort[] <- 1
ai <- annual_cpue_index(ai_d, min_months = 1, min_trap_nights = 1)
# plot-months: 2020-05 100, 2020-06 0, 2021-05 100. Month means: May 100, Jun 0.
check(identical(ai$year, c(2020L, 2021L)) && all(ai$index == 0) && identical(ai$value, ai$index) &&
        identical(ai$n_months, c(2L, 1L)) && identical(ai$trap_nights, c(2, 2)) &&
        nrow(annual_cpue_index(ai_d, ok_only = TRUE)) == 0L,
      "annual_cpue_index removes the calendar-month effect and gates ok years")

# item 10: seasonal verdict
v1 <- seasonal_verdict(0.8, 0.01, 7); v2 <- seasonal_verdict(0.4, 0.01, 7)
v3 <- seasonal_verdict(0.9, 0.01, 5); v4 <- seasonal_verdict(0.9, NA, 8); v5 <- seasonal_verdict(0.9, 0.05, 8)
check(v1$rail == "rail-strong" && v1$can_claim && !v1$few_years && v2$rail == "rail-mod" &&
        v3$rail == "rail-weak" && !v3$can_claim && v3$few_years &&
        v4$rail == "rail-weak" && !v4$can_claim && v5$rail == "rail-weak",
      "seasonal_verdict gates on n >= 6 and p < 0.05 before the strength rail")

# item 11: MNKA edges and gap breaks
mnd <- clean_mam(sf_raw(c("2021-01-01", "2021-02-01", "2021-03-01", "2021-09-01"), c("E1", "E1", "E2", "E2")))
mn <- mnka_series(mnd)
gb <- insert_gap_breaks(mn, max_gap_months = 2)
check(identical(mn$edge, c(TRUE, FALSE, FALSE, TRUE)) && !any(mn$incomplete_grid) &&
        nrow(gb) == 5L && is.na(gb$mnka[4]) && gb$date[4] == as.Date("2021-04-01") &&
        gb$plotID[4] == "TEST_S01",
      "mnka_series marks first and last months; insert_gap_breaks splits a 6-month gap")

# item 12 / 15: n_clamped, gridCompletion plumbing (made-up tokens via options)
srer_nogc <- srer$d; srer_nogc$gridCompletion <- NULL
b_nogc <- bout_closed_capture(srer_nogc)
check(is.numeric(srer$cc$n_clamped) && srer$cc$n_clamped == sum(srer$b$status == "detection near-complete") &&
        srer$cc$n_incomplete == sum(srer$b$status == "incomplete grid") &&
        !any(b_nogc$incomplete_grid) && !any(b_nogc$status == "incomplete grid"),
      "closed_capture_series reports n_clamped and n_incomplete; without the gridCompletion field no bout is held out")
gk <- k_fixture("6 - trap set and empty")
check(expect_error(mnka_series(transform(gk, gridCompletion = "zzz"))),
      "an unknown gridCompletion token fails closed (constants are empty)")
old_opt <- options(smt.grid_complete_tokens = "test-complete", smt.grid_incomplete_tokens = "test-partial")
gk$gridCompletion <- ifelse(gk$date == as.Date("2021-07-02"), "test-partial", "test-complete")
gb2 <- bout_closed_capture(gk); gm <- mnka_series(gk); gs <- closed_capture_series(gk, gb2)
gk$gridCompletion <- "test-complete"
gb3 <- bout_closed_capture(gk)
options(old_opt)
check(identical(mam_grid_complete(c("TEST-complete", "test-partial", NA, " "),
                                  complete = "test-complete", incomplete = "test-partial"),
                c(TRUE, FALSE, NA, NA)) &&
        gb2$status == "incomplete grid" && gb2$incomplete_grid && is.na(gb2$N) &&
        all(gm$incomplete_grid) && gs$n_incomplete == 1L && gs$n_estimable == 0 &&
        gb3$status == ke$status && gb3$N == ke$N && !gb3$incomplete_grid,
      "gridCompletion: a night mapped incomplete holds its bout out and marks the plot-month")

cat(sprintf("\nAll %d helper contract tests passed.\n", passed))
