#!/usr/bin/env Rscript

# Fail-closed fixture tests for the pure scientific contracts in R/helpers.R.
# These tests use no network and no committed production bundle. Run from the
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

cat(sprintf("\nAll %d helper contract tests passed.\n", passed))
