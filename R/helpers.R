# ---------------------------------------------------------------------------
# helpers.R  —  the analytical engine
#
# Pure(ish) functions that turn a raw NEON `mam_pertrapnight` table into the
# metrics that power the app: leaderboards, the Chonk (body-condition) Index,
# trap-grid home ranges, rarity tiers, and community-level stats.
#
# Everything here is defensive: NEON tables are full of NAs and partial rows,
# so each function guards against empty / all-NA inputs.
# ---------------------------------------------------------------------------

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a

# Helpers are also sourced directly by the fail-closed scientific fixture, where
# global.R has not attached dplyr. Bind the declared dplyr pipe explicitly so
# analytical functions do not depend on an ambient search-path side effect.
`%>%` <- dplyr::`%>%`

# Most-frequent non-NA value (ties -> first). Used to pick an individual's
# "home" plot, modal species id, sex, etc.
mode_chr <- function(x) {
  x <- x[!is.na(x) & x != ""]
  if (length(x) == 0) return(NA_character_)
  names(sort(table(x), decreasing = TRUE))[1]
}

# Evaluate `expr` under a fixed seed, then put the session RNG back exactly as
# it was (or absent, if it was absent), so a reproducible helper cannot freeze
# or shift any other randomized output in the same reactive flush.
.with_seed <- function(seed, expr) {
  had <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  old <- if (had) get(".Random.seed", envir = .GlobalEnv, inherits = FALSE) else NULL
  on.exit({
    if (had) assign(".Random.seed", old, envir = .GlobalEnv)
    else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
      rm(".Random.seed", envir = .GlobalEnv)
  }, add = TRUE)
  set.seed(seed)
  force(expr)
}

safe_mean <- function(x) { x <- x[is.finite(x)]; if (length(x) == 0) NA_real_ else mean(x) }
safe_max  <- function(x) { x <- x[is.finite(x)]; if (length(x) == 0) NA_real_ else max(x) }

# min/max of a Date vector that returns NA (not -Inf/Inf) when all values are NA
safe_date_min <- function(x) { x <- x[!is.na(x)]; if (length(x) == 0) as.Date(NA) else min(x) }
safe_date_max <- function(x) { x <- x[!is.na(x)]; if (length(x) == 0) as.Date(NA) else max(x) }

# Strip the "NEON.MAM.D##." prefix to a short, human-friendly tag (e.g. R2626).
short_tag <- function(tagID) {
  sub("^NEON\\.MAM\\.D[0-9]{2}\\.", "", tagID)
}

# ---- species flair --------------------------------------------------------
# Genus -> emoji, just for personality in cards & tables.
genus_emoji <- function(scientificName) {
  g <- sub(" .*$", "", scientificName %||% "")
  lut <- c(
    Dipodomys = "\U0001F998",      # kangaroo rat -> kangaroo
    Chaetodipus = "\U0001F439", Perognathus = "\U0001F439",  # pocket mice -> hamster
    Peromyscus = "\U0001F42D", Reithrodontomys = "\U0001F42D",
    Onychomys = "\U0001F42D", Microtus = "\U0001F42D",
    Neotoma = "\U0001F400", Sigmodon = "\U0001F400", Rattus = "\U0001F400",
    Tamias = "\U0001F43F", Neotamias = "\U0001F43F",
    Tamiasciurus = "\U0001F43F", Sciurus = "\U0001F43F", Glaucomys = "\U0001F43F",
    Sorex = "\U0001F9A2", Blarina = "\U0001F9A2", Cryptotis = "\U0001F9A2",
    Zapus = "\U0001F42D", Mus = "\U0001F42D",
    Sylvilagus = "\U0001F407", Lepus = "\U0001F407",
    Spermophilus = "\U0001F43F", Ammospermophilus = "\U0001F43F",
    Otospermophilus = "\U0001F43F", Ictidomys = "\U0001F43F",
    Urocitellus = "\U0001F43F", Callospermophilus = "\U0001F43F"
  )
  out <- unname(lut[g])
  ifelse(is.na(out), "\U0001F400", out)  # default: rat
}

# Common-name-ish nickname for a scientific name (kept short, optional flair).
species_nickname <- function(scientificName) {
  g <- sub(" .*$", "", scientificName %||% "")
  lut <- c(
    Dipodomys = "kangaroo rat", Chaetodipus = "pocket mouse",
    Perognathus = "pocket mouse", Peromyscus = "deer mouse",
    Reithrodontomys = "harvest mouse", Onychomys = "grasshopper mouse",
    Microtus = "vole", Neotoma = "woodrat", Sigmodon = "cotton rat",
    Tamias = "chipmunk", Neotamias = "chipmunk", Tamiasciurus = "squirrel",
    Sciurus = "squirrel", Glaucomys = "flying squirrel", Sorex = "shrew",
    Blarina = "shrew", Cryptotis = "shrew", Zapus = "jumping mouse",
    Sylvilagus = "cottontail", Lepus = "jackrabbit",
    Spermophilus = "ground squirrel", Ammospermophilus = "antelope squirrel",
    Otospermophilus = "ground squirrel", Ictidomys = "ground squirrel",
    Urocitellus = "ground squirrel"
  )
  unname(lut[g]) %||% NA_character_
}

# Genus -> ecological group (label + color), used to color the national
# site-picker map and its legend. Grouping ~30 genera into 6 families keeps the
# map readable: each site is colored by the family of its most-caught species,
# so the biogeography reads at a glance (desert heteromyids vs. eastern
# deer-mice vs. prairie voles). Colors are drawn from a CVD-reasonable set.
GENUS_GROUPS <- list(
  list(key = "heteromyid", label = "Kangaroo & pocket mice", color = "#E1A100",
       genera = c("Dipodomys","Chaetodipus","Perognathus","Liomys","Heteromys")),
  list(key = "deermouse",  label = "Deer & harvest mice", color = "#2f7fb5",
       genera = c("Peromyscus","Reithrodontomys","Onychomys","Baiomys",
                  "Ochrotomys","Podomys")),
  list(key = "woodrat",    label = "Woodrats & cotton rats", color = "#AB0520",
       genera = c("Neotoma","Sigmodon","Rattus","Mus","Oryzomys","Sigmodontomys")),
  list(key = "vole",       label = "Voles & lemmings", color = "#1a7f37",
       genera = c("Microtus","Alexandromys","Myodes","Clethrionomys","Lemmus",
                  "Synaptomys","Dicrostonyx","Phenacomys","Ondatra")),
  list(key = "squirrel",   label = "Squirrels & chipmunks", color = "#6a4c93",
       genera = c("Tamias","Neotamias","Tamiasciurus","Sciurus","Glaucomys",
                  "Spermophilus","Ammospermophilus","Otospermophilus","Ictidomys",
                  "Urocitellus","Callospermophilus","Marmota","Sciurotamias")),
  list(key = "other",      label = "Shrews, jumping mice & kin", color = "#51677a",
       genera = c("Sorex","Blarina","Cryptotis","Notiosorex","Zapus","Napaeozapus",
                  "Sylvilagus","Lepus","Mustela","Tamiasciurus"))
)

# Resolve a scientific name to its group record (defaults to "other").
genus_group <- function(scientificName) {
  g <- sub(" .*$", "", scientificName %||% "")
  for (grp in GENUS_GROUPS) if (g %in% grp$genera) return(grp)
  GENUS_GROUPS[[length(GENUS_GROUPS)]]  # "other"
}

# 14 hand-verified, well-separated categorical anchors per mode. Used by BOTH
# make_species_pal() and the MNKA per-plot ramp so the two palettes never drift.
#
# COLLISION FIX (reaver 2026-07): the old code interpolated colorRampPalette over
# only 8 anchors, so a diverse (kangaroo-rat-heavy) site with >8 species produced
# MUDDY BLENDS between neighbours — at 12 species the two closest colours were 12
# RGB-units apart (#9A6F1F vs #A36D1E, indistinguishable). These fixed 14-anchor
# sets keep min pairwise RGB-distance = 50 (light) at N=14 (a 4x improvement) so
# no two species/plots read as the same colour. All LIGHT anchors verified
# >=3:1 on white AND mean(col2rgb) <= 200 (no near-white "invisible on white"
# slot, the beetle-app trap from LESSONS 2026-07-02). head(N) used up to 14; only
# beyond 14 (never seen on a NEON 3-plot grid) do we fall back to interpolation
# over the 14 anchors — still far better than interpolating 8.
SPECIES_PAL_DARK <- c(          # reads on the navy panel (Set2 core + distinct extras)
  "#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F", "#E5C494", "#B3B3B3",
  "#7CC8F5", "#F58AA0", "#C99AF0", "#5FD0C4", "#D98A3A", "#9FE0E0")  # minRGBdist(14)=49, all >3:1 on navy
SPECIES_PAL_LIGHT <- c(         # all >=3:1 on white, mean RGB <= 200, minRGBdist(14)=50
  "#0072B2", "#D55E00", "#009E73", "#CC79A7", "#9A6B00", "#1F8FBF", "#AA3377", "#444444",
  "#117733", "#882255", "#4B0092", "#8C510A", "#01665E", "#54278F")

species_anchor_pal <- function(dark = TRUE) if (isTRUE(dark)) SPECIES_PAL_DARK else SPECIES_PAL_LIGHT

# Expand the anchor set to exactly n distinct, collision-safe colours.
categorical_colors <- function(n, dark = TRUE) {
  anchors <- species_anchor_pal(dark)
  if (n <= length(anchors)) return(anchors[seq_len(n)])   # head(): no muddy blends
  grDevices::colorRampPalette(anchors)(n)                  # >14 species: interpolate anchors
}

# Stable, app-wide species -> color map so the SAME species is the SAME color
# on the map, the trend chart, the morphospace scatter, etc.
#
# Species are sorted, so the same species maps to the same slot in both modes ->
# colour stays CONSISTENT across every plot for a given mode, and re-themes when
# the caller re-reads is_dark() on toggle.
make_species_pal <- function(d, dark = TRUE) {
  sp <- sort(unique(d$scientificName[!is.na(d$scientificName)]))
  if (length(sp) == 0) return(character(0))
  stats::setNames(categorical_colors(length(sp), dark), sp)
}

# Luminance-picked label ink for text drawn ON a fill (pie slices, stacked bars).
# NEVER hardcode white: white on the dark-mode juvenile lime (#9bd24a) is ~1.79:1
# and the label vanishes; on the light-mode fills white reads 4-6:1 but on a bright
# slice it dies. YIQ-luminance pick returns dark navy ink on a bright fill, white on
# a dark fill — same rule as glow_badge() (global.R). Vectorised so it maps straight
# onto a colour vector for plotly `textfont`/`insidetextfont` per-slice colours.
# (reaver 2026-07: pie inside-labels were hardcoded #ffffff and failed in dark mode.)
label_ink <- function(fill) {
  vapply(fill, function(col) {
    tryCatch({
      rc <- grDevices::col2rgb(col)
      if ((0.299 * rc[1] + 0.587 * rc[2] + 0.114 * rc[3]) / 255 > 0.6) "#16243a" else "#ffffff"
    }, error = function(e) "#ffffff")
  }, character(1), USE.NAMES = FALSE)
}

# A one-line, friendly fact about a genus — for the "meet the mammals" cards
# (public learners + new NEON techs getting to know their site).
species_blurb <- function(scientificName) {
  g <- sub(" .*$", "", scientificName %||% "")
  lut <- c(
    Dipodomys   = "Kangaroo rats hop on huge hind feet, carry seeds in cheek pouches, and can live without ever drinking water.",
    Chaetodipus = "Spiny pocket mice stuff seeds into fur-lined cheek pouches and sleep away cold spells in torpor.",
    Perognathus = "Silky pocket mice are tiny seed-hoarders that go dormant underground through the worst of winter.",
    Peromyscus  = "Deer mice are wide-eyed climbers found almost everywhere in North America, the classic field-trap regular.",
    Reithrodontomys = "Harvest mice are thumb-sized seed-eaters that weave little grass nests above the ground.",
    Onychomys   = "Grasshopper mice are carnivores that hunt scorpions and insects, and howl like tiny wolves.",
    Neotoma     = "Woodrats (pack rats) build huge stick middens and famously swap any shiny object for what they carry.",
    Microtus    = "Voles are stout grass-eaters whose boom-and-bust population cycles drive whole food webs.",
    Sigmodon    = "Cotton rats breed fast and graze grasses; their numbers can explode after a wet season.",
    Sylvilagus  = "Cottontail rabbits freeze, then bolt in a zig-zag, a favorite meal for nearly every desert predator.",
    Tamias      = "Chipmunks are striped, cheek-pouched seed-hoarders busy stocking their burrows for winter.",
    Spermophilus = "Ground squirrels are burrowing sun-baskers that sound the alarm with sharp whistles.",
    Onychomys_  = ""
  )
  unname(lut[g]) %||% "A small mammal sampled in NEON's box-trapping grids."
}

# ---- rarity tiers ---------------------------------------------------------
# A playful RPG-style tier based on how many times an individual was caught.
rarity_tier <- function(captures) {
  dplyr::case_when(
    captures >= 15 ~ "Legendary",
    captures >= 10 ~ "Epic",
    captures >= 6  ~ "Rare",
    captures >= 3  ~ "Uncommon",
    TRUE           ~ "Common"
  )
}

# Rarity tier colors — desert-night ladder (glow_badge auto-picks dark text on the
# bright fills). MUST stay in lockstep with the legend in www/confirm.js.
rarity_meta <- function(tier) {
  lut <- list(
    Legendary = list(color = "#ffd24a", glow = "#ffd24a", icon = "⭐"),  # gold
    Epic      = list(color = "#fb8a7e", glow = "#fb8a7e", icon = "\U0001F48E"),  # coral
    Rare      = list(color = "#38a8e8", glow = "#38a8e8", icon = "\U0001F535"),  # azure blue
    Uncommon  = list(color = "#5fb56a", glow = "#5fb56a", icon = "\U0001F7E2"),  # green
    Common    = list(color = "#9fb0cf", glow = "#9fb0cf", icon = "▫")
  )
  lut[[tier]] %||% lut$Common
}

# ---- trap grid geometry ---------------------------------------------------
# NEON small-mammal grids are 10x10: columns A-J, rows 1-10 (10 m spacing).
# Parse a trapCoordinate like "J3" / "D10" into x (col 1-10) and y (row 1-10).
parse_trap <- function(trapCoordinate) {
  m <- regmatches(trapCoordinate, regexec("^([A-Ja-j])([0-9]{1,2})$", trapCoordinate))
  x <- vapply(m, function(z) if (length(z) == 3) match(toupper(z[2]), LETTERS) else NA_integer_, integer(1))
  y <- vapply(m, function(z) if (length(z) == 3) as.integer(z[3]) else NA_integer_, integer(1))
  x[!is.na(x) & x > 10] <- NA_integer_
  y[!is.na(y) & y > 10] <- NA_integer_
  list(x = x, y = y)
}

# ---- reviewed physical trap-event effort contract ------------------------
#
# `mam_pertrapnight` is row-shaped for animal handling, not always for physical
# trap effort: status 4 can emit one row per animal caught in the same trap. The
# app therefore resolves effort once at the physical-event grain and allocates it
# back to rows. All downstream sums of `trap_effort` remain valid, while every
# tagged animal row remains available as a capture.
MAM_EFFORT_REQUIRED <- c(
  "collectDate", "nightuid", "plotID", "trapCoordinate", "trapStatus",
  "tagID", "remarks")
MAM_TRAP_STATUS_EFFORT <- c(
  "1 - trap not set" = 0,
  "2 - trap disturbed/door closed but empty" = 0.5,
  "3 - trap door open or closed w/ spoor left" = 0.5,
  "4 - more than 1 capture in one trap" = 1,
  "5 - capture" = 1,
  "6 - trap set and empty" = 1)
MAM_GRID_COORDINATE_RE <- "^[A-J](?:[1-9]|10)$"
MAM_PLACEHOLDER_COORDINATE_RE <- "^(?:[A-J]X|X(?:[1-9]|10)|XX)$"
MAM_REVIEWED_MULTI_TRAP_MARKERS <- c(
  "trap accidentally double set",
  "double trap method (two traps set at each location)")

# ---- gridCompletion contract (plumbing only) ------------------------------
# NEON (2025-11-20) adds a per-night `gridCompletion` field: grid setting is
# complete when >= 75% of traps were deployed, processing is complete when >= 75%
# of captures were tagged and identified. Its value list is not known until the
# first refresh that carries it. mam_grid_complete() maps a token to TRUE
# (complete), FALSE (incomplete) or NA (missing/blank) and stops on any other
# non-NA token, naming it, so a new vocabulary fails closed instead of being
# read as complete. Tests pass made-up tokens through the options
# smt.grid_complete_tokens / smt.grid_incomplete_tokens; production reads the
# constants.
# Vocabulary as published in RELEASE-2026 (all 46 bundles, refresh run
# 40346798927, 2026-10-26). A night is incomplete when grid setting or capture
# processing is "not complete"; a fully set grid that caught nothing is complete.
# NA (no value: nights NEON did not backfill because samplingImpractical was set)
# stays NA and is never read as incomplete.
MAM_GRID_COMPLETE_TOKENS <- c(
  "setting complete, processing complete",
  "setting complete, no small mammals captured",
  "setting complete, no targeted or opportunistic small mammals captured")
MAM_GRID_INCOMPLETE_TOKENS <- c(
  "setting complete, processing not complete",
  "setting not complete, processing complete",
  "setting not complete, no small mammals captured")
mam_grid_complete <- function(x,
    complete = getOption("smt.grid_complete_tokens", MAM_GRID_COMPLETE_TOKENS),
    incomplete = getOption("smt.grid_incomplete_tokens", MAM_GRID_INCOMPLETE_TOKENS)) {
  tok <- tolower(trimws(as.character(x)))
  tok[!is.na(tok) & !nzchar(tok)] <- NA_character_
  out <- rep(NA, length(tok))
  out[tok %in% tolower(complete)] <- TRUE
  out[tok %in% tolower(incomplete)] <- FALSE
  unknown <- !is.na(tok) & is.na(out)
  if (any(unknown))
    stop(sprintf("unknown gridCompletion token(s): %s",
                 paste(utils::head(sort(unique(tok[unknown])), 5L), collapse = ", ")),
         call. = FALSE)
  out
}

mam_require_columns <- function(d, required = MAM_EFFORT_REQUIRED,
                                label = "mammal bundle") {
  if (!is.data.frame(d))
    stop(sprintf("%s must be a data frame", label), call. = FALSE)
  missing <- setdiff(required, names(d))
  if (length(missing))
    stop(sprintf("%s lacks required field(s): %s", label,
                 paste(missing, collapse = ", ")), call. = FALSE)
  invisible(d)
}

mam_trap_status_effort <- function(status) {
  token <- tolower(trimws(as.character(status)))
  token[is.na(status)] <- NA_character_
  out <- unname(MAM_TRAP_STATUS_EFFORT[token])
  unknown <- is.na(out)
  if (any(unknown))
    stop(sprintf("mammal bundle has %d unknown/non-exact trapStatus token(s): %s",
                 sum(unknown),
                 paste(utils::head(sort(unique(token[unknown])), 5L),
                       collapse = ", ")), call. = FALSE)
  as.numeric(out)
}

mam_tag_present <- function(tag_id) {
  tag <- as.character(tag_id)
  !is.na(tag_id) & nzchar(trimws(tag))
}

# NEON placeholder tag IDs. Per NEON.DOC.000481 SOP C.8 (and SOP C step 3 for an
# animal that escapes before it is tagged), an untagged capture is issued
# NEON.MAM.Dxx.O<3-digit plot><trapCoordinate><YYYYMMDD> (about 1% are older
# short forms without the date, e.g. NEON.MAM.D01.O001F1). The ID identifies a
# capture EVENT, not an animal: it is never a mark (short-form IDs without the
# date can recur on 2 to 3 dates, but they still name events, not an animal). Ear tags such as "OL1001" / "OR1001" (left /
# right ear plus digits) are real marks and are excluded by the lookahead.
MAM_PLACEHOLDER_TAG_RE <- "^NEON[.]MAM[.]D[0-9]{2}[.]O(?![LR][0-9]+$)"
mam_is_placeholder_tag <- function(tag_id) {
  t <- as.character(tag_id)
  !is.na(t) & grepl(MAM_PLACEHOLDER_TAG_RE, t, perl = TRUE)
}
# A real, recapturable mark: a present tag that is not a placeholder.
mam_tag_marked <- function(tag_id) mam_tag_present(tag_id) & !mam_is_placeholder_tag(tag_id)

mam_multi_trap_marker <- function(remarks) {
  x <- tolower(as.character(remarks))
  vapply(x, function(value) {
    if (is.na(value)) return(0L)
    hit <- which(vapply(MAM_REVIEWED_MULTI_TRAP_MARKERS,
                        grepl, logical(1), x = value, fixed = TRUE))
    if (length(hit) == 1L) as.integer(hit) else 0L
  }, integer(1))
}

# Return one audit row per SOURCE row. `trap_effort` is allocated exactly once
# for a one-trap multi-capture group (first row owns 1; the other animal rows own
# 0). In a reviewed double-trap group both source rows are real traps and retain
# their individual status weights. Placeholder coordinates deliberately stay
# row-level because they cannot identify a shared physical trap.
mam_resolve_effort_rows <- function(d, year, label = "mammal bundle") {
  mam_require_columns(d, label = label)
  if (length(year) != nrow(d) || any(!is.finite(year)) ||
      any(year != as.integer(year)))
    stop(sprintf("%s requires one finite integer year per row", label),
         call. = FALSE)
  if (!nrow(d))
    return(data.frame(
      trap_event = character(), trap_effort = numeric(),
      trap_effort_rule = character(), trap_event_source_rows = integer(),
      trap_effort_owner = logical(), stringsAsFactors = FALSE))

  event_cols <- c("nightuid", "plotID", "trapCoordinate")
  bad_key <- Reduce(`|`, lapply(event_cols, function(k) {
    value <- as.character(d[[k]])
    is.na(d[[k]]) | !nzchar(value)
  }))
  if (any(bad_key))
    stop(sprintf("%s has %d rows without a complete trap-event key",
                 label, sum(bad_key)), call. = FALSE)

  coordinate <- as.character(d$trapCoordinate)
  canonical <- grepl(MAM_GRID_COORDINATE_RE, coordinate, perl = TRUE)
  placeholder <- grepl(MAM_PLACEHOLDER_COORDINATE_RE,
                       coordinate, perl = TRUE)
  unknown_coordinate <- !(canonical | placeholder)
  if (any(unknown_coordinate))
    stop(sprintf("%s has %d unreviewed trapCoordinate token(s): %s",
                 label, sum(unknown_coordinate),
                 paste(utils::head(sort(unique(coordinate[unknown_coordinate])),
                                   5L), collapse = ", ")), call. = FALSE)

  status <- tolower(trimws(as.character(d$trapStatus)))
  effort <- mam_trap_status_effort(status)
  tag <- trimws(as.character(d$tagID))
  tag_present <- mam_tag_present(d$tagID)
  marker <- mam_multi_trap_marker(d$remarks)
  physical_key <- paste(as.integer(year), d$nightuid, d$plotID, coordinate,
                        sep = "|")

  out <- data.frame(
    trap_event = rep(NA_character_, nrow(d)),
    trap_effort = rep(NA_real_, nrow(d)),
    trap_effort_rule = rep(NA_character_, nrow(d)),
    trap_event_source_rows = rep(NA_integer_, nrow(d)),
    trap_effort_owner = rep(FALSE, nrow(d)),
    stringsAsFactors = FALSE)

  placeholder_rows <- which(placeholder)
  if (length(placeholder_rows)) {
    out$trap_event[placeholder_rows] <-
      paste("placeholder-row", placeholder_rows, sep = "|")
    out$trap_effort[placeholder_rows] <- effort[placeholder_rows]
    out$trap_effort_rule[placeholder_rows] <- "placeholder-row-level"
    out$trap_event_source_rows[placeholder_rows] <- 1L
    out$trap_effort_owner[placeholder_rows] <- TRUE
  }

  duplicated_canonical <- canonical &
    (duplicated(physical_key) | duplicated(physical_key, fromLast = TRUE))
  canonical_single_rows <- which(canonical & !duplicated_canonical)
  if (length(canonical_single_rows)) {
    out$trap_event[canonical_single_rows] <- physical_key[canonical_single_rows]
    out$trap_effort[canonical_single_rows] <- effort[canonical_single_rows]
    out$trap_effort_rule[canonical_single_rows] <- "canonical-single"
    out$trap_event_source_rows[canonical_single_rows] <- 1L
    out$trap_effort_owner[canonical_single_rows] <- TRUE
  }

  duplicate_rows <- which(duplicated_canonical)
  groups <- split(duplicate_rows, physical_key[duplicate_rows])
  for (ix in groups) {
    key <- physical_key[ix[[1L]]]
    capture_status <- status[ix] %in% c(
      "4 - more than 1 capture in one trap", "5 - capture")
    has_multi_capture_status <- any(
      status[ix] == "4 - more than 1 capture in one trap")
    reviewed_marker <- length(ix) == 2L && all(marker[ix] > 0L) &&
      length(unique(marker[ix])) == 1L
    same_collect_date <- length(unique(as.character(d$collectDate[ix]))) == 1L
    reviewed_tags <- tag[ix][tag_present[ix]]
    reviewed_tags_unique <- !anyDuplicated(reviewed_tags)
    one_trap_multi_capture <- all(capture_status) &&
      has_multi_capture_status && all(tag_present[ix]) &&
      !anyDuplicated(tag[ix]) && all(marker[ix] == 0L) && same_collect_date

    out$trap_event[ix] <- key
    out$trap_event_source_rows[ix] <- as.integer(length(ix))
    if (one_trap_multi_capture) {
      out$trap_effort[ix] <- 0
      out$trap_effort[ix[[1L]]] <- 1
      out$trap_effort_rule[ix] <- "canonical-multi-capture-one-trap"
      out$trap_effort_owner[ix[[1L]]] <- TRUE
    } else if (reviewed_marker && !has_multi_capture_status &&
               reviewed_tags_unique && same_collect_date) {
      out$trap_effort[ix] <- effort[ix]
      out$trap_effort_rule[ix] <- "reviewed-double-trap-rows"
      out$trap_effort_owner[ix] <- TRUE
    } else {
      stop(sprintf(
        "%s has an unreviewed duplicated canonical trap event %s (%d rows; statuses: %s)",
        label, key, length(ix), paste(sort(unique(status[ix])), collapse = " + ")),
        call. = FALSE)
    }
  }

  if (anyNA(out$trap_event) || anyNA(out$trap_effort) ||
      anyNA(out$trap_effort_rule) || anyNA(out$trap_event_source_rows))
    stop(sprintf("%s effort resolver left unresolved source rows", label),
         call. = FALSE)
  out
}

# ---------------------------------------------------------------------------
# clean_mam(): the one normalizer every downstream function expects.
# Returns ALL trap rows (captures + empties) plus derived columns; downstream
# code filters to handled animals (`!is.na(tagID)`) where appropriate.
# ---------------------------------------------------------------------------
clean_mam <- function(data.raw) {
  if (is.null(data.raw) || nrow(data.raw) == 0) return(NULL)
  d <- tibble::as_tibble(data.raw)

  # guarantee the columns we lean on exist
  need <- c("tagID","taxonID","scientificName","plotID","trapCoordinate",
            "collectDate","recapture","fate","hindfootLength","earLength",
            "tailLength","totalLength","weight","lifeStage","sex","testes",
            "nipples","pregnancyStatus","vagina","decimalLatitude",
            "decimalLongitude","elevation","nlcdClass","domainID","siteID",
            "nightuid","trapStatus","remarks","nativeStatusCode","taxonRank")
  for (col in need) if (!col %in% names(d)) d[[col]] <- NA

  d$weight         <- suppressWarnings(as.numeric(d$weight))
  d$hindfootLength <- suppressWarnings(as.numeric(d$hindfootLength))
  d$tailLength     <- suppressWarnings(as.numeric(d$tailLength))
  d$earLength      <- suppressWarnings(as.numeric(d$earLength))
  d$totalLength    <- suppressWarnings(as.numeric(d$totalLength))

  d$date  <- as.Date(substr(as.character(d$collectDate), 1, 10))
  d$year  <- as.integer(format(d$date, "%Y"))
  d$ym    <- substr(as.character(d$date), 1, 7)
  d$short <- short_tag(d$tagID)

  tp <- parse_trap(d$trapCoordinate)
  d$tx <- tp$x; d$ty <- tp$y

  d$is_capture <- mam_tag_present(d$tagID)
  effort_rows <- mam_resolve_effort_rows(
    as.data.frame(d), d$year,
    sprintf("%s mammal bundle", mode_chr(d$siteID) %||% "site"))
  d$trap_event <- effort_rows$trap_event
  d$trap_effort <- effort_rows$trap_effort
  d$trap_effort_rule <- effort_rows$trap_effort_rule
  d$trap_event_source_rows <- effort_rows$trap_event_source_rows
  d$trap_effort_owner <- effort_rows$trap_effort_owner
  d$is_set <- mam_trap_status_effort(d$trapStatus) > 0
  d
}

# ---------------------------------------------------------------------------
# Body-condition / "Chonk" Index
# An ADULT weight percentile WITHIN species. NOT a Scaled Mass Index (Peig &
# Green 2009); see the note inside the function for why the SMI is not used.
# ---------------------------------------------------------------------------
compute_condition <- function(d) {
  empty <- tibble::tibble(tagID = character(), scientificName = character(),
                          mean_weight = numeric(), mean_hf = numeric(),
                          chonk_pct = numeric(), chonk_tier = character(),
                          n_meas = integer())

  # NOTE on the science: NEON populates `totalLength` for almost none of these
  # taxa (≈0.3% of rows) and adult hindfoot length barely scales with mass
  # (r≈0.15 for kangaroo/pocket mice), so a Peig & Green (2009) Scaled Mass
  # Index would mostly rank measurement noise. We therefore define the "Chonk"
  # score honestly as an ADULT weight percentile WITHIN species — a true,
  # defensible statement ("heavy for its kind") — and leave the actual
  # mass↔length relationship to the body-size scatter, where users can see it.
  ad <- dplyr::filter(d, !is.na(.data$tagID), !is.na(.data$weight), .data$weight > 0,
                      .data$lifeStage == "adult")
  if (nrow(ad) == 0) return(empty)
  # QC-flag lifecycle · stage 6: drop capture weights flagged as possible data-entry
  # errors (±5·MAD within species-adults — the SAME rule the species table's ⚠ and
  # the QC modal use) BEFORE ranking, so one bad reading can't drag an animal's
  # weight-for-species percentile down or pollute its species pool's ranks. This
  # mirrors how the species median/IQR already exclude them; disclosed in the chonk
  # caption. If an animal's ONLY adult weight is flagged it drops to NA (no reliable
  # weight), which is the honest result.
  ad <- ad %>%
    dplyr::group_by(.data$scientificName) %>%
    dplyr::mutate(.wflag = .is_meas_outlier(.data$weight, .data$weight)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!.data$.wflag) %>%
    dplyr::select(-".wflag")
  if (nrow(ad) == 0) return(empty)

  ind <- ad %>%
    dplyr::group_by(.data$tagID) %>%
    dplyr::summarise(
      scientificName = mode_chr(.data$scientificName),
      mean_weight = round(mean(.data$weight), 1),
      mean_hf     = round(safe_mean(.data$hindfootLength), 1),
      n_meas      = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::group_by(.data$scientificName) %>%
    dplyr::mutate(
      .enough   = dplyr::n() >= 4,
      chonk_pct = dplyr::if_else(.data$.enough,
                                 round(100 * dplyr::percent_rank(.data$mean_weight)), NA_real_)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-".enough")

  ind$chonk_tier <- chonk_label(ind$chonk_pct)
  ind
}

# Per-species mass↔hindfoot scaling for the body-size scatter's reference line.
# Returns the SMA (standardized major axis) fit ONLY where the relationship is
# real (n adults >= 15 and |r| >= 0.3); otherwise b is NA and no line is drawn.
species_scaling <- function(d, min_n = 15, min_r = 0.30) {
  ad <- dplyr::filter(d, !is.na(.data$tagID), .data$lifeStage == "adult",
                      !is.na(.data$weight), !is.na(.data$hindfootLength),
                      .data$weight > 0, .data$hindfootLength > 0)
  if (nrow(ad) == 0)
    return(tibble::tibble(scientificName = character(), n = integer(), r = numeric(),
                          b = numeric(), L0 = numeric()))
  ad %>%
    dplyr::group_by(.data$scientificName) %>%
    dplyr::summarise(
      n  = dplyr::n(),
      L0 = mean(.data$hindfootLength),
      r  = suppressWarnings(stats::cor(log(.data$hindfootLength), log(.data$weight))),
      b  = suppressWarnings(stats::sd(log(.data$weight)) / stats::sd(log(.data$hindfootLength))) *
             sign(suppressWarnings(stats::cor(log(.data$hindfootLength), log(.data$weight)))),
      .groups = "drop"
    ) %>%
    dplyr::mutate(b = dplyr::if_else(.data$n >= min_n & !is.na(.data$r) & abs(.data$r) >= min_r,
                                     .data$b, NA_real_))
}

chonk_label <- function(pct) {
  dplyr::case_when(
    is.na(pct)  ~ "n/a",
    pct >= 95   ~ "MEGACHONK",
    pct >= 80   ~ "Chonky",
    pct >= 60   ~ "Husky",
    pct >= 40   ~ "Average",
    pct >= 20   ~ "Trim",
    TRUE        ~ "Lean"
  )
}

# Mean distance (m) of an individual's captures from its trap-grid centroid.
# Traps are 10 m apart. NB: this is a grid-bounded roam/dispersion index, NOT a
# true (area) home range — the UI labels it "roam radius / mean displacement".
roam_radius <- function(tx, ty) {
  ok <- !is.na(tx) & !is.na(ty)
  tx <- tx[ok]; ty <- ty[ok]
  if (length(tx) < 2) return(NA_real_)          # undefined for a single capture
  cx <- mean(tx); cy <- mean(ty)
  round(mean(sqrt((tx - cx)^2 + (ty - cy)^2)) * 10, 1)
}

# Maximum Distance Moved: the largest straight-line gap (m) between any two of
# an individual's capture locations — the standard, defensible movement metric.
max_dist_moved <- function(tx, ty) {
  ok <- !is.na(tx) & !is.na(ty)
  tx <- tx[ok]; ty <- ty[ok]
  if (length(tx) < 2) return(NA_real_)
  d <- stats::dist(cbind(tx, ty))
  round(max(d) * 10, 1)
}

# ---------------------------------------------------------------------------
# build_leaderboard(): one row per individual with every metric the UI ranks.
# Individuals are real ear tags only (mam_tag_marked): a NEON SOP C.8
# placeholder ID names one untagged capture event, not an animal, so it never
# gets a leaderboard row, a rarity tier or a dossier.
# ---------------------------------------------------------------------------
build_leaderboard <- function(d) {
  base <- d[mam_tag_marked(d$tagID), , drop = FALSE]
  if (nrow(base) == 0) return(NULL)

  ind <- base %>%
    dplyr::group_by(.data$tagID) %>%
    dplyr::summarise(
      scientificName = mode_chr(.data$scientificName),
      home_plot      = mode_chr(.data$plotID),
      captures       = dplyr::n(),
      n_recap        = sum(.data$recapture %in% c("Y", "y"), na.rm = TRUE),
      first_seen     = safe_date_min(.data$date),
      last_seen      = safe_date_max(.data$date),
      career_days    = {
                         a <- safe_date_min(.data$date); b <- safe_date_max(.data$date)
                         if (is.na(a) || is.na(b)) NA_integer_ else as.integer(b - a)
                       },
      # spatial impossibility: the same tag at >1 plot on a SINGLE day = two
      # animals sharing a number (plots are km apart, beyond a heteromyid's daily
      # move). Within a site NEON tag numbers are unique, so this — not a long
      # career — is the real "two animals" signal.
      spatial_conflict = { ok <- !is.na(.data$date) & !is.na(.data$plotID)
                           if (sum(ok) < 2L) FALSE
                           else any(tapply(.data$plotID[ok], .data$date[ok],
                                           function(p) length(unique(p))) > 1L) },
      n_traps        = dplyr::n_distinct(.data$trapCoordinate[!is.na(.data$trapCoordinate)]),
      plots_visited  = dplyr::n_distinct(.data$plotID[!is.na(.data$plotID)]),
      avg_weight     = round(safe_mean(.data$weight), 1),
      max_weight     = round(safe_max(.data$weight), 1),
      avg_hf         = round(safe_mean(.data$hindfootLength), 1),
      sex            = mode_chr(.data$sex),
      lifeStage      = mode_chr(.data$lifeStage),
      roam_m         = roam_radius(.data$tx, .data$ty),
      mdm_m          = max_dist_moved(.data$tx, .data$ty),
      .groups = "drop"
    )

  # n_species_ids: distinct scientificName per tag, from the RAW capture rows.
  # MUST live OUTSIDE the summarise() above — there `scientificName =
  # mode_chr(.data$scientificName)` reassigns (shadows) the source column, so a
  # same-call `n_distinct(.data$scientificName)` sees the length-1 modal scalar and
  # collapses to 1 for EVERY animal, silently disabling the multi-species-tag QC
  # flag (individual_qc_flags #7) and the ambiguous-ID exclusion in
  # min_known_lifespan(). Computed independently here so it can't be re-shadowed.
  # Counted on species-level names only (species_level_only drops "X sp." and
  # "A/B" names), so a genus-only record next to a species record is not read as
  # a second species. n_genus_ids lets the QC flag tell a cross-genus change
  # (worth verifying) from a within-genus swap (common, low concern).
  nsp <- species_level_only(base[!is.na(base$scientificName), , drop = FALSE]) %>%
    dplyr::group_by(.data$tagID) %>%
    dplyr::summarise(n_species_ids = dplyr::n_distinct(.data$scientificName),
                     n_genus_ids = dplyr::n_distinct(genus_of(.data$scientificName)),
                     .groups = "drop")
  ind <- dplyr::left_join(ind, nsp, by = "tagID")
  ind$n_species_ids <- dplyr::coalesce(ind$n_species_ids, 0L)   # no species-level name -> 0
  ind$n_genus_ids   <- dplyr::coalesce(ind$n_genus_ids, 0L)

  # first_stage: life stage at the EARLIEST dated capture (not the modal stage);
  # the age clock only has a birth anchor if we first met the animal young.
  # Computed OUTSIDE the summarise() for the same shadowing reason as
  # n_species_ids: there `lifeStage = mode_chr(.data$lifeStage)` reassigns the
  # column, so a later read saw the modal scalar (or NA), never the first stage.
  # Undated rows are dropped; a tag with no dated row gets NA.
  fst <- base[!is.na(base$date), c("tagID", "date", "lifeStage"), drop = FALSE]
  fst <- fst[order(fst$tagID, fst$date), , drop = FALSE]
  fst <- fst[!duplicated(fst$tagID), , drop = FALSE]
  ind <- dplyr::left_join(ind,
                          tibble::tibble(tagID = fst$tagID,
                                         first_stage = as.character(fst$lifeStage)),
                          by = "tagID")

  cond <- compute_condition(d)
  ind <- dplyr::left_join(ind,
                          dplyr::select(cond, "tagID", "chonk_pct", "chonk_tier"),
                          by = "tagID")

  ind <- ind %>%
    dplyr::mutate(
      short    = short_tag(.data$tagID),
      emoji    = genus_emoji(.data$scientificName),
      nickname = species_nickname(.data$scientificName),
      rarity   = rarity_tier(.data$captures),
      career_days = dplyr::if_else(is.na(.data$career_days), 0L, .data$career_days),
      # Identity-QA flag — NOT "reused tag". NEON does not recycle tag numbers; a
      # tag is applied once and kept for life, and tag numbers are unique within a
      # site (the app loads one site at a time, so group_by(tagID) is already
      # site-scoped). A 1.5–3.5 yr career is NORMAL for these desert heteromyids
      # (D. merriami reaches ≥3.5 yr in the wild — Zeng & Brown 1987), so we must
      # NOT flag long careers. We flag only a history that can't be ONE valid
      # animal: a same-day two-plot record (spatially impossible), or a span
      # beyond any wild career for these genera (>5 yr; genus maxima ~3.5–5 yr).
      # Seasonal detection gaps are NOT flagged — desert rodents routinely go a
      # full year undetected (a detection artifact, not reuse). See Fauna review.
      tag_suspect = .data$spatial_conflict | (.data$career_days > 1825),
      id_uncertain = .data$n_species_ids > 1,
      # APPROX AGE (decimal years) = career span + a coarse estimate of how old
      # the animal already was at first capture. Genus-level heteromyid offsets
      # (ADW/AnAge D. merriami: weaning 15–25 d, maturity 60–102 d): juvenile
      # ~30 d, subadult ~75 d. A CONFIRMED adult first capture is LEFT-CENSORED:
      # it was already mature when first seen, so its true age is unknown-and-
      # greater — floored at ~90 d (earliest reliably-scored adult) and flagged a
      # MINIMUM (shown with "≥"). Unknown / unrecorded first stage is ALSO a
      # minimum, but we must NOT assume maturity, so it takes the conservative
      # 30 d (juvenile) floor — a smaller, safer lower bound for "we don't know."
      # Right-censored regardless: the clock stops at last capture, not death.
      # career_days is already coalesced to 0 above.
      age_offset_days  = dplyr::case_when(.data$first_stage == "juvenile" ~ 30,
                                          .data$first_stage == "subadult" ~ 75,
                                          .data$first_stage == "adult"    ~ 90,
                                          TRUE ~ 30),   # unknown / unrecorded: don't assume it was a mature adult
      approx_age_years = round((.data$age_offset_days + .data$career_days) / 365.25, 1),
      age_is_minimum   = !(.data$first_stage %in% c("juvenile", "subadult"))
    ) %>%
    dplyr::arrange(dplyr::desc(.data$captures), dplyr::desc(.data$career_days)) %>%
    dplyr::mutate(rank = dplyr::row_number())

  ind
}

# Ready-to-sort leaderboard for a named category.
leaderboard_by <- function(lb, category = c("captures", "weight", "career", "roam", "chonk")) {
  category <- match.arg(category)
  key <- switch(category,
    captures = "captures", weight = "max_weight", career = "career_days",
    roam = "roam_m", chonk = "chonk_pct")
  out <- lb[!is.na(lb[[key]]), ]
  # for the "career" board, hide the few impossible histories (same-day two-plot
  # or beyond-lifespan span) so the ranking is honest; everywhere else keep all rows
  if (category == "career") out <- out[!out$tag_suspect, ]
  ord <- if (category == "chonk")
    order(-out[[key]], -out$max_weight) else order(-out[[key]])
  out[ord, ]
}

# ---------------------------------------------------------------------------
# Community-level snapshot for the site/date window.
# ---------------------------------------------------------------------------
community_stats <- function(d, lb = NULL) {
  handled <- dplyr::filter(d, .data$is_capture)
  if (is.null(lb)) lb <- build_leaderboard(d)

  list(
    total_captures = nrow(handled),
    # distinct real ear tags; placeholder IDs are capture events, not animals
    individuals    = dplyr::n_distinct(handled$tagID[mam_tag_marked(handled$tagID)]),
    untagged_captures = sum(mam_is_placeholder_tag(handled$tagID)),
    # species-level (incl. cf. species): excludes genus-only "X sp.", ambiguous
    # "A/B" and cf./aff. genus or family IDs, so this matches the richness used
    # by Hill numbers, Chao1, and the map
    species        = dplyr::n_distinct(species_level_only(
                       dplyr::filter(handled, !is.na(.data$scientificName)))$scientificName),
    # the same list without cf. species / aff. species records: species with at
    # least one unqualified species-level identification
    species_confirmed = dplyr::n_distinct(species_confirmed_only(
                       dplyr::filter(handled, !is.na(.data$scientificName)))$scientificName),
    plots          = dplyr::n_distinct(d$plotID[!is.na(d$plotID)]),
    trap_nights    = round(sum(d$trap_effort, na.rm = TRUE)),  # display value (CPUE keeps the precise denominator)
    recap_rate     = if (nrow(handled) > 0)
                       round(100 * mean(handled$recapture %in% c("Y", "y"), na.rm = TRUE), 1) else 0,
    legendary      = if (!is.null(lb)) sum(lb$captures >= 10, na.rm = TRUE) else 0,
    n_female       = sum(handled$sex == "F", na.rm = TRUE),
    n_male         = sum(handled$sex == "M", na.rm = TRUE),
    date_min       = safe_date_min(d$date),
    date_max       = safe_date_max(d$date),
    heaviest       = if (!is.null(lb) && any(is.finite(lb$max_weight)))
                       lb[which.max(lb$max_weight), ] else NULL,
    most_caught    = if (!is.null(lb) && nrow(lb) > 0) lb[1, ] else NULL,
    biggest_roamer = if (!is.null(lb) && any(is.finite(lb$roam_m)))
                       lb[which.max(lb$roam_m), ] else NULL
  )
}

# Per-species community summary (richness / abundance table + charts).
species_summary <- function(d) {
  hh <- dplyr::filter(d, !is.na(.data$tagID), !is.na(.data$scientificName))
  # ADULT mean weight, de-pseudoreplicated and gated. Two honesty fixes:
  #  (1) average ONE mean-weight per INDIVIDUAL first, so a much-recaptured heavy
  #      animal weighed 20× can't pull the species mean up (and the n counts
  #      animals, not capture-rows);
  #  (2) require >= 8 distinct staged adults before reporting a mean — matches the
  #      size-violin / dossier-band n>=8 floors sitting beside it; below that a
  #      "typical weight" / "heaviest species" claim is too noisy, so avg_weight
  #      is NA (renders as "—" with an "n too low" note). adults-only as before.
  adult_w <- hh %>%
    dplyr::filter(.data$lifeStage %in% "adult", is.finite(.data$weight), .data$weight > 0) %>%
    dplyr::group_by(.data$scientificName, .data$tagID) %>%
    dplyr::summarise(w = mean(.data$weight), .groups = "drop_last") %>%
    dplyr::summarise(n_adult = dplyr::n(), adult_mean = mean(.data$w), .groups = "drop")

  hh %>%
    dplyr::group_by(.data$scientificName) %>%
    dplyr::summarise(
      # individuals = distinct real ear tags; untagged_captures = placeholder
      # capture events (counted in captures, never as individuals)
      individuals = dplyr::n_distinct(.data$tagID[mam_tag_marked(.data$tagID)]),
      untagged_captures = sum(mam_is_placeholder_tag(.data$tagID)),
      captures    = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::left_join(adult_w, by = "scientificName") %>%
    dplyr::mutate(
      n_adult    = dplyr::coalesce(.data$n_adult, 0L),
      avg_weight = dplyr::if_else(.data$n_adult >= 8L, round(.data$adult_mean, 1), NA_real_)
    ) %>%
    dplyr::select(-"adult_mean") %>%
    dplyr::arrange(dplyr::desc(.data$captures)) %>%
    dplyr::mutate(emoji = genus_emoji(.data$scientificName),
                  nickname = species_nickname(.data$scientificName))
}

# Per-species body-measurement profile. ADULTS ONLY — juveniles/subadults are
# much lighter and shorter, so pooling stages inflates the range and pulls the
# median down (verified on JORN: C. eremicus mean 14.7 g all-stages vs 15.4 g
# adults-only; D. merriami 42.7 vs 43.7). Matches the body-size violin + Chonk
# Index, which are also adults-only.
# NEON records weight + hindfoot densely; tail/ear are sparse, so each measure
# carries its own n and a cell is shown only where measured. "Mode" isn't
# meaningful for a continuous measure, so we report median + range. One row per
# species-level ID.
species_measurements <- function(d) {
  h <- species_level_only(dplyr::filter(d, !is.na(.data$tagID), !is.na(.data$scientificName),
                                        .data$lifeStage %in% "adult"))
  if (is.null(h) || !nrow(h)) return(NULL)
  pos <- function(x) { x[is.finite(x) & x > 0] }
  npos <- function(x) length(pos(x))
  med  <- function(x) { v <- pos(x); if (length(v)) round(stats::median(v), 1) else NA_real_ }
  # Headline range is the p5–p95 envelope (a "typical adult" band), so a single
  # mis-keyed value can't blow the bracket open — the raw min/max are kept
  # separately for QC.
  lo   <- function(x) { v <- pos(x); if (length(v)) round(stats::quantile(v, 0.05, names = FALSE), 1) else NA_real_ }
  hi   <- function(x) { v <- pos(x); if (length(v)) round(stats::quantile(v, 0.95, names = FALSE), 1) else NA_real_ }
  rmin <- function(x) { v <- pos(x); if (length(v)) round(min(v), 1) else NA_real_ }
  rmax <- function(x) { v <- pos(x); if (length(v)) round(max(v), 1) else NA_real_ }
  # Count values flagged as POSSIBLE data-entry errors: |x − median| > 5·MAD,
  # with MAD floored at 10% of the median so a tight integer distribution (pocket
  # mice clustered at a few grams, mad≈1.5) doesn't read normal jitter as an
  # error. MAD/median are breakdown-robust so the outlier can't mask itself. The
  # flagged value STAYS in the data (and in the QC tooltip) but is beyond the
  # p5–p95 range shown; this is a "verify this record" affordance, not a delete.
  nflag <- function(x) {
    v <- pos(x); if (length(v) < 3) return(0L)
    m <- stats::median(v); s <- max(stats::mad(v), 0.1 * m)
    if (!is.finite(s) || s <= 0) return(0L)
    as.integer(sum(abs(v - m) > 5 * s))
  }
  out <- h %>% dplyr::group_by(.data$scientificName) %>% dplyr::summarise(
    n_ind  = dplyr::n_distinct(.data$tagID[mam_tag_marked(.data$tagID)]),   # ear-tagged animals
    w_n  = npos(.data$weight),  w_med  = med(.data$weight),  w_lo  = lo(.data$weight),  w_hi  = hi(.data$weight),  w_min  = rmin(.data$weight),         w_max  = rmax(.data$weight),         w_nflag  = nflag(.data$weight),
    hf_n = npos(.data$hindfootLength), hf_med = med(.data$hindfootLength), hf_lo = lo(.data$hindfootLength), hf_hi = hi(.data$hindfootLength), hf_min = rmin(.data$hindfootLength), hf_max = rmax(.data$hindfootLength), hf_nflag = nflag(.data$hindfootLength),
    tl_n = npos(.data$tailLength),     tl_med = med(.data$tailLength),     tl_lo = lo(.data$tailLength),     tl_hi = hi(.data$tailLength),     tl_min = rmin(.data$tailLength),     tl_max = rmax(.data$tailLength),     tl_nflag = nflag(.data$tailLength),
    el_n = npos(.data$earLength),      el_med = med(.data$earLength),      el_lo = lo(.data$earLength),      el_hi = hi(.data$earLength),      el_min = rmin(.data$earLength),      el_max = rmax(.data$earLength),      el_nflag = nflag(.data$earLength),
    .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(.data$n_ind))
  dplyr::mutate(out, emoji = genus_emoji(.data$scientificName))
}

# Genus-level CAPTIVE maximum longevity (years), from AnAge / HAGR. Shown ONLY as
# a sanity ceiling beside the observed floor — captive maxima run ~3–10× typical
# wild lifespan, so they are always labelled "captive" and never read as a wild
# value. Chaetodipus penicillatus/eremicus aren't in AnAge; the congener
# C. formosus (7.1 yr) stands in for the genus. Genera absent here show no value.
# Sources: genomics.senescence.info — Dipodomys merriami 9.7 / D. ordii 9.9,
# Chaetodipus formosus 7.1, Perognathus flavus 4.9, Peromyscus maniculatus 8.3.
ANAGE_CAPTIVE_MAX_YR <- c(
  Dipodomys = 9.9, Chaetodipus = 7.1, Perognathus = 4.9,
  Peromyscus = 8.3, Onychomys = 4.8, Reithrodontomys = 5.0
)
genus_of <- function(sci) sub("^([A-Za-z]+).*$", "\\1", sci)

# Per-species "longest confirmed time alive" — a right-censored FLOOR, NOT a
# lifespan. The longest age-at-last-capture (approx_age_years = conservative
# age-at-first-capture + career span) among non-tag-suspect individuals with >=3
# captures, where >=5 such individuals exist. Biased LOW: animals still alive or
# that left the grid are uncounted, absence != death (death vs permanent
# emigration are indistinguishable), the record only spans the years sampled, and
# it's the single longest individual so more-trapped species reach higher floors.
# NEON keeps a tag on one animal for life (no number reuse; unique within a site),
# so a multi-year career is a REAL long-lived individual — we trust it and show
# the AnAge captive max alongside for scale.
# (Restricting to juvenile-first animals — verified — collapses this to ~0.4 yr:
# young-first animals are caught repeatedly in one season then gone, while the
# long-tracked individuals are ~96-100% adult-first. Their approx_age_years uses
# the conservative ~90 d adult-age floor, so including them stays a valid lower
# bound.) A model-based apparent-survival (CJS φ) lifespan is deliberately NOT
# computed — it needs an offline-validated session definition and would
# over-claim. One row per qualifying species-level taxon.
min_known_lifespan <- function(lb) {
  if (is.null(lb) || !nrow(lb)) return(NULL)
  if (!all(c("approx_age_years", "captures", "tag_suspect", "id_uncertain") %in% names(lb)))
    return(NULL)
  q <- dplyr::filter(lb,
    .data$captures >= 3,
    !.data$tag_suspect,
    # exclude tags recorded under >1 species — an ambiguous-ID animal shouldn't
    # anchor a species' longevity (Fauna ranks a species flip the weakest signal).
    !(.data$id_uncertain %in% TRUE),
    is.finite(.data$approx_age_years),
    !is.na(.data$scientificName))
  q <- species_level_only(q)
  if (is.null(q) || !nrow(q)) return(NULL)
  out <- q %>% dplyr::group_by(.data$scientificName) %>%
    dplyr::summarise(n_qual = dplyr::n(),
                     min_known_yr = round(max(.data$approx_age_years), 1),
                     .groups = "drop") %>%
    dplyr::filter(.data$n_qual >= 5) %>%
    dplyr::arrange(dplyr::desc(.data$min_known_yr))
  if (!nrow(out)) return(NULL)
  out$captive_max_yr <- unname(ANAGE_CAPTIVE_MAX_YR[genus_of(out$scientificName)])
  out$emoji <- genus_emoji(out$scientificName)
  out
}

# Light 3x3 smoothing of a capture-count grid -> "hotspot blur" view.
blur_grid <- function(z) {
  n <- nrow(z); m <- ncol(z)
  out <- matrix(0, n, m)
  k <- matrix(c(0.5, 1, 0.5, 1, 3, 1, 0.5, 1, 0.5), 3, 3)
  k <- k / sum(k)
  for (i in 1:n) for (j in 1:m) {
    acc <- 0; wsum <- 0
    for (di in -1:1) for (dj in -1:1) {
      ii <- i + di; jj <- j + dj
      if (ii >= 1 && ii <= n && jj >= 1 && jj <= m) {
        w <- k[di + 2, dj + 2]; acc <- acc + z[ii, jj] * w; wsum <- wsum + w
      }
    }
    out[i, j] <- acc / wsum
  }
  round(out, 2)
}

# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------
# Between-plot recapture connectivity — the "movement" the dot map throws away.
# For each individual caught at >=2 distinct plots, link its SUCCESSIVE distinct
# capture plots (consecutive same-plot recaptures collapsed), aggregate across
# animals so a plot-pair's weight = number of individuals that made that move.
# This is mark-recapture, NOT telemetry: the line means "this animal was here,
# then there," not a tracked route. Endpoints are pinned to fixed plot centroids.
# Returns list(edges, n_movers, n_tagged, n_plots, max_pair_m) or NULL.
# ---------------------------------------------------------------------------
plot_span_m <- function(cen) {
  if (is.null(cen) || nrow(cen) < 2) return(0)
  R <- 6371000; rad <- pi / 180; mx <- 0
  for (i in 1:(nrow(cen) - 1)) for (j in (i + 1):nrow(cen)) {
    dlat <- (cen$lat[j] - cen$lat[i]) * rad; dlng <- (cen$lng[j] - cen$lng[i]) * rad
    a <- sin(dlat / 2)^2 + cos(cen$lat[i] * rad) * cos(cen$lat[j] * rad) * sin(dlng / 2)^2
    mx <- max(mx, 2 * R * asin(min(1, sqrt(a))))
  }
  mx
}

recapture_edges <- function(d) {
  h <- dplyr::filter(d, !is.na(.data$tagID), !is.na(.data$plotID), !is.na(.data$date),
                     !is.na(.data$decimalLatitude), !is.na(.data$decimalLongitude))
  if (nrow(h) == 0) return(NULL)
  cen <- h %>% dplyr::group_by(.data$plotID) %>%
    dplyr::summarise(lat = mean(.data$decimalLatitude), lng = mean(.data$decimalLongitude), .groups = "drop")
  n_plots <- nrow(cen); n_tagged <- dplyr::n_distinct(h$tagID); span <- plot_span_m(cen)
  seqs <- h %>% dplyr::arrange(.data$tagID, .data$date) %>%
    dplyr::group_by(.data$tagID) %>% dplyr::summarise(ps = list(.data$plotID), .groups = "drop")
  rows <- list(); movers <- character(0)
  for (i in seq_len(nrow(seqs))) {
    pl <- rle(as.character(seqs$ps[[i]]))$values   # drop consecutive same-plot repeats, keep A->B->A
    if (length(pl) < 2) next
    movers <- c(movers, seqs$tagID[i])
    for (j in seq_len(length(pl) - 1)) {
      ab <- sort(c(pl[j], pl[j + 1]))              # unordered pair as columns (no delimiter round-trip)
      rows[[length(rows) + 1]] <- data.frame(a = ab[1], b = ab[2],
        tag = as.character(seqs$tagID[i]), stringsAsFactors = FALSE)
    }
  }
  base <- list(edges = NULL, n_movers = length(unique(movers)),
               movers = unique(movers),
               n_tagged = n_tagged, n_plots = n_plots, max_pair_m = span, cen = cen)
  if (!length(rows)) return(base)
  pr <- do.call(rbind, rows)
  agg <- pr %>% dplyr::group_by(.data$a, .data$b) %>%
    dplyr::summarise(n_movers = dplyr::n_distinct(.data$tag), .groups = "drop")
  ca <- cen[match(agg$a, cen$plotID), ]; cb <- cen[match(agg$b, cen$plotID), ]
  base$edges <- data.frame(plot_a = agg$a, plot_b = agg$b,
    lat0 = ca$lat, lng0 = ca$lng, lat1 = cb$lat, lng1 = cb$lng,
    n_movers = agg$n_movers, stringsAsFactors = FALSE)
  base
}

# Capture-level detail for a set of individuals — feeds the QC "inspect" modals.
# Ordered by tag then date, with the per-capture fields a user needs to judge
# whether a tag is one animal or a mix-up: date, plot, species, sex, life stage.
inspect_captures <- function(d, tags) {
  if (is.null(d) || !length(tags)) return(NULL)
  # same population recapture_edges() used to flag movers (needs a plot + coords),
  # so the modal's plot count / same-day check can't disagree with the map.
  h <- dplyr::filter(d, .data$tagID %in% tags, !is.na(.data$date), !is.na(.data$plotID),
                     !is.na(.data$decimalLatitude), !is.na(.data$decimalLongitude))
  if (!nrow(h)) return(NULL)
  h %>% dplyr::arrange(.data$tagID, .data$date) %>%
    dplyr::transmute(short = short_tag(.data$tagID),
                     date = .data$date, plotID = .data$plotID,
                     scientificName = .data$scientificName,
                     sex = .data$sex, lifeStage = .data$lifeStage)
}

# ---------------------------------------------------------------------------
# QC-flag lifecycle — the ONE possible-error rule, shared everywhere
# ---------------------------------------------------------------------------
# A value flagged as a possible data-entry error must be flagged the SAME way in
# every place it appears: the species table's ⚠, the QC modal, the per-capture
# marks on the dossier plots (measPlot / morphoPlot / capHistory), AND the Chonk
# Index exclusion. This single rule is that source of truth — |x − median| > 5·MAD
# among the reference pool, MAD floored at 10% of the median so a tight integer
# distribution can't manufacture a zero-MAD "everything is an outlier". Robust
# (median/MAD) so the outlier can't mask itself.
.MEAS_COL <- c(weight = "weight", hindfoot = "hindfootLength",
               tail = "tailLength", ear = "earLength")
.is_meas_outlier <- function(v, ref) {
  ref <- ref[is.finite(ref) & ref > 0]
  if (length(ref) < 3) return(is.finite(v) & FALSE)
  m <- stats::median(ref); s <- max(stats::mad(ref), 0.1 * m)
  if (!is.finite(s) || s <= 0) return(is.finite(v) & FALSE)
  is.finite(v) & (abs(v - m) > 5 * s)
}

# The species-level ADULT reference distribution for one measure — the exact pool
# the flag is judged against (matches species_measurements()'s nflag). The dossier
# plots pass an animal's own capture values against this so a mark on the plot is
# the SAME record the species-table ⚠ and the QC modal flag.
species_adult_values <- function(d, sp, measure) {
  col <- .MEAS_COL[[measure]]
  if (is.null(col) || is.null(d) || length(sp) != 1 || is.na(sp)) return(numeric(0))
  h <- species_level_only(dplyr::filter(d, !is.na(.data$tagID),
         .data$scientificName == sp, .data$lifeStage %in% "adult"))
  if (is.null(h) || !nrow(h)) return(numeric(0))
  v <- h[[col]]; v[is.finite(v) & v > 0]
}

# The actual captures whose body measurement was flagged as a possible error, so
# the QC modal can list the exact records to verify AND drill to the animal's
# dossier. Returns (tag = full tagID for the drill-through, short, date, plot,
# value, sex, median) or NULL.
flagged_measure_captures <- function(d, sp, measure) {
  col <- .MEAS_COL[[measure]]
  if (is.null(col) || is.null(d)) return(NULL)
  h <- species_level_only(dplyr::filter(d, !is.na(.data$tagID), .data$scientificName == sp,
                          .data$lifeStage %in% "adult"))
  if (is.null(h) || !nrow(h)) return(NULL)
  v <- h[[col]]; keep <- is.finite(v) & v > 0
  h <- h[keep, , drop = FALSE]; v <- v[keep]
  if (length(v) < 3) return(NULL)
  flag <- .is_meas_outlier(v, v)
  if (!any(flag)) return(NULL)
  data.frame(tag = h$tagID[flag], short = short_tag(h$tagID[flag]),
             date = h$date[flag], plotID = h$plotID[flag],
             value = round(v[flag], 1), sex = h$sex[flag],
             median = stats::median(v), stringsAsFactors = FALSE)
}

# ---------------------------------------------------------------------------
# individual_history(): every capture event for ONE tagged animal, ordered by
# date, with all the body measurements ("meso" morphometrics) + field context a
# QC reviewer needs. Unlike inspect_captures() (which requires coords, for the
# map-consistent mover modal) this keeps EVERY dated capture so the card never
# silently drops a record. Missing columns degrade to NA so a live-fetched table
# with a different shape can't error the card.
# ---------------------------------------------------------------------------
# Columns a per-animal history carries. pregnancyStatus and nipples are read by
# the weight-jump flag (reproductive females gain and lose mass fast); they sit
# at the end so the existing column order of the QC CSV is unchanged.
.HIST_COLS <- c("plotID", "trapCoordinate", "lifeStage", "sex", "scientificName",
                "weight", "hindfootLength", "tailLength", "earLength", "totalLength",
                "recapture", "fate", "pregnancyStatus", "nipples")
.history_rows <- function(h) {
  h <- h[!is.na(h$date), , drop = FALSE]
  if (!nrow(h)) return(NULL)
  for (cc in .HIST_COLS) if (!cc %in% names(h)) h[[cc]] <- NA
  h[order(h$date), c("date", .HIST_COLS), drop = FALSE]
}
individual_history <- function(d, tag) {
  if (is.null(d) || is.null(tag) || length(tag) != 1 || is.na(tag) || tag == "") return(NULL)
  .history_rows(d[which(d$tagID == tag), , drop = FALSE])
}

# exact-token reproductive state, the same tokens flag_repro() uses (never
# substring matching: "not pregnant" contains "pregnant")
.tok <- function(x) tolower(trimws(as.character(x)))
.is_pregnant <- function(x) .tok(x) %in% "pregnant"
.is_lactating <- function(x) .tok(x) %in% "enlarged"

# ---------------------------------------------------------------------------
# individual_qc_flags(): the ranked QC signals for one animal's capture history,
# returned as a list of list(level, text, code, dates). level is one of "high",
# "warn", "info"; code is a stable machine name; dates are the capture dates the
# flag points at (NULL when it is about the whole record). Ranking follows the
# Fauna review: the most reliable error signals are "high" (same tag at two
# plots on one day, a two-level or repeated life-stage regression, a span beyond
# any wild lifespan); suggestive ones are "warn" (sex flip, adult weight jump, a
# single one-step stage regression, a cross-genus species change); weak ones are
# "info" (hind-foot spread, a within-genus species change). Every flag is
# phrased as "verify", not "wrong".
# `hist` = individual_history(d, tag); `lb_row` = the build_leaderboard() row.
# ---------------------------------------------------------------------------
individual_qc_flags <- function(hist, lb_row) {
  flags <- list()
  add <- function(level, text, code, dates = NULL)
    flags[[length(flags) + 1L]] <<- list(level = level, text = text, code = code,
                                         dates = if (length(dates)) sort(unique(as.Date(dates))) else NULL)
  if (is.null(hist) || !nrow(hist)) return(flags)
  has_row <- !is.null(lb_row) && nrow(lb_row) == 1
  fi <- function(x) format(x, big.mark = ",")
  col <- function(nm) if (nm %in% names(hist)) hist[[nm]] else rep(NA, nrow(hist))

  # Caught once -> there are no recaptures to cross-check, so the consistency
  # checks below never run. Say that, rather than falling through to a green
  # "all consistent" that would over-claim a check that never happened.
  if (nrow(hist) < 2) {
    add("info", "Caught once, so there are no recaptures to cross-check and the consistency flags below don't apply.",
        "caught_once")
    return(flags)
  }

  # 1 — same tag, two plots, same day: spatially impossible (highest-confidence).
  if (has_row && isTRUE(lb_row$spatial_conflict[1])) {
    ok <- !is.na(hist$date) & !is.na(hist$plotID)
    np <- tapply(hist$plotID[ok], as.character(hist$date[ok]), function(p) length(unique(p)))
    add("high", "Recorded at two plots on the same day, which is physically impossible for one animal (plots are hundreds of metres apart). Almost always a tag-number mix-up or data-entry error.",
        "same_day_two_plots", names(np)[np > 1])
  }

  # 2 — life stage moving backward across DISTINCT dates. Collapse same-day rows
  #     to that day's most-advanced stage first, so a same-day (adult, juvenile)
  #     pair (or a two-plot-one-day record) can't be misread as a regression.
  #     "high" when the stage drops two levels (adult to juvenile) or regresses
  #     at least twice; a single one-step drop is "warn", because the adult vs
  #     subadult (and subadult vs juvenile) call is a field judgment.
  ord <- c(juvenile = 1L, subadult = 2L, adult = 3L)
  hs <- data.frame(date = hist$date, st = unname(ord[as.character(hist$lifeStage)]))
  hs <- hs[!is.na(hs$st) & !is.na(hs$date), , drop = FALSE]
  if (nrow(hs) >= 2) {
    per_day <- tapply(hs$st, as.character(hs$date), max)
    per_day <- per_day[order(as.Date(names(per_day)))]
    dd <- diff(per_day)
    if (length(per_day) >= 2 && any(dd < 0)) {
      ix <- which(dd < 0)
      dts <- c(names(per_day)[ix], names(per_day)[ix + 1L])
      if (any(dd <= -2L) || length(ix) >= 2L) {
        add("high", "Life stage moves backward on a later date (adult then juvenile, or more than one step back). An animal cannot get younger, so one of these records is wrong. Check the staging on the datasheet.",
            "stage_regression", dts)
      } else if (per_day[ix + 1L] == 2L) {
        add("warn", "Life stage is recorded as younger on a later date (for example adult, then subadult). The adult and subadult call is a field judgment, so one of these records is likely mis-staged; check the datasheet.",
            "stage_regression", dts)
      } else {
        add("warn", "Life stage is recorded as younger on a later date (subadult, then juvenile). The subadult and juvenile call is a field judgment, so one of these records is likely mis-staged; check the datasheet.",
            "stage_regression", dts)
      }
    }
  }

  # 3 — career span beyond any wild lifespan for these genera (>5 yr).
  if (has_row && isTRUE(lb_row$career_days[1] > 1825))
    add("high", sprintf("Career span of %s days exceeds any wild lifespan for these species (>5 yr); verify the tag number isn't shared between two animals.", fi(lb_row$career_days[1])),
        "career_span", range(hist$date, na.rm = TRUE))

  # 4 — sex flip across recaptures: field sexing is error-prone, so flag (don't condemn).
  sx_all <- col("sex")
  sx <- unique(sx_all[sx_all %in% c("M", "F")])
  if (length(sx) > 1) {
    tb <- table(sx_all[sx_all %in% c("M", "F")])
    minority <- if (tb[["M"]] == tb[["F"]]) c("M", "F") else names(tb)[which.min(tb)]
    add("warn", "Sex was recorded as both M and F across captures. Field sexing is error-prone (especially non-reproductive animals), so this is worth a check, not necessarily an error.",
        "sex_flip", hist$date[sx_all %in% minority])
  }

  # 5 — implausible weight jump: percent change FROM THE PRIOR capture (a fixed,
  #     reproducible baseline — not pmin, which would inflate the %), over a real
  #     interval of >=1 day and <=30 days, and ONLY when the prior capture was an
  #     adult — a juvenile/subadult can legitimately add >30% mass in under a
  #     month, so the flag would otherwise just relabel normal growth as a typo.
  #     Reproduction: a pair is skipped when she gained and either capture is
  #     reproductive (pregnant, or nipples enlarged), or when the prior capture
  #     was pregnant and she lost (gave birth). Exact tokens, as flag_repro().
  wok <- is.finite(hist$weight) & hist$weight > 0
  if (sum(wok) >= 2) {
    ww <- data.frame(date = hist$date[wok], weight = hist$weight[wok],
                     lifeStage = col("lifeStage")[wok],
                     preg = .is_pregnant(col("pregnancyStatus")[wok]),
                     lact = .is_lactating(col("nipples")[wok]))
    ww <- ww[order(ww$date), ]
    n <- nrow(ww)
    prevw <- ww$weight[-n]; nextw <- ww$weight[-1]
    dd <- as.numeric(diff(ww$date))
    young <- ww$lifeStage[-n] %in% c("juvenile", "subadult")
    repro <- ww$preg | ww$lact
    pct <- (nextw - prevw) / prevw * 100
    skip <- (pct > 0 & (repro[-n] | repro[-1])) | (ww$preg[-n] & pct < 0)
    bad <- which(is.finite(pct) & abs(pct) > 30 & dd >= 1 & dd <= 30 & !young & !skip)
    if (length(bad)) {
      i <- bad[which.max(abs(pct[bad]))]
      add("warn", sprintf("Weight changed %+.0f%% from the prior capture (%.1f → %.1f g) in %d day%s (%s → %s); a swing this fast in an adult can be a transposed digit, so check it against the capture table and reproductive state.",
        pct[i], prevw[i], nextw[i], dd[i], ifelse(dd[i] == 1, "", "s"),
        format(ww$date[i], "%Y-%m-%d"), format(ww$date[i + 1], "%Y-%m-%d")),
        "weight_jump", c(ww$date[i], ww$date[i + 1]))
    }
  }

  # 6 — hind-foot spread among ADULT recaptures: foot length is near-fixed in
  #     adults, so a wide spread usually reflects measurement differences. Use a
  #     >3 mm threshold (above mm-rounding jitter) and require >=3 adult captures
  #     so one odd remeasure on a 2-capture animal doesn't over-fire.
  ad <- hist[hist$lifeStage %in% "adult" & is.finite(hist$hindfootLength) & hist$hindfootLength > 0, , drop = FALSE]
  if (nrow(ad) >= 3) {
    rng <- diff(range(ad$hindfootLength))
    if (rng > 3)
      add("info", sprintf("Hind-foot length spans %.0f mm across %d adult recaptures. Adult foot length is near-fixed, so a spread this wide usually reflects measurement differences (ruler vs caliper, a different tech, foot flex) rather than growth.", rng, nrow(ad)),
          "hindfoot_spread",
          ad$date[ad$hindfootLength %in% range(ad$hindfootLength)])
  }

  # 7 — same tag, more than one species-level ID. A cross-genus change is a
  #     "warn"; a within-genus swap (common, low concern) stays "info".
  if (has_row && isTRUE(lb_row$id_uncertain[1])) {
    sp <- species_level_only(hist[!is.na(hist$scientificName), , drop = FALSE])
    modal <- mode_chr(sp$scientificName)
    dts <- sp$date[!is.na(sp$scientificName) & sp$scientificName != modal]
    ng <- if ("n_genus_ids" %in% names(lb_row)) lb_row$n_genus_ids[1] else NA
    if (isTRUE(ng > 1))
      add("warn", "This tag was recorded under species in more than one genus. A cross-genus change is unlikely to be a simple congener swap, so check the datasheet (NEON reconciles IDs later via its identification history).",
          "multi_species_id", dts)
    else
      add("info", "This tag was recorded under more than one species of the same genus. Within-genus swaps are common and low concern (NEON reconciles IDs later via its identification history).",
          "multi_species_id", dts)
  }

  flags
}

# Suite-standard name for the per-animal QC flag function.
mammal_qc <- individual_qc_flags

QC_LEVELS <- c("high", "warn", "info")

# capture_qc_flags(): one `qc_flags` text per capture row for the per-animal QC
# history CSV. Each flag from individual_qc_flags() is written onto the rows of
# the dates it points at, as "code (level)", joined with "; ". When `d` (the
# site's clean_mam() table) is given, adult measurements outside the shared
# possible-error rule (.is_meas_outlier against the species' adult pool, the
# rule behind the warning triangle and the Chonk exclusion) are added as
# "measurement_outlier_<measure> (warn)". Rows with no flag get "".
# Call: h <- individual_history(d, tag)
#       h <- capture_qc_flags(h, individual_qc_flags(h, lb_row), d)
capture_qc_flags <- function(hist, flags, d = NULL) {
  if (is.null(hist) || !nrow(hist)) return(hist)
  lab <- vector("list", nrow(hist))
  for (f in flags) {
    if (is.null(f$dates) || !length(f$dates)) next
    hit <- which(hist$date %in% f$dates)
    for (i in hit) lab[[i]] <- c(lab[[i]], sprintf("%s (%s)", f$code, f$level))
  }
  if (!is.null(d)) {
    adult <- hist$lifeStage %in% "adult"
    for (ms in names(.MEAS_COL)) {
      v <- suppressWarnings(as.numeric(hist[[.MEAS_COL[[ms]]]]))
      for (sp in unique(hist$scientificName[adult & is.finite(v)])) {
        rows <- which(adult & is.finite(v) & hist$scientificName %in% sp)
        ref <- species_adult_values(d, sp, ms)
        out <- rows[.is_meas_outlier(v[rows], ref)]
        for (i in out) lab[[i]] <- c(lab[[i]], sprintf("measurement_outlier_%s (warn)", ms))
      }
    }
  }
  hist$qc_flags <- vapply(lab, function(x) paste(unique(x), collapse = "; "), character(1))
  hist
}

# site_qc_report(): one row per flagged record across the site, with columns
# flag, level ("high", "warn", "info"), tagID, date, plotID, scientificName,
# measure, value, rule. Three sources, each the same rule the app shows:
#  1. measurement outliers: .is_meas_outlier per species-level species and
#     measure on adult captures (flagged_measure_captures; the warning triangle
#     and the Chonk exclusion);
#  2. cross-grid movers with an identity tell (sex change, species change, or
#     two plots on one day), the rule of the movers inspector; one row per tag;
#  3. individual_qc_flags() for every marked tag, one row per implicated date
#     (date NA for a flag about the whole record). "caught_once" is not a flag
#     on a record and is left out.
# History rows are split once by tag, so a site with ~4,700 tags runs in seconds.
site_qc_report <- function(d, lb = NULL) {
  empty <- data.frame(flag = character(), level = character(), tagID = character(),
                      date = as.Date(character()), plotID = character(),
                      scientificName = character(), measure = character(),
                      value = character(), rule = character(), stringsAsFactors = FALSE)
  if (is.null(d) || !nrow(d)) return(empty)
  out <- list()
  # 1 — measurement outliers
  sps <- sort(unique(species_level_only(d[!is.na(d$tagID) & !is.na(d$scientificName) &
                                          d$lifeStage %in% "adult", , drop = FALSE])$scientificName))
  for (sp in sps) for (ms in names(.MEAS_COL)) {
    fm <- flagged_measure_captures(d, sp, ms)
    if (is.null(fm) || !nrow(fm)) next
    out[[length(out) + 1L]] <- data.frame(
      flag = "measurement_outlier", level = "warn", tagID = fm$tag, date = fm$date,
      plotID = fm$plotID, scientificName = sp, measure = ms, value = as.character(fm$value),
      rule = sprintf("Adult %s more than 5 x MAD from the median (%.1f) of adult %s; MAD floored at 10%% of the median.",
                     ms, fm$median[1], sp), stringsAsFactors = FALSE)
  }
  # 2 — movers with an identity tell
  rf <- tryCatch(recapture_edges(d), error = function(e) NULL)
  if (!is.null(rf) && length(rf$movers)) {
    h <- d[d$tagID %in% rf$movers & !is.na(d$date) & !is.na(d$plotID) &
             !is.na(d$decimalLatitude) & !is.na(d$decimalLongitude), , drop = FALSE]
    for (g in split(h, h$tagID)) {
      sexes <- unique(g$sex[g$sex %in% c("M", "F")])
      spp <- unique(g$scientificName[!is.na(g$scientificName)])
      sameday <- any(tapply(g$plotID, g$date, function(p) length(unique(p))) > 1, na.rm = TRUE)
      tells <- c(if (length(sexes) > 1) "sex changes", if (length(spp) > 1) "species changes",
                 if (isTRUE(sameday)) "same day at 2 plots")
      if (!length(tells)) next
      out[[length(out) + 1L]] <- data.frame(
        flag = "mover_identity", level = "warn", tagID = g$tagID[1], date = as.Date(NA),
        plotID = paste(sort(unique(g$plotID)), collapse = ","),
        scientificName = mode_chr(g$scientificName), measure = "identity",
        value = paste(tells, collapse = "; "),
        rule = "Caught at 2+ grids with a sex change, species change or same-day two-plot record: possibly two animals sharing a tag number.",
        stringsAsFactors = FALSE)
    }
  }
  # 3 — per-animal flags
  if (is.null(lb)) lb <- build_leaderboard(d)
  if (!is.null(lb) && nrow(lb)) {
    keep <- mam_tag_marked(d$tagID) & !is.na(d$date)
    cols <- intersect(c("tagID", "date", .HIST_COLS), names(d))
    base <- as.data.frame(d[keep, cols, drop = FALSE])
    pieces <- split(base, base$tagID)
    pieces <- pieces[vapply(pieces, nrow, integer(1)) >= 2L]   # caught once: nothing to cross-check
    meas_of <- c(same_day_two_plots = "plotID", stage_regression = "lifeStage",
                 career_span = "career_days", sex_flip = "sex", weight_jump = "weight",
                 hindfoot_spread = "hindfootLength", multi_species_id = "scientificName")
    li <- match(names(pieces), lb$tagID)
    for (k in seq_along(pieces)) {
      if (is.na(li[k])) next
      row <- lb[li[k], , drop = FALSE]
      hist <- .history_rows(pieces[[k]])
      fl <- individual_qc_flags(hist, row)
      for (f in fl) {
        if (identical(f$code, "caught_once")) next
        dts <- if (length(f$dates)) f$dates else as.Date(NA)
        mcol <- meas_of[[f$code]]
        vals <- vapply(seq_along(dts), function(j) {
          if (mcol == "career_days") return(as.character(row$career_days[1]))
          r <- hist[!is.na(dts[j]) & hist$date == dts[j], , drop = FALSE]
          if (!nrow(r)) return(NA_character_)
          paste(unique(as.character(r[[mcol]])), collapse = ",")
        }, character(1))
        pl <- vapply(seq_along(dts), function(j) {
          r <- hist$plotID[!is.na(dts[j]) & hist$date == dts[j]]
          if (length(r)) paste(sort(unique(r)), collapse = ",") else NA_character_
        }, character(1))
        out[[length(out) + 1L]] <- data.frame(
          flag = f$code, level = f$level, tagID = row$tagID[1], date = dts, plotID = pl,
          scientificName = row$scientificName[1], measure = mcol, value = vals,
          rule = f$text, stringsAsFactors = FALSE)
      }
    }
  }
  if (!length(out)) return(empty)
  res <- do.call(rbind, out)
  stopifnot(all(res$level %in% QC_LEVELS))
  res <- res[order(match(res$level, QC_LEVELS), res$flag, res$tagID, res$date), , drop = FALSE]
  rownames(res) <- NULL
  res
}

# Quadratic-Bezier arc points between two lon/lat endpoints (a curved connector,
# so a line on a satellite tile never reads as a walked straight route). `bow`
# offsets the control point perpendicular to the chord.
arc_xy <- function(lng0, lat0, lng1, lat1, n = 24, bow = 0.18) {
  mx <- (lng0 + lng1) / 2; my <- (lat0 + lat1) / 2
  dx <- lng1 - lng0; dy <- lat1 - lat0
  cx <- mx - dy * bow; cy <- my + dx * bow
  t <- seq(0, 1, length.out = n)
  list(lng = (1 - t)^2 * lng0 + 2 * (1 - t) * t * cx + t^2 * lng1,
       lat = (1 - t)^2 * lat0 + 2 * (1 - t) * t * cy + t^2 * lat1)
}

# Population index: Minimum Number Known Alive (MNKA; Krebs 1966) + CPUE.
# An individual is "known alive" in every monthly session between its first and
# last capture in a plot (even sessions it was missed). Returns one row per
# (plotID, ym) with mnka and captures-per-100-trap-nights. When a species is
# requested, capture spans/counts are species-specific but effort still comes from
# the FULL site trap-opportunity frame; filtering rows before computing effort
# would make the denominator outcome-conditioned.
# ---------------------------------------------------------------------------
mnka_series <- function(d, scientific_name = NULL) {
  h <- dplyr::filter(d, .data$is_capture, !is.na(.data$ym), !is.na(.data$plotID))
  if (!is.null(scientific_name))
    h <- dplyr::filter(h, !is.na(.data$scientificName),
                      .data$scientificName == scientific_name)
  if (nrow(h) == 0) return(NULL)
  span <- h %>% dplyr::group_by(.data$plotID, .data$tagID) %>%
    dplyr::summarise(first = min(.data$ym), last = max(.data$ym), .groups = "drop")
  cap_counts <- h %>% dplyr::group_by(.data$plotID, .data$ym) %>%
    dplyr::summarise(captures = dplyr::n(), .groups = "drop")
  eff <- d %>% dplyr::filter(!is.na(.data$ym), !is.na(.data$plotID)) %>%
    dplyr::group_by(.data$plotID, .data$ym) %>%
    dplyr::summarise(trap_nights = sum(.data$trap_effort, na.rm = TRUE),
                     .groups = "drop") %>%
    dplyr::left_join(cap_counts, by = c("plotID", "ym")) %>%
    dplyr::mutate(captures = dplyr::coalesce(.data$captures, 0L))
  # Drop plot-months with ZERO trap effort: NEON ships non-trapping records (e.g.
  # the COVID-2020 pause, trap-status-only rows) that carry a plotID+ym but no
  # actual sampling. Left in, they give CPUE = 100*0/0 = NaN, which shatters the
  # site-total line into scattered single-month breaks (the SRER "spotty" bug),
  # and add phantom points to the per-plot MNKA lines. A no-effort month is not
  # a sampling event, so it isn't a data point.
  eff <- eff[is.finite(eff$trap_nights) & eff$trap_nights > 0, , drop = FALSE]
  if (nrow(eff) == 0) return(NULL)
  out <- eff %>% dplyr::rowwise() %>%
    dplyr::mutate(mnka = sum(span$plotID == .data$plotID &
                             span$first <= .data$ym & span$last >= .data$ym)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(cpue = dplyr::if_else(.data$trap_nights > 0,
                                        round(100 * .data$captures / .data$trap_nights, 1), NA_real_),
                  date = as.Date(paste0(.data$ym, "-01")))
  # edge: the first and last sampled month of each plot's series. MNKA counts an
  # animal only between its first and last capture, so these months are biased
  # low (an animal first caught later, or last caught earlier, is uncounted).
  out <- out %>% dplyr::group_by(.data$plotID) %>%
    dplyr::mutate(edge = .data$ym == min(.data$ym) | .data$ym == max(.data$ym)) %>%
    dplyr::ungroup()
  # incomplete_grid: TRUE when any trapped night of that plot-month carries a
  # gridCompletion token mapped FALSE. FALSE when the field is absent.
  out$incomplete_grid <- FALSE
  if ("gridCompletion" %in% names(d)) {
    e <- suppressWarnings(as.numeric(d$trap_effort))
    tr <- !is.na(d$ym) & !is.na(d$plotID) & is.finite(e) & e > 0
    g <- mam_grid_complete(d$gridCompletion[tr])
    bad <- unique(paste(d$plotID[tr], d$ym[tr], sep = "|")[g %in% FALSE])
    out$incomplete_grid <- paste(out$plotID, out$ym, sep = "|") %in% bad
  }
  out
}

# Insert one all-NA row after any gap of more than `max_gap_months` between
# consecutive sampled months (per `group`, e.g. plotID; NULL = one series), so a
# plotly line (connectgaps = FALSE, the default) breaks across a seasonal pause
# instead of drawing a straight bridge. The inserted row has the group value and
# a date one month after the last sampled month; every column in `value_cols`
# (and every other column) is NA. Use on mnka_series() output before add_trace.
insert_gap_breaks <- function(df, max_gap_months = 2L, group = "plotID",
                              value_cols = c("mnka", "cpue")) {
  if (is.null(df) || !nrow(df)) return(df)
  df <- as.data.frame(df)
  gkey <- if (is.null(group)) rep("all", nrow(df)) else as.character(df[[group]])
  mi <- function(dt) as.integer(format(dt, "%Y")) * 12L + as.integer(format(dt, "%m"))
  pieces <- lapply(split(seq_len(nrow(df)), gkey), function(ix) {
    g <- df[ix[order(df$date[ix])], , drop = FALSE]
    if (nrow(g) < 2) return(g)
    jump <- which(diff(mi(g$date)) > max_gap_months)
    if (!length(jump)) return(g)
    br <- g[jump, , drop = FALSE]
    keep <- if (is.null(group)) character(0) else group
    for (cc in setdiff(names(br), c(keep, "date"))) br[[cc]] <- br[[cc]][NA_integer_]
    lt <- as.POSIXlt(br$date); lt$mon <- lt$mon + 1L; br$date <- as.Date(lt)
    if ("ym" %in% names(br)) br$ym <- format(br$date, "%Y-%m")
    out <- rbind(g, br)
    out[order(out$date), , drop = FALSE]
  })
  res <- do.call(rbind, unname(pieces))
  rownames(res) <- NULL
  res
}

# Keep only confirmed SPECIES-level identifications — drop genus-only "X sp."
# and ambiguous "A/B" records so an unidentified "Rodentia sp." isn't counted as
# its own species. This matters most for Chao1 (which scales with the singleton
# count, so each phantom "sp." inflates the estimate), but also for plain
# richness and the Hill profile. Uses NEON's taxonRank when present (the robust
# discriminator) with a scientific-name regex as a backstop. Mirrors the filter
# in build_site_index.R so the map, the stat cards, the diversity profile and the
# accumulation curve all agree on ONE species list. (Quinn review.)
#
# identificationQualifier: NEON marks an uncertain ID with cf. (compare) or
# aff. (has affinity to) plus the rank the doubt applies to. Tokens seen across
# the 46 committed bundles: cf. species (45,368 rows), cf. genus (354),
# cf. variety (219), cf. subspecies (47), aff. species (32), cf. family (29),
# aff. genus (3), aff. subspecies (1), aff. variety (1). A doubt at genus or
# family rank means the name is not a species-level ID, so those rows are
# dropped here. cf./aff. species and below stay species-level; see
# species_confirmed_only() for the stricter list.
MAM_ID_QUALIFIER_NOT_SPECIES <- c("cf. genus", "cf. family", "aff. genus")
MAM_ID_QUALIFIER_SPECIES_DOUBT <- c("cf. species", "aff. species")
.id_qualifier <- function(h) {
  if (!"identificationQualifier" %in% names(h)) return(rep(NA_character_, nrow(h)))
  tolower(trimws(as.character(h$identificationQualifier)))
}
species_level_only <- function(h) {
  if (is.null(h) || nrow(h) == 0) return(h)
  rank <- if ("taxonRank" %in% names(h)) h$taxonRank else rep(NA_character_, nrow(h))
  rank_ok <- is.na(rank) | rank %in% c("species", "subspecies", "speciesGroup")
  nm <- ifelse(is.na(h$scientificName), "", as.character(h$scientificName))
  ambiguous <- grepl("\\bsp\\.?$", nm) | grepl("/", nm, fixed = TRUE)
  coarse_doubt <- .id_qualifier(h) %in% MAM_ID_QUALIFIER_NOT_SPECIES
  h[rank_ok & !ambiguous & !coarse_doubt, , drop = FALSE]
}

# species_level_only() minus cf. species / aff. species rows: identifications
# with no doubt at species rank. Used for community_stats()$species_confirmed.
species_confirmed_only <- function(h) {
  h <- species_level_only(h)
  if (is.null(h) || nrow(h) == 0) return(h)
  h[!(.id_qualifier(h) %in% MAM_ID_QUALIFIER_SPECIES_DOUBT), , drop = FALSE]
}

# Sample-based species accumulation over monthly bouts (Gotelli & Colwell 2001),
# averaged over permutations, + a bias-corrected Chao1 asymptotic richness
# estimate with a 95% CI (Chao 1987; Chao & Chiu 2016). Counts species-level IDs
# only; Chao1 is a LOWER BOUND and is flagged unstable when doubletons are scarce.
# The band is the mean +/- 1 SD over `perms` random bout orderings drawn with a
# fixed `seed` (the session RNG is restored afterwards), so it is reproducible.
species_accum <- function(d, perms = 100, seed = 20261022L) {
  h <- dplyr::filter(d, !is.na(.data$tagID), !is.na(.data$scientificName), !is.na(.data$ym))
  h <- species_level_only(h)                       # drop "X sp." / "A/B" so they aren't phantom species
  if (nrow(h) == 0) return(NULL)
  bouts <- split(h$scientificName, h$ym)
  k <- length(bouts)
  # seeded uniform shuffles of the bouts; matrix() keeps a one-month window
  # (k = 1) two-dimensional for rowMeans/apply
  perm_mat <- .with_seed(seed, matrix(nrow = k, vapply(seq_len(perms), function(p) {
    ord <- sample.int(k)
    seen <- character(0); rich <- integer(k)
    for (i in seq_len(k)) { seen <- union(seen, bouts[[ord[i]]]); rich[i] <- length(seen) }
    rich
  }, integer(k))))
  mean_rich <- rowMeans(perm_mat)
  sd_rich   <- apply(perm_mat, 1, stats::sd)

  # f1 / f2 = species represented by exactly 1 / 2 distinct MARKED individuals
  # (real ear tags; a placeholder ID is a capture event, not an animal).
  # sobs stays on every capture, so a species seen only untagged still counts.
  sobs <- dplyr::n_distinct(h$scientificName)
  cnt <- h[mam_tag_marked(h$tagID), , drop = FALSE] %>%
    dplyr::distinct(.data$tagID, .data$scientificName) %>%
    dplyr::count(.data$scientificName)
  f1 <- sum(cnt$n == 1); f2 <- sum(cnt$n == 2)
  # Bias-corrected Chao1 (exact at f2==0, lower-variance than the classic
  # f1^2/(2 f2) form which is upward-biased & unstable for small f2).
  chao1 <- sobs + (f1 * (f1 - 1)) / (2 * (f2 + 1))
  # Chao (1987) log-normal 95% CI on the # of undetected species, + instability flag.
  T_extra <- chao1 - sobs
  if (f2 > 0) {
    r <- f1 / f2
    varC <- f2 * (0.5 * r^2 + r^3 + 0.25 * r^4)
  } else {
    # bias-corrected form at f2 = 0 (Chao 2005; Chao & Chiu 2016), with no
    # (n-1)/n factor, matching the f2 > 0 branch above
    varC <- max(f1 * (f1 - 1) / 2 + f1 * (2 * f1 - 1)^2 / 4 - f1^4 / (4 * max(chao1, 1)), 0)
  }
  if (T_extra > 0 && varC > 0) {
    K  <- exp(1.96 * sqrt(log(1 + varC / T_extra^2)))
    lo <- sobs + T_extra / K
    hi <- sobs + T_extra * K
  } else { lo <- sobs; hi <- chao1 }
  unstable <- f2 < 5   # too few doubletons -> treat the estimate as a soft lower bound

  list(curve = tibble::tibble(bouts = seq_len(k), richness = mean_rich,
                              lo = pmax(0, mean_rich - sd_rich), hi = mean_rich + sd_rich),
       sobs = sobs, chao1 = round(chao1),
       chao_lo = round(lo), chao_hi = round(ceiling(hi)),
       f1 = f1, f2 = f2, unstable = unstable)
}

# ---------------------------------------------------------------------------
# Hill numbers — the "effective number of species" diversity profile.
# (Hill 1973; Jost 2006; Chao et al. 2014, Annu. Rev. Ecol. Evol. Syst.)
# A unified family indexed by q, all in the same intuitive unit (species):
#   q0 = species richness            (counts every species equally)
#   q1 = exp(Shannon entropy)        = effective # of "common" species
#   q2 = inverse Simpson 1/Σpᵢ²      = effective # of "dominant" species
# Higher q downweights rare species, so q0 ≥ q1 ≥ q2 always. When q1/q0 is near
# 1 the community is even; when it's small a few species dominate.
# Abundance = distinct MARKED individuals per species (real ear tags, not
# captures and not placeholder capture-event IDs), so a heavily re-trapped
# animal isn't double-counted and an untagged capture is not an animal. q0 is
# the species with at least one marked animal, so a species seen only untagged
# is in richness elsewhere but not in this profile.
# ---------------------------------------------------------------------------
## Monthly breeding phenology, deduped to one row per (individual, calendar
## month) so recaptures don't inflate the n or the proportion (matches the
## donut/violin dedup discipline). Completed to all 12 months — an unsampled
## month is an explicit NA gap; months with <5 sexed adults -> NA (suppressed).
## Returns a 12-row tibble(mon, males, females, breeding_m, repro_f, pm, pf) or
## NULL. Shared by the phenology chart AND its answer-banner so they can't drift.
repro_by_month <- function(d) {
  ad <- flag_repro(dplyr::filter(d, !is.na(.data$tagID), .data$lifeStage == "adult", !is.na(.data$date)))
  if (nrow(ad) == 0) return(NULL)
  ad$mon <- as.integer(format(ad$date, "%m"))
  im <- ad %>% dplyr::group_by(.data$tagID, .data$mon) %>% dplyr::summarise(
    sex = mode_chr(.data$sex),
    bred_m = as.integer(any(.data$repro == "breeding male", na.rm = TRUE)),
    repr_f = as.integer(any(.data$repro %in% c("pregnant female", "lactating/receptive female"), na.rm = TRUE)),
    .groups = "drop")
  by_m <- im %>% dplyr::group_by(.data$mon) %>% dplyr::summarise(
    males = sum(.data$sex == "M", na.rm = TRUE),
    females = sum(.data$sex == "F", na.rm = TRUE),
    breeding_m = sum(.data$bred_m[.data$sex == "M"], na.rm = TRUE),
    repro_f = sum(.data$repr_f[.data$sex == "F"], na.rm = TRUE),
    .groups = "drop")
  by_m <- dplyr::left_join(data.frame(mon = 1:12), by_m, by = "mon")
  for (cc in c("males", "females", "breeding_m", "repro_f")) by_m[[cc]][is.na(by_m[[cc]])] <- 0
  by_m$pm <- ifelse(by_m$males >= 5, round(100 * by_m$breeding_m / by_m$males), NA)
  by_m$pf <- ifelse(by_m$females >= 5, round(100 * by_m$repro_f / by_m$females), NA)
  by_m
}

hill_numbers <- function(d) {
  h <- dplyr::filter(d, !is.na(.data$tagID), !is.na(.data$scientificName))
  h <- species_level_only(h)                       # same species list as richness/Chao1
  h <- h[mam_tag_marked(h$tagID), , drop = FALSE]  # abundance = marked animals
  ab <- h %>% dplyr::distinct(.data$tagID, .data$scientificName) %>%
    dplyr::count(.data$scientificName, name = "n")
  n <- ab$n
  N <- sum(n)
  if (N == 0 || length(n) == 0)
    return(list(q0 = 0, q1 = 0, q2 = 0, even = NA_real_, n_ind = 0, n_sp = 0))
  p  <- n / N
  q0 <- length(n)
  q1 <- exp(-sum(p * log(p)))           # exp Shannon
  q2 <- 1 / sum(p^2)                    # inverse Simpson
  list(q0 = q0, q1 = round(q1, 1), q2 = round(q2, 1),
       even = round(q1 / q0, 2),        # Shannon evenness ratio (Pielou-like, 0–1)
       n_ind = N, n_sp = q0)
}

# ---------------------------------------------------------------------------
# Detection-corrected abundance: closed-capture estimation per trapping bout.
#
# NEON traps each grid in multi-night "bouts" (pathogen grids ~3 consecutive
# nights; diversity grids 1 night). On a closed multi-night bout we can correct
# the raw count for animals we never caught, using recaptures WITHIN the bout.
#
# Tier-1, dependency-free (base R + dplyr). Estimators on REAL ear tags only:
#   k >= 3 nights : Schnabel (1938)   N = Σ(Cₜ·Mₜ) / Σ Rₜ
#   k == 2 nights : Chapman (1951)    N = (M+1)(C+1)/(R+1) − 1
#   k == 1 night  : no estimate (index only; that is what MNKA/CPUE are for)
# Detection p̂ under Otis et al. (1978) Model M0: p̂ = ΣCₜ / (k·N_marked), on
# real-tag captures and the real-tag estimate only.
#
# Marks. A mark is a real ear tag (mam_tag_marked). NEON SOP C.8 issues a
# placeholder tagID NEON.MAM.Dxx.O<plot><trapCoordinate><YYYYMMDD> once per
# UNTAGGED capture; it names a capture event, not an animal, and can never be
# recaptured, so it never enters Cₜ, Uₜ, Rₜ or Mₜ. Each placeholder capture
# event is instead added to the bout as a known-alive animal (n_unmarked) on
# top of the real-tag estimate: N = N_marked + n_unmarked, and the same constant
# shifts MNKA and both interval limits. A placeholder row the crew marked
# recapture = "Y" is a repeat sighting of an animal already counted, so it adds
# nothing (n_unmarked_recap records how many were dropped).
# Deaths. A real-tag animal with fate "dead" on night t is in Cₜ (and in Rₜ if
# it was marked earlier in the bout), then leaves the marked pool:
#   Mₜ = Σ_{s<t} Uₛ − Σ_{s<t} Dₛ   (floored at 0).
#
# Guards: report only when ΣR (real tags) ≥ RECAP_GATE (3); clamp N_marked ≥
# MNKA of real tags (minimum known alive is a hard floor).
# Bout 95% limits (both skewed, never ±SE on N):
#   Schnabel, ΣR < 50 : Krebs (1999) Poisson limits on the recapture count,
#                       N_lo = Σ(CM)/R_hi, N_hi = Σ(CM)/R_lo, finite for ΣR ≥ 3.
#   Schnabel, ΣR ≥ 50 : normal limits in the 1/N domain, then inverted.
#   Chapman           : the same Poisson limits on R plugged into Chapman.
# varN is stored for the monthly roll-up only (Schnabel: delta-method
# var(1/N)·N⁴ even when the Poisson limits are used; Chapman: Seber's variance).
# Spec: Fauna review (Schnabel 1938; Chapman 1951; Otis et al. 1978; Krebs 1999,
# 2017); NEON.DOC.000481 SOP C.8 for the placeholder convention.
#
# Bout grouping: k counts trapped nights (trap_effort > 0), so an empty night
# inside a bout still counts toward detection; bouts are assigned over trapped
# nights, captures join by plot and night. A trapped plot-night is a distinct
# (plotID, collectDate) with any row of trap_effort > 0 (fallback: is_set, then
# the tagged capture rows when neither column exists). Within a plot a new bout
# starts when the gap to the previous trapped night is > 2 days (NEON allows a
# 1-night slip), keyed off consecutive trapped dates (NOT a hardcoded
# "3 nights", since NEON reduced sampling at some sites). A bout that trapped
# but caught no tagged animal stays in the table (sumC = 0, NA estimates).
# Capture rows are distinct (plotID, collectDate, tagID) over ALL tagged rows,
# placeholders included; a capture on a night with no effort breaks the effort
# contract and stops. Recapture status for real tags is recomputed WITHIN
# bout (the raw `recapture` column carries cross-bout history).
#
# The monthly roll-up (closed_capture_series) is a MEAN PER ESTIMABLE GRID; its
# interval is the bout's own limits in a one-bout month, else a Chao (1989)
# log-normal interval on the animals beyond MNKA (see the rule above that function).
# ---------------------------------------------------------------------------
RECAP_GATE <- 3L   # minimum ΣRₜ (real tags) to report a point estimate
RECAP_GOOD <- 7L   # ΣRₜ at/above which precision is "good" (else low-precision)
RECAP_POISSON_MAX <- 50L  # below this ΣR, Schnabel bout limits are Poisson (Krebs 1999)

# Krebs (1999) Poisson 95% limits on an observed recapture count R.
recap_poisson_limits <- function(R) {
  c(lo = stats::qchisq(0.025, 2 * R) / 2,
    hi = stats::qchisq(0.975, 2 * R + 2) / 2)
}

bout_closed_capture <- function(d) {
  if (is.null(d) || nrow(d) == 0) return(NULL)
  keep <- mam_tag_present(d$tagID) & !is.na(d$plotID) & !is.na(d$date)
  # trapped plot-nights: any row with effort on that plot and night
  trapped <- if ("trap_effort" %in% names(d)) {
    e <- suppressWarnings(as.numeric(d$trap_effort)); is.finite(e) & e > 0
  } else if ("is_set" %in% names(d)) {
    d$is_set %in% TRUE
  } else keep
  trapped <- trapped & !is.na(d$plotID) & !is.na(d$date)
  if (!any(trapped)) {
    if (any(keep)) stop(sprintf(
      "bout_closed_capture: effort contract broken: capture on %s %s has no trapped night",
      d$plotID[keep][1], format(d$date[keep][1])), call. = FALSE)
    return(NULL)
  }
  ym_all <- if ("ym" %in% names(d)) as.character(d$ym) else format(d$date, "%Y-%m")
  tn <- tibble::tibble(plotID = d$plotID[trapped], date = d$date[trapped],
                       ym = ym_all[trapped]) %>%
    dplyr::distinct(.data$plotID, .data$date, .keep_all = TRUE) %>%
    dplyr::arrange(.data$plotID, .data$date) %>%
    dplyr::group_by(.data$plotID) %>%
    dplyr::mutate(gap = as.integer(.data$date - dplyr::lag(.data$date)),
                  new_bout = is.na(.data$gap) | .data$gap > 2,
                  bout = cumsum(.data$new_bout)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(boutID = paste0(.data$plotID, "#", .data$bout)) %>%
    dplyr::group_by(.data$boutID) %>%
    dplyr::mutate(t = dplyr::row_number()) %>%
    dplyr::ungroup()

  # incomplete_grid per trapped night: any row of that plot-night whose
  # gridCompletion maps FALSE (absent field: never)
  if ("gridCompletion" %in% names(d)) {
    rows <- !is.na(d$plotID) & !is.na(d$date)
    gn <- tibble::tibble(plotID = d$plotID[rows], date = d$date[rows],
                         bad = mam_grid_complete(d$gridCompletion[rows]) %in% FALSE) %>%
      dplyr::group_by(.data$plotID, .data$date) %>%
      dplyr::summarise(bad = any(.data$bad), .groups = "drop")
    tn <- dplyr::left_join(tn, gn, by = c("plotID", "date"))
    tn$bad[is.na(tn$bad)] <- FALSE
  } else tn$bad <- FALSE

  src <- d[keep, , drop = FALSE]
  fate <- if ("fate" %in% names(src)) as.character(src$fate) else rep(NA_character_, nrow(src))
  recap <- if ("recapture" %in% names(src)) as.character(src$recapture) else rep(NA_character_, nrow(src))
  src <- tibble::tibble(plotID = src$plotID, date = src$date, tagID = as.character(src$tagID),
                        dead = !is.na(fate) & fate == "dead",
                        recapY = !is.na(recap) & recap == "Y")
  # one row per (plot, night, tag); duplicates that disagree resolve to any()
  cap <- src %>% dplyr::group_by(.data$plotID, .data$date, .data$tagID) %>%
    dplyr::summarise(dead = any(.data$dead), recapY = any(.data$recapY), .groups = "drop")
  # join captures onto the trapped-night bouts by (plot, night)
  cap <- dplyr::left_join(cap, tn[, c("plotID", "date", "boutID")], by = c("plotID", "date"))
  if (anyNA(cap$boutID)) {
    bad <- cap[is.na(cap$boutID), , drop = FALSE][1, ]
    stop(sprintf(
      "bout_closed_capture: effort contract broken: capture on %s %s has no trapped night",
      bad$plotID, format(bad$date)), call. = FALSE)
  }
  cap$ph <- mam_is_placeholder_tag(cap$tagID)
  real <- cap[!cap$ph, , drop = FALSE]

  # --- per-bout sufficient statistics on REAL tags, computed with a handful of
  #     grouped ops over the WHOLE table (not one dplyr call per bout; that was
  #     ~100x slower). Per trapped night t within a bout: C = caught, U = newly
  #     marked, R = C-U recaps, D = marked animals that died that night,
  #     M = marked and alive before t = (cumsum(U)-U) - (cumsum(D)-D). A trapped
  #     night with no capture has C = U = R = D = 0 and still counts in k and M.
  nights <- tn[, c("boutID", "date", "t")]
  Cn <- real %>% dplyr::count(.data$boutID, .data$date, name = "C")    # caught/night
  Un <- if (nrow(real) == 0) tibble::tibble(boutID = character(), date = as.Date(character()),
                                            U = integer()) else
    real %>% dplyr::group_by(.data$boutID, .data$tagID) %>%
    dplyr::summarise(first = min(.data$date), .groups = "drop") %>%
    dplyr::count(.data$boutID, date = .data$first, name = "U")         # new/night
  Dn <- real %>% dplyr::filter(.data$dead) %>%
    dplyr::count(.data$boutID, .data$date, name = "D")                 # died/night
  per <- nights %>%
    dplyr::left_join(Cn, by = c("boutID", "date")) %>%
    dplyr::left_join(Un, by = c("boutID", "date")) %>%
    dplyr::left_join(Dn, by = c("boutID", "date"))
  per$C[is.na(per$C)] <- 0L; per$U[is.na(per$U)] <- 0L; per$D[is.na(per$D)] <- 0L
  per <- per %>% dplyr::arrange(.data$boutID, .data$t) %>%
    dplyr::group_by(.data$boutID) %>%
    dplyr::mutate(M = pmax((cumsum(.data$U) - .data$U) - (cumsum(.data$D) - .data$D), 0L),
                  R = .data$C - .data$U) %>%
    dplyr::ungroup()
  agg <- per %>% dplyr::group_by(.data$boutID) %>%
    dplyr::summarise(k = max(.data$t), sumC = sum(.data$C), sumR = sum(.data$R),
                     num = sum(.data$C * .data$M),
                     M1 = .data$U[.data$t == 1][1] - .data$D[.data$t == 1][1],
                     C2 = .data$C[.data$t == 2][1],
                     R2 = .data$R[.data$t == 2][1], .groups = "drop")
  # bout ym and start come from its first trapped night (tn is date-ordered)
  bmeta <- tn %>% dplyr::group_by(.data$boutID) %>%
    dplyr::summarise(plotID = dplyr::first(.data$plotID), ym = dplyr::first(.data$ym),
                     start = min(.data$date), incomplete_grid = any(.data$bad),
                     .groups = "drop")
  cmeta <- cap %>% dplyr::group_by(.data$boutID) %>%
    dplyr::summarise(mnka_marked = dplyr::n_distinct(.data$tagID[!.data$ph]),
                     n_unmarked = sum(.data$ph & !.data$recapY),
                     n_unmarked_recap = sum(.data$ph & .data$recapY),
                     n_dead = dplyr::n_distinct(.data$tagID[!.data$ph & .data$dead]),
                     .groups = "drop")
  bmeta <- dplyr::left_join(bmeta, cmeta, by = "boutID")
  for (v in c("mnka_marked", "n_unmarked", "n_unmarked_recap", "n_dead"))
    bmeta[[v]] <- dplyr::coalesce(as.integer(bmeta[[v]]), 0L)   # zero-capture bouts
  est <- dplyr::left_join(bmeta, agg, by = "boutID")

  # --- estimator: pure arithmetic per bout (scalars; the loop is microseconds) -
  estimate_row <- function(k, sumC, sumR, num, M1, C2, R2, mnka_m, n_un) {
    out <- list(N_marked = NA_real_, N = NA_real_, lo = NA_real_, hi = NA_real_,
                p = NA_real_, sumR = as.integer(if (is.na(sumR)) 0L else sumR),
                varN = NA_real_, status = "single-night")
    if (is.na(k) || k < 2) return(out)
    if (is.na(sumR) || sumR < RECAP_GATE) { out$status <- "insufficient recaptures"; return(out) }
    pl <- recap_poisson_limits(sumR)
    if (k >= 3) {                                  # Schnabel
      N <- num / sumR
      var_invN <- sumR / (num^2)
      if (sumR < RECAP_POISSON_MAX) {              # Krebs (1999) Poisson limits on ΣR
        lo <- num / pl[["hi"]]
        hi <- num / pl[["lo"]]
      } else {                                     # normal limits in 1/N, inverted
        ci_inv <- 1 / N + c(1, -1) * 1.96 * sqrt(var_invN)   # lo 1/N -> hi N
        lo <- if (ci_inv[1] > 0) 1 / ci_inv[1] else NA_real_
        hi <- if (ci_inv[2] > 0) 1 / ci_inv[2] else Inf
      }
      # delta-method var(N) for the roll-up, from var(1/N), whichever limits
      varN <- var_invN * N^4
    } else {                                        # k == 2: Chapman
      M <- M1; C <- C2; R <- R2
      N <- ((M + 1) * (C + 1) / (R + 1)) - 1
      varN <- ((M + 1) * (C + 1) * (M - R) * (C - R)) / ((R + 1)^2 * (R + 2))
      # Poisson limits on R are used even where R/C > 0.10 (Krebs's binomial
      # zone); they are wider there, a conservative choice.
      lo <- (M + 1) * (C + 1) / (pl[["hi"]] + 1) - 1
      hi <- (M + 1) * (C + 1) / (pl[["lo"]] + 1) - 1
    }
    clamped <- FALSE
    if (is.finite(N) && N < mnka_m) { N <- mnka_m; clamped <- TRUE }   # MNKA is a hard floor
    lo <- max(lo, mnka_m, na.rm = TRUE)
    if (is.finite(hi) && hi < N) hi <- N           # keep lo <= N <= hi after the clamp
    p  <- sumC / (k * N)                           # real-tag captures over N_marked
    N_m <- round(N, 1)
    out$N_marked <- N_m
    out$N  <- N_m + n_un
    out$lo <- round(lo, 1) + n_un
    out$hi <- if (is.finite(hi)) round(hi, 1) + n_un else Inf
    out$p  <- round(min(max(p, 0), 1), 3); out$varN <- varN
    out$status <- if (clamped) "detection near-complete"
                  else if (sumR < RECAP_GOOD) "low-precision" else "ok"
    out
  }
  res <- lapply(seq_len(nrow(est)), function(i)
    estimate_row(est$k[i], est$sumC[i], est$sumR[i], est$num[i],
                 est$M1[i], est$C2[i], est$R2[i], est$mnka_marked[i], est$n_unmarked[i]))
  est$mnka     <- est$mnka_marked + est$n_unmarked
  est$N_marked <- vapply(res, function(z) z$N_marked, numeric(1))
  est$N        <- vapply(res, function(z) z$N, numeric(1))
  est$lo       <- vapply(res, function(z) z$lo, numeric(1))
  est$hi       <- vapply(res, function(z) z$hi, numeric(1))
  est$p        <- vapply(res, function(z) z$p, numeric(1))
  est$sumR     <- vapply(res, function(z) z$sumR, integer(1))
  est$varN     <- vapply(res, function(z) z$varN, numeric(1))
  est$status   <- vapply(res, function(z) z$status, character(1))
  # a bout with any night whose grid was not completely set or processed is
  # held out of estimation (like "insufficient recaptures"): no N, interval or p
  ig <- est$incomplete_grid %in% TRUE
  if (any(ig)) {
    for (v in c("N", "lo", "hi", "p", "varN", "N_marked")) est[[v]][ig] <- NA_real_
    est$status[ig] <- "incomplete grid"
  }
  est <- est[, c("boutID", "plotID", "ym", "start", "k", "sumC", "sumR", "num",
                 "mnka", "N", "lo", "hi", "p", "varN", "status",
                 "N_marked", "mnka_marked", "n_unmarked", "n_unmarked_recap", "n_dead",
                 "incomplete_grid")]
  est[order(est$start), ]
}

# Roll per-bout estimates up to a per-month site series as a MEAN PER ESTIMABLE
# GRID, so the line does not track how many grids happened to pass the gate.
# Two bouts on the same plot in one month are averaged first (plot-month), then
# plot-months are averaged within the month; varN follows the same means.
# Monthly p̂ is pooled on real tags, ΣC / Σ(k·N_marked). The 95% interval
# follows one three-case rule: a month with one estimable bout keeps that bout's
# own limits; a month with several bouts uses a Chao (1989) log-normal interval
# on the animals beyond MNKA (f0 = N − MNKA) when f0 > 0 and varN > 0, and
# otherwise lo = MNKA, hi = max(N, mean of the bouts' finite hi). lo ≥ MNKA
# always, and an unbounded bout upper limit keeps the month's hi at Inf.
# `design_break` marks a month whose modal nights per bout (`k_mode`, the most
# frequent k among its estimable bouts, ties to the larger k) differs from the
# previous estimable month (e.g. NEON moving grids from 3 nights to 1): the
# line breaks where nights per bout change. A change in how many grids, or which
# grids, were estimable is not a break, since the per-grid mean keeps those
# months comparable (`n_grids` and `grids` record them).
# Only months with at least one estimable bout get an abundance; others stay
# index-only.
closed_capture_series <- function(d, bouts = NULL) {
  if (is.null(bouts)) bouts <- bout_closed_capture(d)
  if (is.null(bouts) || nrow(bouts) == 0) return(NULL)
  est <- dplyr::filter(bouts, .data$status %in% c("ok", "low-precision", "detection near-complete"),
                       is.finite(.data$N))
  # single-night (k=1) bouts can carry NO within-bout recapture, so Schnabel/
  # Chapman can never run on them; they are MNKA/CPUE-only BY DESIGN, not missing
  # data. Surface the count so a sparse N̂ series reads as a sampling-design fact.
  n_single <- sum(bouts$status == "single-night", na.rm = TRUE)
  n_unmarked_total <- sum(bouts$n_unmarked, na.rm = TRUE)
  n_dead_total <- sum(bouts$n_dead, na.rm = TRUE)
  # bouts held out because a night's grid was incomplete (gridCompletion)
  n_incomplete <- sum(bouts$status == "incomplete grid", na.rm = TRUE)
  # estimable bouts whose raw estimate fell below MNKA and was clamped to it
  n_clamped <- sum(est$status == "detection near-complete", na.rm = TRUE)
  if (nrow(est) == 0)
    return(list(series = NULL, n_bouts = nrow(bouts), n_single = n_single,
                n_estimable = 0, mean_p = NA_real_, mean_detect = NA_real_,
                n_unmarked_total = n_unmarked_total, n_dead_total = n_dead_total,
                n_clamped = 0L, n_incomplete = n_incomplete))
  pm <- est %>% dplyr::group_by(.data$ym, .data$plotID) %>%
    dplyr::summarise(
      n_bouts = dplyr::n(),
      # var of a mean of bouts assumed independent; same-plot repeat bouts in a
      # month share animals, so this is marginally understated
      varN = sum(.data$varN, na.rm = TRUE) / dplyr::n()^2,
      sumC = sum(.data$sumC),
      kN = sum(.data$k * .data$N_marked),
      k_mean = mean(.data$k),
      hi_inf = any(!is.finite(.data$hi)),
      unmarked = mean(.data$n_unmarked),
      N_marked = mean(.data$N_marked),
      mnka = mean(.data$mnka),
      N = mean(.data$N), .groups = "drop")
  series <- pm %>% dplyr::group_by(.data$ym) %>%
    dplyr::summarise(
      n_grids = dplyr::n_distinct(.data$plotID),
      grids = paste(sort(unique(.data$plotID)), collapse = ","),
      n_bouts = sum(.data$n_bouts),
      # var of a mean of plot-months assumed independent across grids (and, via
      # the step above, across same-plot repeat bouts, which share animals, so
      # varN is marginally understated)
      varN = sum(.data$varN) / dplyr::n_distinct(.data$plotID)^2,
      k_mean = mean(.data$k_mean),
      p = round(sum(.data$sumC) / sum(.data$kN), 3),
      hi_inf = any(.data$hi_inf),
      unmarked = mean(.data$unmarked),
      N_marked = mean(.data$N_marked),
      mnka = mean(.data$mnka),
      N = mean(.data$N), .groups = "drop") %>%
    dplyr::mutate(date = as.Date(paste0(.data$ym, "-01"))) %>%
    dplyr::arrange(.data$date)
  # per-month bout limits: the single bout's own lo/hi, and the mean finite hi
  bl <- est %>% dplyr::group_by(.data$ym) %>%
    dplyr::summarise(bout_lo = dplyr::first(.data$lo), bout_hi = dplyr::first(.data$hi),
                     k_mode = { tk <- table(.data$k); kv <- as.integer(names(tk)); max(kv[tk == max(tk)]) },
                     hi_fin_mean = if (any(is.finite(.data$hi)))
                       mean(.data$hi[is.finite(.data$hi)]) else NA_real_,
                     .groups = "drop")
  series <- dplyr::left_join(series, bl, by = "ym")
  single <- series$n_bouts == 1L                                    # case (a)
  f0 <- series$N - series$mnka
  chao <- !single & is.finite(f0) & f0 > 0 &                         # case (b)
    is.finite(series$varN) & series$varN > 0
  K <- rep(NA_real_, nrow(series))
  K[chao] <- exp(1.96 * sqrt(log(1 + series$varN[chao] / f0[chao]^2)))
  series$se <- sqrt(pmax(series$varN, 0))
  # case (c): several bouts, no usable f0 / varN
  series$lo <- ifelse(chao, series$mnka + f0 / K, series$mnka)
  series$hi <- ifelse(chao, series$mnka + f0 * K,
                      pmax(series$N, series$hi_fin_mean, na.rm = TRUE))
  series$hi[!single & series$hi_inf] <- Inf
  series$lo[single] <- series$bout_lo[single]
  series$hi[single] <- series$bout_hi[single]
  series$lo <- pmax(series$lo, series$mnka)
  series$bout_lo <- series$bout_hi <- series$hi_fin_mean <- NULL
  series$design_break <- c(FALSE, series$k_mode[-1] != series$k_mode[-nrow(series)])
  list(series = series, n_bouts = nrow(bouts), n_single = n_single,
       n_estimable = nrow(est),
       mean_p = round(stats::weighted.mean(est$p, est$k * est$N_marked), 3),
       mean_detect = round(stats::weighted.mean(pmin(est$mnka / est$N, 1), est$N), 3),
       n_unmarked_total = n_unmarked_total, n_dead_total = n_dead_total,
       n_clamped = n_clamped, n_incomplete = n_incomplete)
}

# Reproductive activity per row (adults): scrotal males / pregnant-or-lactating
# females. Returns the input rows with a `repro` label added. Matching is
# exact-token (trimmed, lowercased, NA-safe) against the NEON.DOC.000481vQ
# vocabulary: testes scrotal/nonscrotal; pregnancyStatus pregnant/not pregnant;
# nipples enlarged/nonenlarged; vagina swollen (estrous), plugged, both,
# neither, not plugged. Substring matching is wrong here: "not pregnant",
# "nonenlarged" and "not plugged" all contain the positive token. The committed
# bundles store non-pregnant as "not". NA, "", "not" and "unknown" fall through
# to non-reproductive.
flag_repro <- function(d) {
  n <- nrow(d)
  lv <- function(x) tolower(trimws(as.character(x %||% rep(NA_character_, n))))
  scrotal <- lv(d$testes) %in% "scrotal"
  preg    <- lv(d$pregnancyStatus) %in% "pregnant"
  lact    <- lv(d$nipples) %in% "enlarged"
  vag_rec <- lv(d$vagina) %in% c("swollen", "plugged", "both")
  d$repro <- dplyr::case_when(
    scrotal             ~ "breeding male",
    preg                ~ "pregnant female",
    lact | vag_rec      ~ "lactating/receptive female",
    TRUE                ~ "non-reproductive"
  )
  d
}

fmt_int <- function(x) format(round(as.numeric(x)), big.mark = ",", trim = TRUE)

# ---------------------------------------------------------------------------
# Narrative insights — Girth-Index-style plain-English, data-driven sentences.
# Aimed at a general public audience + new NEON techs learning the site.
# Returns a character vector of short HTML sentences.
# ---------------------------------------------------------------------------
site_insights <- function(d, lb = NULL, cs = NULL) {
  if (is.null(lb)) lb <- build_leaderboard(d)
  if (is.null(cs)) cs <- community_stats(d, lb)
  sp <- species_summary(d)
  out <- character(0)

  # scope line first, so a shared/screenshotted insight list is self-describing
  if (!is.null(cs$date_min) && !is.na(cs$date_min))
    out <- c(out, sprintf(
      "This snapshot spans <b>%s – %s</b>: <b>%s</b> individuals of <b>%s</b> species across <b>%s</b> plots.",
      format(cs$date_min, "%b %Y"), format(cs$date_max, "%b %Y"),
      fmt_int(cs$individuals), cs$species, cs$plots))

  if (nrow(sp) > 0) {
    top <- sp[1, ]
    nn <- if (!is.na(top$nickname)) sprintf(" (the %s)", top$nickname) else ""
    out <- c(out, sprintf(
      "The most-trapped mammal here is the <b><i>%s</i></b>%s, with <b>%s</b> individuals across <b>%s</b> captures.",
      top$scientificName, nn, fmt_int(top$individuals), fmt_int(top$captures)))
    if (nrow(sp) >= 2) {
      # only species clearing the n>=8 adult floor are eligible (avg_weight is NA
      # below it) — never crown a "heaviest" off a 2-adult mean.
      elig <- sp[sp$n_adult >= 8L & is.finite(sp$avg_weight), ]
      if (nrow(elig) > 0) {
        hv <- elig[which.max(elig$avg_weight), ]
        if (hv$scientificName != top$scientificName)
          out <- c(out, sprintf(
            "The heaviest species caught is the <b><i>%s</i></b>, with adults averaging about <b>%s g</b> (n=%s); one of the larger-bodied species at this site.",
            hv$scientificName, hv$avg_weight, fmt_int(hv$n_adult)))
      }
    }
  }

  if (!is.null(lb) && nrow(lb) > 0) {
    L <- lb[1, ]
    out <- c(out, sprintf(
      "The hardest-working individual is <b>%s</b> (<i>%s</i>), caught <b>%s times</b>%s, earning a <b>%s</b> rank.",
      L$short, L$scientificName, L$captures,
      if (!is.na(L$career_days) && L$career_days > 60) sprintf(" over %s days", L$career_days) else "",
      L$rarity))
  }

  out <- c(out, sprintf(
    "<b>%s%%</b> of captures were re-encounters of already-tagged animals%s.",
    cs$recap_rate,
    if (cs$recap_rate >= 45) "; a high recapture rate means the same residents keep returning, so the population is well-marked"
    else "; most animals were seen only once"))

  out <- c(out, sprintf(
    "It took roughly <b>%s trap-nights</b> across <b>%s plots</b> to gather all of this.",
    fmt_int(cs$trap_nights), cs$plots))

  sa <- tryCatch(species_accum(d), error = function(e) NULL)
  if (!is.null(sa)) {
    est <- if (isTRUE(sa$unstable))
      sprintf("at least <b>%s</b> (a soft lower bound, too few twice-seen species to pin down)", sa$chao1)
    else sprintf("about <b>%s</b> (95%% CI %s–%s)", sa$chao1, sa$chao_lo, sa$chao_hi)
    out <- c(out, sprintf(
      "<b>%s species</b> were found; a Chao1 estimate suggests %s are really present, so sampling looks %s.",
      sa$sobs, est,
      if (sa$sobs >= 0.85 * sa$chao1) "close to complete" else "like it may still be missing a rare species or two"))
  }

  if (cs$n_male + cs$n_female > 0) {
    skew <- if (cs$n_male > cs$n_female * 1.2) "male-skewed"
            else if (cs$n_female > cs$n_male * 1.2) "female-skewed" else "fairly even"
    out <- c(out, sprintf("The catch was <b>%s</b> (%s males, %s females identified).",
                          skew, fmt_int(cs$n_male), fmt_int(cs$n_female)))
  }
  out
}

# ---------------------------------------------------------------------------
# Ranked breakdowns behind each clickable hero stat (Girth-style "click a stat,
# see the ranked story"). Returns list(title, subtitle, insight, icon, rows)
# where rows is a tibble(rank, name, metric, sub).
# ---------------------------------------------------------------------------
stat_breakdown <- function(d, lb, which) {
  # NEON values go into HTML rows and sentences: escape them (defense in depth;
  # today they are controlled vocabularies, but a bad upstream record must not
  # become markup).
  esc <- function(x) htmltools::htmlEscape(as.character(x))
  pack <- function(title, subtitle, insight, rows, icon = "\U0001F4CA")
    list(title = title, subtitle = subtitle, insight = insight, icon = icon, rows = rows)

  if (which == "species") {
    s <- species_summary(d)
    rows <- tibble::tibble(rank = seq_len(nrow(s)),
      name = paste0(s$emoji, "  <i>", esc(s$scientificName), "</i>",
                    ifelse(is.na(s$nickname), "", paste0(" <span class='dim'>· ", esc(s$nickname), "</span>"))),
      metric = paste0(fmt_int(s$captures), " caps"),
      sub = paste0(fmt_int(s$individuals), " ear-tagged individuals",
                   ifelse(s$untagged_captures > 0,
                          paste0(" · ", fmt_int(s$untagged_captures), " untagged captures"), "")))
    return(pack("Species, ranked by abundance", "Every species caught here, most common first",
      if (nrow(s)) sprintf("The <b><i>%s</i></b> dominates; the bottom of the list is where the rare finds hide.", esc(s$scientificName[1])) else "",
      rows, "\U0001F9EC"))
  }
  if (which == "individuals") {
    v <- utils::head(lb, 25)
    rows <- tibble::tibble(rank = v$rank, tag = v$tagID,
      name = paste0(v$emoji, "  <b>", esc(v$short), "</b> <span class='dim'>· ", esc(v$scientificName), "</span>"),
      metric = paste0(v$captures, " caps"),
      sub = paste0(ifelse(v$career_days > 0, paste0(v$career_days, "d career"), "single capture")))
    return(pack("Most-caught individuals", "Tap any animal to open its full dossier",
      "These are the regulars: the animals that kept turning up in traps.", rows, "\U0001F50D"))
  }
  if (which == "captures") {
    cby <- d %>% dplyr::filter(!is.na(.data$tagID), !is.na(.data$plotID)) %>%
      dplyr::group_by(.data$plotID) %>%
      dplyr::summarise(caps = dplyr::n(),
                       inds = dplyr::n_distinct(.data$tagID[mam_tag_marked(.data$tagID)]),  # real ear tags
                       .groups = "drop") %>%
      dplyr::arrange(dplyr::desc(.data$caps))
    rows <- tibble::tibble(rank = seq_len(nrow(cby)),
      name = paste0("\U0001F4CD ", cby$plotID), metric = paste0(fmt_int(cby$caps), " caps"),
      sub = paste0(fmt_int(cby$inds), " individuals"))
    return(pack("Captures by plot", "Where the action was across the site's trapping grids",
      if (nrow(cby)) sprintf("Plot <b>%s</b> was the busiest with %s captures.", esc(cby$plotID[1]), fmt_int(cby$caps[1])) else "",
      rows, "\U0001F3AF"))
  }
  if (which == "recapture") {
    rc <- d %>% dplyr::filter(!is.na(.data$tagID), !is.na(.data$scientificName)) %>%
      dplyr::group_by(.data$scientificName) %>%
      dplyr::summarise(caps = dplyr::n(),
        rate = round(100 * mean(.data$recapture %in% c("Y", "y")), 0), .groups = "drop") %>%
      dplyr::filter(.data$caps >= 5) %>% dplyr::arrange(dplyr::desc(.data$rate))
    rows <- tibble::tibble(rank = seq_len(nrow(rc)),
      name = paste0("<i>", esc(rc$scientificName), "</i>"), metric = paste0(rc$rate, "%"),
      sub = paste0(fmt_int(rc$caps), " captures"))
    return(pack("Recapture rate by species", "Which species stick around and get re-caught",
      "A high recapture rate means resident animals that survive and return to traps.", rows, "\U0001F501"))
  }
  if (which == "trapnights") {
    eff <- d %>% dplyr::filter(!is.na(.data$plotID)) %>% dplyr::group_by(.data$plotID) %>%
      dplyr::summarise(tn = sum(.data$trap_effort, na.rm = TRUE),
        caps = sum(.data$is_capture), .groups = "drop") %>%
      dplyr::mutate(cpue = ifelse(.data$tn > 0, round(100 * .data$caps / .data$tn, 1), NA)) %>%
      dplyr::arrange(dplyr::desc(.data$tn))
    rows <- tibble::tibble(rank = seq_len(nrow(eff)),
      name = paste0("\U0001F4CD ", eff$plotID), metric = paste0(fmt_int(eff$tn), " TN"),
      sub = paste0(eff$cpue, " caps / 100 TN"))
    return(pack("Trapping effort by plot", "A trap-night = one trap set for one night",
      "Catch-per-100-trap-nights (CPUE) lets you compare plots fairly, despite different effort.", rows, "\U0001F311"))
  }
  if (which == "legends") {
    v <- lb[lb$captures >= 10, ]
    if (nrow(v) == 0) v <- utils::head(lb, 10)
    rows <- tibble::tibble(rank = seq_len(nrow(v)), tag = v$tagID,
      name = paste0(v$emoji, "  <b>", esc(v$short), "</b> <span class='dim'>· ", esc(v$scientificName), "</span>"),
      metric = paste0(v$captures, " caps"),
      sub = paste0(rarity_tier(v$captures)))
    return(pack("The Legends · caught 10+ times", "Tap any legend to open its dossier",
      "These animals were exceptionally trap-happy, resident, or both.", rows, "\U0001F3C6"))
  }
  NULL
}

# ---------------------------------------------------------------------------
# Environmental overlays — "compare population with environment"
#
# A monthly per-site env table (precip / temp / soil moisture / phenology, see
# global.R ENV_LAYERS + scripts/refresh_env_data.R) gets drawn as a soft filled
# area BEHIND the abundance lines, on its own right-hand axis. These helpers are
# pure plotly/data utilities so the future beetle app can reuse them verbatim.
# ---------------------------------------------------------------------------

# Shift a monthly env table's dates forward by `lag` months. Ecological drivers
# often LEAD the response (a rain pulse feeds the seed crop that feeds the
# rodent boom months later); shifting the driver forward lines it up under the
# boom it putatively caused, which is exactly what the lag slider explores.
shift_env <- function(env, lag = 0) {
  if (is.null(env) || !nrow(env)) return(env)
  env$date <- as.Date(env$date)
  lag <- as.integer(lag %||% 0)
  if (lag != 0) {
    lt <- as.POSIXlt(env$date); lt$mon <- lt$mon + lag
    env$date <- as.Date(lt)
  }
  env
}

# Add an environmental overlay (filled area) to a plotly time-series, bound to a
# secondary axis (default "y3" so it can sit alongside an existing y2). `xlim`
# clips the area to the data's own date range so it never zooms the chart out.
# `conv` optionally transforms the displayed values (e.g. C->F) and `unit_label`
# overrides the hover unit to match (defaults keep the layer's native unit).
add_env_overlay <- function(p, env, layer, lag = 0, yaxis = "y3", xlim = NULL,
                            demo = FALSE, conv = NULL, unit_label = NULL) {
  meta <- ENV_LAYERS[[layer]]
  if (is.null(meta) || is.null(env) || !(meta$col %in% names(env))) return(p)
  e <- shift_env(env, lag)
  e$.v <- suppressWarnings(as.numeric(e[[meta$col]]))
  if (!is.null(conv)) e$.v <- conv(e$.v)
  e <- e[!is.na(e$.v), , drop = FALSE]
  if (!is.null(xlim)) e <- e[e$date >= xlim[1] & e$date <= xlim[2], , drop = FALSE]
  if (!nrow(e)) return(p)
  u  <- unit_label %||% meta$unit
  nm <- meta$label
  if (lag) nm <- sprintf("%s · lag %d mo", nm, as.integer(lag))
  if (demo) nm <- paste0(nm, " (demo)")
  dig <- meta$dig %||% 0                      # round the hover value (no 5-decimal %)
  plotly::add_trace(p, data = e, x = ~date, y = ~.v, yaxis = yaxis,
    type = "scatter", mode = "lines", fill = "tozeroy",
    name = nm, legendgroup = "env",
    line = list(color = meta$color, width = 1.6, shape = "spline"),
    fillcolor = paste0(meta$color, "1f"),
    hovertemplate = paste0(meta$label, "<br>%{x|%b %Y}: %{y:.", dig, "f} ", u, "<extra></extra>"))
}

# Layout spec for an env overlay's axis. `show` toggles the tick labels/title
# (off when the overlay is pure background context behind a busy chart).
env_axis_spec <- function(layer, side = "right", overlaying = "y", show = TRUE,
                          position = NULL, unit_label = NULL) {
  meta <- ENV_LAYERS[[layer]]
  if (is.null(meta)) return(list(overlaying = overlaying, side = side, visible = FALSE))
  spec <- list(
    title = if (show) sprintf("%s (%s)", meta$label, unit_label %||% meta$unit) else "",
    overlaying = overlaying, side = side, rangemode = "tozero",
    showgrid = FALSE, zeroline = FALSE, color = meta$color,
    showticklabels = show)
  if (!is.null(position)) spec$position <- position
  spec
}

# Collapse a monthly env table to a 12-point calendar-month climatology (mean
# of each metric across years) for the by-month phenology overlay. `lag` rotates
# the months so a leading driver lines up with the response month.
env_climatology <- function(env, layer, lag = 0) {
  meta <- ENV_LAYERS[[layer]]
  if (is.null(meta) || is.null(env) || !(meta$col %in% names(env))) return(NULL)
  e <- env; e$date <- as.Date(e$date)
  e$.v <- suppressWarnings(as.numeric(e[[meta$col]]))
  e <- e[!is.na(e$.v), , drop = FALSE]
  if (!nrow(e)) return(NULL)
  e$mon <- as.integer(format(e$date, "%m"))
  clim <- stats::aggregate(.v ~ mon, data = e, FUN = mean, na.rm = TRUE)
  clim <- clim[order(clim$mon), ]
  lag <- as.integer(lag %||% 0)
  if (lag != 0) clim$mon <- ((clim$mon - 1 + lag) %% 12) + 1
  clim <- clim[order(clim$mon), ]
  clim$value <- round(clim$.v, 1)
  clim[, c("mon", "value")]
}

# Scan lags 0..max_lag for the strongest correlation between this site's monthly
# catch-per-effort and a (lagged) environmental driver. Returns the best lag and
# Pearson r — the quantitative backbone of the "rain pulse leads the boom" story.
# Returns NULL when there's too little overlap to be meaningful.
# Monthly catch-per-effort series (one row per calendar month with effort).
# Factored out so env_corr_pvalue()'s circular-shift null shifts the EXACT
# same series env_corr_scan() correlates — the null and the observed statistic
# can never drift apart.
.mam_cpue_series <- function(d) {
  m <- d %>% dplyr::filter(!is.na(.data$ym)) %>%
    dplyr::group_by(.data$ym) %>%
    dplyr::summarise(cap = sum(.data$is_capture),
                     tn  = sum(.data$trap_effort, na.rm = TRUE), .groups = "drop")
  m <- m[m$tn > 0, , drop = FALSE]
  if (!nrow(m)) return(NULL)
  m$cpue <- 100 * m$cap / m$tn
  m$date <- as.Date(paste0(m$ym, "-01"))
  m[, c("date", "cpue")]
}

env_corr_scan <- function(d, env, layer, max_lag = 12) {
  meta <- ENV_LAYERS[[layer]]
  if (is.null(meta) || is.null(env) || !(meta$col %in% names(env))) return(NULL)
  m <- .mam_cpue_series(d)
  if (is.null(m) || nrow(m) < 8) return(NULL)   # honest overlap floor (was 4)
  ev <- env; ev$date <- as.Date(ev$date)
  ev$.v <- suppressWarnings(as.numeric(ev[[meta$col]]))
  ev <- ev[!is.na(ev$.v), c("date", ".v"), drop = FALSE]
  if (!nrow(ev)) return(NULL)
  # DESEASONALIZE both series (subtract each one's calendar-month climatology)
  # before correlating, so r reflects year-to-year ANOMALIES, not the shared
  # "both peak in summer" annual cycle — which otherwise inflates |r| and
  # manufactures driver-vs-driver multicollinearity. (Phenology review, 2026-06.)
  deseason <- function(val, date) {
    mon  <- as.integer(format(date, "%m"))
    clim <- tapply(val, mon, mean, na.rm = TRUE)
    val - as.numeric(clim[as.character(mon)])
  }
  m$cpue <- deseason(m$cpue, m$date)
  ev$.v  <- deseason(ev$.v,  ev$date)
  best <- list(lag = NA_integer_, r = NA_real_, n = 0L)
  for (lag in 0:max_lag) {
    e2 <- ev; lt <- as.POSIXlt(e2$date); lt$mon <- lt$mon + lag; e2$date <- as.Date(lt)
    j <- merge(m[, c("date", "cpue")], e2, by = "date")
    if (nrow(j) >= 8 && stats::sd(j$cpue, na.rm = TRUE) > 0 && stats::sd(j$.v, na.rm = TRUE) > 0) {
      r <- suppressWarnings(stats::cor(j$cpue, j$.v))
      if (!is.na(r) && (is.na(best$r) || abs(r) > abs(best$r)))
        best <- list(lag = lag, r = round(r, 2), n = nrow(j))
    }
  }
  if (is.na(best$r)) return(NULL)
  best$label <- meta$label; best$unit <- meta$unit
  best
}

# Month-matched pairs of (catch-per-effort, lagged driver value) for the
# environmental RESPONSE scatter — the same data the correlation note summarises,
# but as points so the shape of the relationship (linear? saturating?) is visible.
env_response_points <- function(d, env, layer, lag = 0) {
  meta <- ENV_LAYERS[[layer]]
  if (is.null(meta) || is.null(env) || !(meta$col %in% names(env))) return(NULL)
  m <- d %>% dplyr::filter(!is.na(.data$ym)) %>%
    dplyr::group_by(.data$ym) %>%
    dplyr::summarise(cap = sum(.data$is_capture),
                     tn  = sum(.data$trap_effort, na.rm = TRUE), .groups = "drop")
  m <- m[m$tn > 0, , drop = FALSE]
  if (!nrow(m)) return(NULL)
  m$cpue <- 100 * m$cap / m$tn
  m$date <- as.Date(paste0(m$ym, "-01"))
  e <- shift_env(env, lag)
  e$.v <- suppressWarnings(as.numeric(e[[meta$col]]))
  e <- e[!is.na(e$.v), c("date", ".v"), drop = FALSE]
  j <- merge(m[, c("date", "cpue")], e, by = "date")
  # Draw floor = the SCORING floor (n>=8, matching env_corr_scan + the fit-line
  # gate in output$envScatter). Below 8 month-matched pairs the scatter would
  # render a cloud too thin to fit or score — an implied relationship the data
  # can't support — so return nothing and let the panel show its honest
  # "Not enough month-matched data" note instead.
  if (nrow(j) < 8) return(NULL)
  j$year <- as.integer(format(j$date, "%Y"))
  names(j)[names(j) == ".v"] <- "value"
  tibble::as_tibble(j[order(j$date), c("date", "year", "value", "cpue")])
}

# Best-lag correlation for EVERY available driver, ranked by |r|. Answers the
# question "which environmental signal does this population track best?" — the
# data behind the multi-driver comparison panel.
env_corr_all <- function(d, env, max_lag = 12) {
  if (is.null(d) || is.null(env)) return(NULL)
  rows <- lapply(names(ENV_LAYERS), function(k) {
    meta <- ENV_LAYERS[[k]]
    if (!(meta$col %in% names(env)) || !any(!is.na(env[[meta$col]]))) return(NULL)
    sc <- env_corr_scan(d, env, k, max_lag)
    if (is.null(sc)) return(NULL)
    data.frame(layer = k, label = meta$label, color = meta$color,
               lag = sc$lag, r = sc$r, n = sc$n, stringsAsFactors = FALSE)
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (!length(rows)) return(NULL)
  res <- do.call(rbind, rows)
  tibble::as_tibble(res[order(-abs(res$r)), ])
}

# ---------------------------------------------------------------------------
# env_corr_pvalue(): the honesty p-value for env_corr_all()'s dredge.
#
# env_corr_all() reports the BEST |r| over ~5 drivers x 13 lags = up to 65
# candidate correlations. Best-of-65 |r| is upward-biased: some lag x driver combo
# will look strong by chance alone. This computes a CIRCULAR-SHIFT null (the same
# family as seasonal_env.R's .seas_adj_p): rotate the deseasonalized monthly CPUE
# anomaly series by a random offset — which PRESERVES its serial structure and
# seasonal-anomaly distribution while breaking any true alignment to the drivers —
# re-run the FULL scan, take the best |r|, and repeat. p = fraction of null runs
# whose best-|r| meets or beats the observed best-|r|. A ~1-yr granivore seed-crop
# lag is a REAL mechanism, so no lag is penalized on length; only the search-space
# (many combos) and annual autocorrelation are penalized. seed 11 = stable.
#
# Returns list(p, n_search = combos actually evaluated, n_match = observed best n,
# n_months = length of the shifted series, r_obs = the unrounded observed best r,
# layer, lag) or NULL. Needs a ~24-month coverage floor for the shift to have
# room to break alignment (below it the rotation barely moves and the null is
# meaningless -> p = NA, "too short to test").
#
# The observed statistic is the UNROUNDED best |r| from the same scan the null
# runs (env_corr_scan() rounds r to 2 dp for display; comparing a rounded
# observed value with unrounded null values biased p). Each driver x lag pairing
# is resolved once to fixed (response index, driver value) pairs; the null only
# rotates the response values over the same months, so each null scan is just
# the correlations, with results identical to re-merging by date.
# `series` (data.frame(date, cpue)) replaces the site's monthly CPUE series; it
# is for calibration runs on synthetic responses.
env_corr_pvalue <- function(d, env, max_lag = 12, nperm = 500, min_months = 24,
                            series = NULL, seed = 11L) {
  if (is.null(env)) return(NULL)
  m <- if (!is.null(series)) as.data.frame(series)[, c("date", "cpue")] else
    if (is.null(d)) NULL else .mam_cpue_series(d)
  if (is.null(m) || nrow(m) < 8) return(NULL)
  m$date <- as.Date(m$date)
  # deseasonalize the CPUE series once (same rule as env_corr_scan); the driver
  # side is deseasonalized per layer, so shifting the response alone is a valid
  # null for the aligned (deseasonalized) correlation.
  mon  <- as.integer(format(m$date, "%m"))
  clim <- tapply(m$cpue, mon, mean, na.rm = TRUE)
  y <- m$cpue - as.numeric(clim[as.character(mon)])
  ny <- length(y)
  # resolve every driver x lag pairing to (index into y, driver anomaly)
  grid <- list(); glayer <- character(0); glag <- integer(0)
  for (lay in names(ENV_LAYERS)) {
    meta <- ENV_LAYERS[[lay]]
    if (!(meta$col %in% names(env))) next
    ev <- env; ev$date <- as.Date(ev$date)
    ev$.v <- suppressWarnings(as.numeric(ev[[meta$col]]))
    ev <- ev[!is.na(ev$.v), c("date", ".v"), drop = FALSE]
    if (!nrow(ev)) next
    emo <- as.integer(format(ev$date, "%m"))
    ecl <- tapply(ev$.v, emo, mean, na.rm = TRUE)
    ev$.v <- ev$.v - as.numeric(ecl[as.character(emo)])
    for (lag in 0:max_lag) {
      lt <- as.POSIXlt(ev$date); lt$mon <- lt$mon + lag; ed <- as.Date(lt)
      im <- which(m$date %in% ed)
      x <- ev$.v[match(m$date[im], ed)]
      grid[[length(grid) + 1L]] <- list(im = im, x = x,
        ok = length(im) >= 8 && stats::sd(x) > 0)
      glayer <- c(glayer, lay); glag <- c(glag, lag)
    }
  }
  if (!length(grid)) return(NULL)
  scan_r <- function(yv) vapply(grid, function(g) {
    if (!g$ok) return(NA_real_)
    yy <- yv[g$im]
    if (!(stats::sd(yy) > 0)) return(NA_real_)
    r <- suppressWarnings(stats::cor(yy, g$x))
    if (is.finite(r)) r else NA_real_
  }, numeric(1))
  r0 <- scan_r(y)
  # a driver is in the search only if its own scan found a usable lag (the
  # same layers env_corr_all() returns)
  live <- tapply(is.finite(r0), glayer, any)
  layers <- names(live)[live]
  if (!length(layers)) return(NULL)
  use <- glayer %in% layers
  grid <- grid[use]; r0 <- r0[use]; glayer <- glayer[use]; glag <- glag[use]
  b <- which.max(abs(r0))
  obs <- abs(r0[b])
  out <- list(p = NA_real_, n_search = length(grid), n_match = length(grid[[b]]$im),
              n_months = ny, r_obs = r0[b], layer = glayer[b], lag = glag[b])
  if (ny < min_months) return(out)
  perm_best <- .with_seed(seed, replicate(nperm, {
    k <- sample.int(ny - 1L, 1L)
    rr <- scan_r(y[((seq_len(ny) - 1L + k) %% ny) + 1L])
    if (all(is.na(rr))) NA_real_ else max(abs(rr), na.rm = TRUE)
  }))
  out$p <- round(mean(perm_best >= obs - 1e-9, na.rm = TRUE), 3)
  out
}

# ---------------------------------------------------------------------------
# ec_corr_color() — the SINGLE source of truth for the hue of a (driver,
# correlation) pair on the population-driver visuals. Three meanings, three
# channels, never overloaded onto one:
#   identity  -> WHICH driver (the hue family: temp warm, rain blue, leaf green)
#   direction -> the SIGN of r (which pole) — but only USED on surfaces that
#                ALSO encode sign geometrically (a bar's side of 0, a slope), so
#                a colour-blind reader never reads direction from hue alone
#   magnitude -> length / the r number; colour only modulates LOUDNESS here:
#                weak links fade toward the surface, |r| < 0.2 -> neutral grey
# The only CVD-bulletproof diverging axis is cool-blue <-> warm, so fruit- and
# flower-negative escape to blue/slate rather than a green/brown pair that would
# collapse under red-green colour-blindness. (Vera + Quinn + Alyssa, 2026-06.)
EC_CORR_POLES <- list(
  precip  = list(pos = c("#1f6fb2", "#5aa9e6"), neg = c("#b07a35", "#d8a85a")),  # wet blue <-> dry tan
  temp    = list(pos = c("#d9480f", "#ff7a45"), neg = c("#2f7fb5", "#6cc4ec")),  # hot red <-> cold blue
  flower  = list(pos = c("#c2255c", "#f06595"), neg = c("#7a8a99", "#9aa7b5")),  # bloom magenta <-> slate
  greenup = list(pos = c("#2b8a3e", "#69db7c"), neg = c("#9c6644", "#c08457")),  # leaf green <-> dead brown
  fruit   = list(pos = c("#9c6644", "#c08457"), neg = c("#2f7fb5", "#6cc4ec"))   # ripe brown <-> cool blue
)

# mix hex `a` toward hex `b` by fraction `w` (0 = all a, 1 = all b)
blend_hex <- function(a, b, w) {
  ca <- grDevices::col2rgb(a); cb <- grDevices::col2rgb(b)
  m  <- round(ca * (1 - w) + cb * w)
  grDevices::rgb(m[1], m[2], m[3], maxColorValue = 255)
}

ec_corr_color <- function(layer, r, dark = FALSE) {
  if (length(r) != 1 || is.na(r)) return("#8a97a8")
  s <- abs(r)
  if (s < 0.2) return("#8a97a8")                       # negligible -> neutral grey
  pole <- EC_CORR_POLES[[layer]]
  base <- if (is.null(pole)) (ENV_LAYERS[[layer]]$color %||% "#8a97a8")
          else (if (r >= 0) pole$pos else pole$neg)[[if (dark) 2L else 1L]]
  surf <- if (dark) "#16213a" else "#ffffff"           # blend target = the plot surface
  w    <- if (s >= 0.6) 0 else if (s >= 0.35) 0.15 else 0.40  # fade loudness for weaker links
  blend_hex(base, surf, w)
}

# Long trap-grid table (one row per A-J x 1-10 cell) for an individual's heatmap.
trap_grid_long <- function(d, tag) {
  sub <- dplyr::filter(d, .data$tagID == tag, !is.na(.data$tx), !is.na(.data$ty))
  counts <- sub %>%
    dplyr::count(.data$tx, .data$ty, name = "captures")
  grid <- expand.grid(tx = 1:10, ty = 1:10)
  grid <- dplyr::left_join(grid, counts, by = c("tx", "ty"))
  grid$captures[is.na(grid$captures)] <- 0
  grid$letter <- LETTERS[grid$tx]
  grid$plot   <- mode_chr(sub$plotID)
  grid
}
