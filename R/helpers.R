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

# Most-frequent non-NA value (ties -> first). Used to pick an individual's
# "home" plot, modal species id, sex, etc.
mode_chr <- function(x) {
  x <- x[!is.na(x) & x != ""]
  if (length(x) == 0) return(NA_character_)
  names(sort(table(x), decreasing = TRUE))[1]
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

# Stable, app-wide species -> color map so the SAME species is the SAME color
# on the map, the trend chart, the morphospace scatter, etc.
make_species_pal <- function(d) {
  sp <- sort(unique(d$scientificName[!is.na(d$scientificName)]))
  if (length(sp) == 0) return(character(0))
  cols <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))(length(sp))
  stats::setNames(cols, sp)
}

# A one-line, friendly fact about a genus — for the "meet the mammals" cards
# (public learners + new NEON techs getting to know their site).
species_blurb <- function(scientificName) {
  g <- sub(" .*$", "", scientificName %||% "")
  lut <- c(
    Dipodomys   = "Kangaroo rats hop on huge hind feet, carry seeds in cheek pouches, and can live without ever drinking water.",
    Chaetodipus = "Spiny pocket mice stuff seeds into fur-lined cheek pouches and sleep away cold spells in torpor.",
    Perognathus = "Silky pocket mice are tiny seed-hoarders that go dormant underground through the worst of winter.",
    Peromyscus  = "Deer mice are wide-eyed climbers found almost everywhere in North America — the classic field-trap regular.",
    Reithrodontomys = "Harvest mice are thumb-sized seed-eaters that weave little grass nests above the ground.",
    Onychomys   = "Grasshopper mice are carnivores that hunt scorpions and insects — and howl like tiny wolves.",
    Neotoma     = "Woodrats (pack rats) build huge stick middens and famously swap any shiny object for what they carry.",
    Microtus    = "Voles are stout grass-eaters whose boom-and-bust population cycles drive whole food webs.",
    Sigmodon    = "Cotton rats breed fast and graze grasses; their numbers can explode after a wet season.",
    Sylvilagus  = "Cottontail rabbits freeze, then bolt in a zig-zag — a favorite meal for nearly every desert predator.",
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

# Tier colors for the Girth light theme (solid fills, white text on top).
rarity_meta <- function(tier) {
  lut <- list(
    Legendary = list(color = "#c9a300", glow = "#c9a300", icon = "⭐"),  # deep gold
    Epic      = list(color = "#AB0520", glow = "#AB0520", icon = "\U0001F48E"),  # cardinal
    Rare      = list(color = "#16386e", glow = "#16386e", icon = "\U0001F535"),  # navy
    Uncommon  = list(color = "#1a7f37", glow = "#1a7f37", icon = "\U0001F7E2"),  # green
    Common    = list(color = "#6b7280", glow = "#6b7280", icon = "▫")
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
            "nightuid","trapStatus","remarks","nativeStatusCode")
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

  d$is_capture <- !is.na(d$tagID)
  d$is_set     <- !grepl("^1", d$trapStatus %||% "")  # trapStatus "1 - trap not set"
  # Effort weight (trap-nights) for catch-per-effort. A trap-night is one trap
  # AVAILABLE to catch for one night, computed from each site's own data — never
  # a fixed grid size. A sprung/disturbed trap (codes 2,3) was only available
  # part of the night, so it counts as HALF a trap-night (Nelson & Clark 1973);
  # codes 4,5,6 (captured / set-and-empty) = a full trap-night; code 1 (not set)
  # = 0. (Fauna review.)
  ts1 <- substr(as.character(d$trapStatus %||% ""), 1, 1); ts1[is.na(ts1)] <- ""
  d$trap_effort <- ifelse(ts1 == "1", 0, ifelse(ts1 %in% c("2", "3"), 0.5, 1))
  d
}

# ---------------------------------------------------------------------------
# Body-condition / "Chonk" Index
# Implements the Scaled Mass Index (Peig & Green 2009, Oikos): standardises an
# individual's mass to a common body length using a species-specific SMA slope,
# then expresses condition as a percentile within its species.
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
    is.na(pct)  ~ "—",
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
# ---------------------------------------------------------------------------
build_leaderboard <- function(d) {
  base <- dplyr::filter(d, !is.na(.data$tagID))
  if (nrow(base) == 0) return(NULL)

  ind <- base %>%
    dplyr::group_by(.data$tagID) %>%
    dplyr::summarise(
      scientificName = mode_chr(.data$scientificName),
      n_species_ids  = dplyr::n_distinct(.data$scientificName[!is.na(.data$scientificName)]),
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
      # life stage at the EARLIEST capture (not the modal stage) — the age clock
      # only has a birth anchor if we first met the animal young. NA-safe: drops
      # undated rows, NA when none are dated.
      first_stage    = { o <- order(.data$date, na.last = NA)
                         if (length(o)) .data$lifeStage[o[1]] else NA_character_ },
      roam_m         = roam_radius(.data$tx, .data$ty),
      mdm_m          = max_dist_moved(.data$tx, .data$ty),
      .groups = "drop"
    )

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
  handled <- dplyr::filter(d, !is.na(.data$tagID))
  if (is.null(lb)) lb <- build_leaderboard(d)

  list(
    total_captures = nrow(handled),
    individuals    = dplyr::n_distinct(handled$tagID),
    # species-level IDs only (excludes genus-only "X sp." / ambiguous "A/B"),
    # so this matches the richness used by Hill numbers, Chao1, and the map
    species        = dplyr::n_distinct(species_level_only(
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
      individuals = dplyr::n_distinct(.data$tagID),
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
    n_ind  = dplyr::n_distinct(.data$tagID),
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

# The actual captures whose body measurement was flagged as a possible error
# (beyond median +/- 5*MAD among this species' adults — same rule as
# species_measurements()), so the QC modal can list the exact records to verify.
# Returns a data frame (short, date, plot, value, sex) or NULL.
flagged_measure_captures <- function(d, sp, measure) {
  col <- switch(measure, weight = "weight", hindfoot = "hindfootLength",
                tail = "tailLength", ear = "earLength", NULL)
  if (is.null(col) || is.null(d)) return(NULL)
  # species_level_only() so this draws from the exact same adult pool as
  # species_measurements()'s nflag() — the flag rule must agree or the count
  # here won't match the ⚠ tooltip's count.
  h <- species_level_only(dplyr::filter(d, !is.na(.data$tagID), .data$scientificName == sp,
                          .data$lifeStage %in% "adult"))
  if (is.null(h) || !nrow(h)) return(NULL)
  v <- h[[col]]; keep <- is.finite(v) & v > 0
  h <- h[keep, , drop = FALSE]; v <- v[keep]
  if (length(v) < 3) return(NULL)
  m <- stats::median(v); s <- max(stats::mad(v), 0.1 * m)
  if (!is.finite(s) || s <= 0) return(NULL)
  flag <- abs(v - m) > 5 * s
  if (!any(flag)) return(NULL)
  data.frame(short = short_tag(h$tagID[flag]),
             date = h$date[flag], plotID = h$plotID[flag],
             value = round(v[flag], 1), sex = h$sex[flag],
             median = m, stringsAsFactors = FALSE)
}

# ---------------------------------------------------------------------------
# individual_history(): every capture event for ONE tagged animal, ordered by
# date, with all the body measurements ("meso" morphometrics) + field context a
# QC reviewer needs. Unlike inspect_captures() (which requires coords, for the
# map-consistent mover modal) this keeps EVERY dated capture so the card never
# silently drops a record. Missing columns degrade to NA so a live-fetched table
# with a different shape can't error the card.
# ---------------------------------------------------------------------------
individual_history <- function(d, tag) {
  if (is.null(d) || is.null(tag) || length(tag) != 1 || is.na(tag) || tag == "") return(NULL)
  h <- dplyr::filter(d, .data$tagID == tag, !is.na(.data$date))
  if (!nrow(h)) return(NULL)
  need <- c("plotID", "trapCoordinate", "lifeStage", "sex", "scientificName",
            "weight", "hindfootLength", "tailLength", "earLength", "totalLength",
            "recapture", "fate")
  for (cc in need) if (!cc %in% names(h)) h[[cc]] <- NA
  h[order(h$date), c("date", need), drop = FALSE]
}

# ---------------------------------------------------------------------------
# individual_qc_flags(): the ranked QC signals for one animal's capture history,
# returned as a list of list(level, text). Ranking follows the Fauna review —
# the most reliable error signals first (same-tag-two-plots, stage regression,
# beyond-lifespan span = "high"), the suggestive ones after (sex flip, weight
# jump = "warn"; hindfoot jitter, species change = "info"). Every flag is phrased
# as "verify", not "wrong": legitimate causes exist for the lower-ranked ones.
# `hist` = individual_history(d, tag); `lb_row` = the build_leaderboard() row.
# ---------------------------------------------------------------------------
individual_qc_flags <- function(hist, lb_row) {
  flags <- list()
  add <- function(level, text) flags[[length(flags) + 1L]] <<- list(level = level, text = text)
  if (is.null(hist) || !nrow(hist)) return(flags)
  has_row <- !is.null(lb_row) && nrow(lb_row) == 1
  fi <- function(x) format(x, big.mark = ",")

  # Caught once -> there are no recaptures to cross-check, so the consistency
  # checks below never run. Say that, rather than falling through to a green
  # "all consistent" that would over-claim a check that never happened.
  if (nrow(hist) < 2) {
    add("info", "Caught once — there are no recaptures to cross-check, so the consistency flags below don't apply.")
    return(flags)
  }

  # 1 — same tag, two plots, same day: spatially impossible (highest-confidence).
  if (has_row && isTRUE(lb_row$spatial_conflict[1]))
    add("high", "Recorded at two plots on the same day — physically impossible for one animal (plots are hundreds of metres apart). Almost always a tag-number mix-up or data-entry error.")

  # 2 — life stage moving backward across DISTINCT dates. Collapse same-day rows
  #     to that day's most-advanced stage first, so a same-day (adult, juvenile)
  #     pair (or a two-plot-one-day record) can't be misread as a regression.
  ord <- c(juvenile = 1L, subadult = 2L, adult = 3L)
  hs <- data.frame(date = hist$date, st = ord[as.character(hist$lifeStage)])
  hs <- hs[!is.na(hs$st) & !is.na(hs$date), , drop = FALSE]
  if (nrow(hs) >= 2) {
    per_day <- tapply(hs$st, as.character(hs$date), max)
    per_day <- per_day[order(as.Date(names(per_day)))]
    if (length(per_day) >= 2 && any(diff(per_day) < 0))
      add("high", "Life stage moves backward (adult → a younger stage) on a later date — biologically impossible; check the staging on the datasheet.")
  }

  # 3 — career span beyond any wild lifespan for these genera (>5 yr).
  if (has_row && isTRUE(lb_row$career_days[1] > 1825))
    add("high", sprintf("Career span of %s days exceeds any wild lifespan for these species (>5 yr) — verify the tag number isn't shared between two animals.", fi(lb_row$career_days[1])))

  # 4 — sex flip across recaptures: field sexing is error-prone, so flag (don't condemn).
  sx <- unique(hist$sex[hist$sex %in% c("M", "F")])
  if (length(sx) > 1)
    add("warn", "Sex was recorded as both M and F across captures. Field sexing is error-prone (especially non-reproductive animals), so this is worth a check — not necessarily an error.")

  # 5 — implausible weight jump: percent change FROM THE PRIOR capture (a fixed,
  #     reproducible baseline — not pmin, which would inflate the %), over a real
  #     interval of >=1 day and <=30 days, and ONLY when the prior capture was an
  #     adult — a juvenile/subadult can legitimately add >30% mass in under a
  #     month, so the flag would otherwise just relabel normal growth as a typo.
  ww <- hist[is.finite(hist$weight) & hist$weight > 0, c("date", "weight", "lifeStage"), drop = FALSE]
  if (nrow(ww) >= 2) {
    ww <- ww[order(ww$date), ]
    prevw <- ww$weight[-nrow(ww)]; nextw <- ww$weight[-1]
    dd <- as.numeric(diff(ww$date))
    young <- ww$lifeStage[-nrow(ww)] %in% c("juvenile", "subadult")
    pct <- (nextw - prevw) / prevw * 100
    bad <- which(is.finite(pct) & abs(pct) > 30 & dd >= 1 & dd <= 30 & !young)
    if (length(bad)) {
      i <- bad[which.max(abs(pct[bad]))]
      add("warn", sprintf("Weight changed %+.0f%% from the prior capture (%.1f → %.1f g) in %d day%s (%s → %s) — a swing this fast in an adult can be a transposed digit; check it against the capture table and reproductive state.",
        pct[i], prevw[i], nextw[i], dd[i], ifelse(dd[i] == 1, "", "s"),
        format(ww$date[i], "%Y-%m-%d"), format(ww$date[i + 1], "%Y-%m-%d")))
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
      add("info", sprintf("Hind-foot length spans %.0f mm across %d adult recaptures. Adult foot length is near-fixed, so a spread this wide usually reflects measurement differences (ruler vs caliper, a different tech, foot flex) rather than growth.", rng, nrow(ad)))
  }

  # 7 — same tag, more than one species ID: weakest signal (congener swaps are common).
  if (has_row && isTRUE(lb_row$id_uncertain[1]))
    add("info", "This tag was recorded under more than one species. Within-genus swaps are common and low-concern; a cross-genus change is worth verifying (NEON reconciles IDs later via its identification history).")

  flags
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
# (plotID, ym) with mnka and captures-per-100-trap-nights.
# ---------------------------------------------------------------------------
mnka_series <- function(d) {
  h <- dplyr::filter(d, !is.na(.data$tagID), !is.na(.data$ym), !is.na(.data$plotID))
  if (nrow(h) == 0) return(NULL)
  span <- h %>% dplyr::group_by(.data$plotID, .data$tagID) %>%
    dplyr::summarise(first = min(.data$ym), last = max(.data$ym), .groups = "drop")
  eff <- d %>% dplyr::filter(!is.na(.data$ym), !is.na(.data$plotID)) %>%
    dplyr::group_by(.data$plotID, .data$ym) %>%
    dplyr::summarise(trap_nights = sum(.data$trap_effort, na.rm = TRUE),
                     captures = sum(!is.na(.data$tagID)), .groups = "drop")
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
  out
}

# Keep only confirmed SPECIES-level identifications — drop genus-only "X sp."
# and ambiguous "A/B" records so an unidentified "Rodentia sp." isn't counted as
# its own species. This matters most for Chao1 (which scales with the singleton
# count, so each phantom "sp." inflates the estimate), but also for plain
# richness and the Hill profile. Uses NEON's taxonRank when present (the robust
# discriminator) with a scientific-name regex as a backstop. Mirrors the filter
# in build_site_index.R so the map, the stat cards, the diversity profile and the
# accumulation curve all agree on ONE species list. (Quinn review.)
species_level_only <- function(h) {
  if (is.null(h) || nrow(h) == 0) return(h)
  rank <- if ("taxonRank" %in% names(h)) h$taxonRank else rep(NA_character_, nrow(h))
  rank_ok <- is.na(rank) | rank %in% c("species", "subspecies", "speciesGroup")
  nm <- ifelse(is.na(h$scientificName), "", as.character(h$scientificName))
  ambiguous <- grepl("\\bsp\\.?$", nm) | grepl("/", nm, fixed = TRUE)
  h[rank_ok & !ambiguous, , drop = FALSE]
}

# Sample-based species accumulation over monthly bouts (Gotelli & Colwell 2001),
# averaged over permutations, + a bias-corrected Chao1 asymptotic richness
# estimate with a 95% CI (Chao 1987; Chao & Chiu 2016). Counts species-level IDs
# only; Chao1 is a LOWER BOUND and is flagged unstable when doubletons are scarce.
species_accum <- function(d, perms = 40) {
  h <- dplyr::filter(d, !is.na(.data$tagID), !is.na(.data$scientificName), !is.na(.data$ym))
  h <- species_level_only(h)                       # drop "X sp." / "A/B" so they aren't phantom species
  if (nrow(h) == 0) return(NULL)
  bouts <- split(h$scientificName, h$ym)
  k <- length(bouts)
  # deterministic permutation seeds (no Math.random / set.seed dependence on RNG state)
  perm_mat <- vapply(seq_len(perms), function(p) {
    ord <- order((seq_len(k) * (p * 2 + 1)) %% (k + 1))  # cheap varied orderings
    seen <- character(0); rich <- integer(k)
    for (i in seq_len(k)) { seen <- union(seen, bouts[[ord[i]]]); rich[i] <- length(seen) }
    rich
  }, integer(k))
  mean_rich <- rowMeans(perm_mat)
  sd_rich   <- apply(perm_mat, 1, stats::sd)

  # f1 / f2 = species represented by exactly 1 / 2 distinct individuals
  cnt <- h %>% dplyr::distinct(.data$tagID, .data$scientificName) %>%
    dplyr::count(.data$scientificName)
  f1 <- sum(cnt$n == 1); f2 <- sum(cnt$n == 2); sobs <- nrow(cnt)
  # Bias-corrected Chao1 (exact at f2==0, lower-variance than the classic
  # f1^2/(2 f2) form which is upward-biased & unstable for small f2).
  chao1 <- sobs + (f1 * (f1 - 1)) / (2 * (f2 + 1))
  # Chao (1987) log-normal 95% CI on the # of undetected species, + instability flag.
  T_extra <- chao1 - sobs
  if (f2 > 0) {
    r <- f1 / f2
    varC <- f2 * (0.5 * r^2 + r^3 + 0.25 * r^4)
  } else {
    varC <- max(0.25 * f1 * (2 * f1 - 1)^2 / (f2 + 1) - f1^4 / (4 * max(chao1, 1)), 0)
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
# Abundance = distinct INDIVIDUALS per species (not captures), so a heavily
# re-trapped animal isn't double-counted.
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
# Detection-corrected abundance — closed-capture estimation per trapping bout.
#
# NEON traps each grid in multi-night "bouts" (pathogen grids ~3 consecutive
# nights; diversity grids 1 night). On a closed multi-night bout we can correct
# the raw count for animals we never caught, using recaptures WITHIN the bout.
#
# Tier-1, dependency-free (base R + dplyr). Estimators:
#   k >= 3 nights : Schnabel (1938)   N = Σ(Cₜ·Mₜ) / Σ Rₜ
#   k == 2 nights : Chapman (1951)    N = (M+1)(C+1)/(R+1) − 1
#   k == 1 night  : no estimate (index only — that's what MNKA/CPUE are for)
# Detection p̂ under Otis et al. (1978) Model M0: p̂ = ΣCₜ / (k·N).
# Guard (critical): Schnabel → ∞ as ΣR → 0, so require ΣR ≥ 3 to report; clamp
# N ≥ MNKA (minimum known alive is a hard floor); CI computed in the 1/N domain
# (the estimate is skewed) then inverted, never ±SE on N directly.
# Spec: Fauna review (Schnabel 1938; Chapman 1951; Otis et al. 1978; Krebs 2017).
#
# Bout grouping: distinct (plotID, collectDate, tagID); within a plot a new bout
# starts when the gap to the previous trapping night is > 2 days (NEON allows a
# 1-night slip), keyed off consecutive collectDates (NOT a hardcoded "3 nights",
# since NEON reduced sampling at some sites). Recapture status is recomputed
# WITHIN bout (the raw `recapture` column carries cross-bout history).
# ---------------------------------------------------------------------------
RECAP_GATE <- 3L   # minimum ΣRₜ to report a point estimate
RECAP_GOOD <- 7L   # ΣRₜ at/above which precision is "good" (else low-precision)

bout_closed_capture <- function(d) {
  cap <- dplyr::filter(d, !is.na(.data$tagID), !is.na(.data$plotID), !is.na(.data$date)) %>%
    dplyr::distinct(.data$plotID, .data$date, .data$tagID, .data$ym)
  if (nrow(cap) == 0) return(NULL)

  # assign bouts: consecutive nights within a plot (gap > 2 days -> new bout)
  cap <- cap %>% dplyr::arrange(.data$plotID, .data$date)
  cap <- cap %>% dplyr::group_by(.data$plotID) %>%
    dplyr::mutate(gap = as.integer(.data$date - dplyr::lag(.data$date)),
                  new_bout = is.na(.data$gap) | .data$gap > 2,
                  bout = cumsum(.data$new_bout)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(boutID = paste0(.data$plotID, "#", .data$bout))

  # --- per-bout sufficient statistics, computed with a handful of grouped
  #     ops over the WHOLE table (not one dplyr call per bout — that was ~100x
  #     slower). Per night t within a bout: C = caught, U = newly marked,
  #     R = C-U recaps, M = marked-before-t = cumsum(U)-U.
  nights <- cap %>% dplyr::distinct(.data$boutID, .data$date) %>%
    dplyr::arrange(.data$boutID, .data$date) %>%
    dplyr::group_by(.data$boutID) %>% dplyr::mutate(t = dplyr::row_number()) %>%
    dplyr::ungroup()
  Cn <- cap %>% dplyr::count(.data$boutID, .data$date, name = "C")     # caught/night
  Un <- cap %>% dplyr::group_by(.data$boutID, .data$tagID) %>%
    dplyr::summarise(first = min(.data$date), .groups = "drop") %>%
    dplyr::count(.data$boutID, date = .data$first, name = "U")         # new/night
  per <- nights %>%
    dplyr::left_join(Cn, by = c("boutID", "date")) %>%
    dplyr::left_join(Un, by = c("boutID", "date"))
  per$C[is.na(per$C)] <- 0L; per$U[is.na(per$U)] <- 0L
  per <- per %>% dplyr::arrange(.data$boutID, .data$t) %>%
    dplyr::group_by(.data$boutID) %>%
    dplyr::mutate(M = cumsum(.data$U) - .data$U, R = .data$C - .data$U) %>%
    dplyr::ungroup()
  agg <- per %>% dplyr::group_by(.data$boutID) %>%
    dplyr::summarise(k = max(.data$t), sumC = sum(.data$C), sumR = sum(.data$R),
                     num = sum(.data$C * .data$M),
                     U1 = .data$U[.data$t == 1][1],
                     C2 = .data$C[.data$t == 2][1],
                     R2 = .data$R[.data$t == 2][1], .groups = "drop")
  bmeta <- cap %>% dplyr::group_by(.data$boutID) %>%
    dplyr::summarise(plotID = dplyr::first(.data$plotID), ym = dplyr::first(.data$ym),
                     start = min(.data$date), mnka = dplyr::n_distinct(.data$tagID),
                     .groups = "drop")
  est <- dplyr::left_join(bmeta, agg, by = "boutID")

  # --- estimator: pure arithmetic per bout (scalars; the loop is microseconds) -
  estimate_row <- function(k, sumC, sumR, num, U1, C2, R2, mnka) {
    out <- list(N = NA_real_, lo = NA_real_, hi = NA_real_, p = NA_real_,
                sumR = as.integer(if (is.na(sumR)) 0L else sumR), varN = NA_real_,
                status = "single-night")
    if (is.na(k) || k < 2) return(out)
    if (is.na(sumR) || sumR < RECAP_GATE) { out$status <- "insufficient recaptures"; return(out) }
    if (k >= 3) {                                  # Schnabel
      N <- num / sumR
      var_invN <- sumR / (num^2)
      invN <- 1 / N
      ci_inv <- invN + c(1, -1) * 1.96 * sqrt(var_invN)   # lo 1/N -> hi N
      lo <- if (ci_inv[1] > 0) 1 / ci_inv[1] else NA_real_
      hi <- if (ci_inv[2] > 0) 1 / ci_inv[2] else Inf
      varN <- var_invN * N^4                       # delta-method var(N) for roll-up
    } else {                                        # k == 2: Chapman
      M <- U1; C <- C2; R <- R2
      N <- ((M + 1) * (C + 1) / (R + 1)) - 1
      varN <- ((M + 1) * (C + 1) * (M - R) * (C - R)) / ((R + 1)^2 * (R + 2))
      se <- sqrt(max(varN, 0)); lo <- N - 1.96 * se; hi <- N + 1.96 * se
    }
    clamped <- FALSE
    if (is.finite(N) && N < mnka) { N <- mnka; clamped <- TRUE }   # MNKA is a hard floor
    lo <- max(lo, mnka, na.rm = TRUE)
    p  <- sumC / (k * N)
    out$N <- round(N, 1); out$lo <- round(lo, 1)
    out$hi <- if (is.finite(hi)) round(hi, 1) else Inf
    out$p <- round(min(max(p, 0), 1), 3); out$varN <- varN
    out$status <- if (clamped) "detection near-complete"
                  else if (sumR < RECAP_GOOD) "low-precision" else "ok"
    out
  }
  res <- lapply(seq_len(nrow(est)), function(i)
    estimate_row(est$k[i], est$sumC[i], est$sumR[i], est$num[i],
                 est$U1[i], est$C2[i], est$R2[i], est$mnka[i]))
  est$N      <- vapply(res, function(z) z$N, numeric(1))
  est$lo     <- vapply(res, function(z) z$lo, numeric(1))
  est$hi     <- vapply(res, function(z) z$hi, numeric(1))
  est$p      <- vapply(res, function(z) z$p, numeric(1))
  est$sumR   <- vapply(res, function(z) z$sumR, integer(1))
  est$varN   <- vapply(res, function(z) z$varN, numeric(1))
  est$status <- vapply(res, function(z) z$status, character(1))
  est[order(est$start), ]
}

# Roll per-bout estimates up to a per-month site series: SUM N̂ across grids
# (abundance adds), pool p̂ as ΣC/Σ(k·N̂), MNKA floor per month. Only months with
# at least one estimable bout get an abundance; others stay index-only.
closed_capture_series <- function(d, bouts = NULL) {
  if (is.null(bouts)) bouts <- bout_closed_capture(d)
  if (is.null(bouts) || nrow(bouts) == 0) return(NULL)
  est <- dplyr::filter(bouts, .data$status %in% c("ok", "low-precision", "detection near-complete"),
                       is.finite(.data$N))
  if (nrow(est) == 0)
    return(list(series = NULL, n_bouts = nrow(bouts),
                n_estimable = 0, mean_p = NA_real_, mean_detect = NA_real_))
  series <- est %>% dplyr::group_by(.data$ym) %>%
    dplyr::summarise(
      N = sum(.data$N),
      mnka = sum(.data$mnka),
      varN = sum(.data$varN, na.rm = TRUE),
      ckN = sum(.data$k * .data$N),
      capt = sum(.data$p * .data$k * .data$N),   # = ΣCₜ recovered from p̂=ΣC/(kN)
      n_grids = dplyr::n(), .groups = "drop") %>%
    dplyr::mutate(
      se = sqrt(pmax(.data$varN, 0)),
      lo = pmax(.data$mnka, .data$N - 1.96 * .data$se),
      hi = .data$N + 1.96 * .data$se,
      p  = round(.data$capt / .data$ckN, 3),
      date = as.Date(paste0(.data$ym, "-01"))) %>%
    dplyr::arrange(.data$date)
  list(series = series, n_bouts = nrow(bouts), n_estimable = nrow(est),
       mean_p = round(stats::weighted.mean(est$p, est$k * est$N), 3),
       mean_detect = round(stats::weighted.mean(pmin(est$mnka / est$N, 1), est$N), 3))
}

# Reproductive activity per row (adults): scrotal males / pregnant-or-lactating
# females. Returns the input rows with a `repro` factor added.
flag_repro <- function(d) {
  scrotal  <- grepl("scrotal", d$testes %||% "", ignore.case = TRUE) &
              !grepl("nonscrotal", d$testes %||% "", ignore.case = TRUE)
  preg     <- grepl("pregnant", d$pregnancyStatus %||% "", ignore.case = TRUE)
  lact     <- grepl("enlarged", d$nipples %||% "", ignore.case = TRUE)
  vag_open <- grepl("open|plugged|perforate", d$vagina %||% "", ignore.case = TRUE)
  d$repro <- dplyr::case_when(
    scrotal             ~ "breeding male",
    preg                ~ "pregnant female",
    lact | vag_open     ~ "lactating/receptive female",
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
      "The most-trapped mammal here is the <b><i>%s</i></b>%s — <b>%s</b> individuals across <b>%s</b> captures.",
      top$scientificName, nn, fmt_int(top$individuals), fmt_int(top$captures)))
    if (nrow(sp) >= 2) {
      # only species clearing the n>=8 adult floor are eligible (avg_weight is NA
      # below it) — never crown a "heaviest" off a 2-adult mean.
      elig <- sp[sp$n_adult >= 8L & is.finite(sp$avg_weight), ]
      if (nrow(elig) > 0) {
        hv <- elig[which.max(elig$avg_weight), ]
        if (hv$scientificName != top$scientificName)
          out <- c(out, sprintf(
            "The heaviest species caught is the <b><i>%s</i></b>, with adults averaging about <b>%s g</b> (n=%s) — one of the larger-bodied species at this site.",
            hv$scientificName, hv$avg_weight, fmt_int(hv$n_adult)))
      }
    }
  }

  if (!is.null(lb) && nrow(lb) > 0) {
    L <- lb[1, ]
    out <- c(out, sprintf(
      "The hardest-working individual is <b>%s</b> (<i>%s</i>), caught <b>%s times</b>%s — earning a <b>%s</b> rank.",
      L$short, L$scientificName, L$captures,
      if (!is.na(L$career_days) && L$career_days > 60) sprintf(" over %s days", L$career_days) else "",
      L$rarity))
  }

  out <- c(out, sprintf(
    "<b>%s%%</b> of captures were re-encounters of already-tagged animals%s.",
    cs$recap_rate,
    if (cs$recap_rate >= 45) " — a high recapture rate means the same residents keep returning, so the population is well-marked"
    else " — most animals were seen only once"))

  out <- c(out, sprintf(
    "It took roughly <b>%s trap-nights</b> across <b>%s plots</b> to gather all of this.",
    fmt_int(cs$trap_nights), cs$plots))

  sa <- tryCatch(species_accum(d), error = function(e) NULL)
  if (!is.null(sa)) {
    est <- if (isTRUE(sa$unstable))
      sprintf("at least <b>%s</b> (a soft lower bound — too few twice-seen species to pin down)", sa$chao1)
    else sprintf("about <b>%s</b> (95%% CI %s–%s)", sa$chao1, sa$chao_lo, sa$chao_hi)
    out <- c(out, sprintf(
      "<b>%s species</b> were found; a Chao1 estimate suggests %s are really present — sampling looks %s.",
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
  pack <- function(title, subtitle, insight, rows, icon = "\U0001F4CA")
    list(title = title, subtitle = subtitle, insight = insight, icon = icon, rows = rows)

  if (which == "species") {
    s <- species_summary(d)
    rows <- tibble::tibble(rank = seq_len(nrow(s)),
      name = paste0(s$emoji, "  <i>", s$scientificName, "</i>",
                    ifelse(is.na(s$nickname), "", paste0(" <span class='dim'>· ", s$nickname, "</span>"))),
      metric = paste0(fmt_int(s$captures), " caps"),
      sub = paste0(fmt_int(s$individuals), " individuals"))
    return(pack("Species, ranked by abundance", "Every species caught here, most common first",
      if (nrow(s)) sprintf("The <b><i>%s</i></b> dominates; the bottom of the list is where the rare finds hide.", s$scientificName[1]) else "",
      rows, "\U0001F9EC"))
  }
  if (which == "individuals") {
    v <- utils::head(lb, 25)
    rows <- tibble::tibble(rank = v$rank, tag = v$tagID,
      name = paste0(v$emoji, "  <b>", v$short, "</b> <span class='dim'>· ", v$scientificName, "</span>"),
      metric = paste0(v$captures, " caps"),
      sub = paste0(ifelse(v$career_days > 0, paste0(v$career_days, "d career"), "single capture")))
    return(pack("Most-caught individuals", "Tap any animal to open its full dossier",
      "These are the regulars — the animals that kept turning up in traps.", rows, "\U0001F50D"))
  }
  if (which == "captures") {
    cby <- d %>% dplyr::filter(!is.na(.data$tagID), !is.na(.data$plotID)) %>%
      dplyr::group_by(.data$plotID) %>%
      dplyr::summarise(caps = dplyr::n(), inds = dplyr::n_distinct(.data$tagID), .groups = "drop") %>%
      dplyr::arrange(dplyr::desc(.data$caps))
    rows <- tibble::tibble(rank = seq_len(nrow(cby)),
      name = paste0("\U0001F4CD ", cby$plotID), metric = paste0(fmt_int(cby$caps), " caps"),
      sub = paste0(fmt_int(cby$inds), " individuals"))
    return(pack("Captures by plot", "Where the action was across the site's trapping grids",
      if (nrow(cby)) sprintf("Plot <b>%s</b> was the busiest with %s captures.", cby$plotID[1], fmt_int(cby$caps[1])) else "",
      rows, "\U0001F3AF"))
  }
  if (which == "recapture") {
    rc <- d %>% dplyr::filter(!is.na(.data$tagID), !is.na(.data$scientificName)) %>%
      dplyr::group_by(.data$scientificName) %>%
      dplyr::summarise(caps = dplyr::n(),
        rate = round(100 * mean(.data$recapture %in% c("Y", "y")), 0), .groups = "drop") %>%
      dplyr::filter(.data$caps >= 5) %>% dplyr::arrange(dplyr::desc(.data$rate))
    rows <- tibble::tibble(rank = seq_len(nrow(rc)),
      name = paste0("<i>", rc$scientificName, "</i>"), metric = paste0(rc$rate, "%"),
      sub = paste0(fmt_int(rc$caps), " captures"))
    return(pack("Recapture rate by species", "Which species stick around and get re-caught",
      "A high recapture rate means resident animals that survive and return to traps.", rows, "\U0001F501"))
  }
  if (which == "trapnights") {
    eff <- d %>% dplyr::filter(!is.na(.data$plotID)) %>% dplyr::group_by(.data$plotID) %>%
      dplyr::summarise(tn = sum(.data$trap_effort, na.rm = TRUE),
        caps = sum(!is.na(.data$tagID)), .groups = "drop") %>%
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
      name = paste0(v$emoji, "  <b>", v$short, "</b> <span class='dim'>· ", v$scientificName, "</span>"),
      metric = paste0(v$captures, " caps"),
      sub = paste0(rarity_tier(v$captures)))
    return(pack("The Legends — caught 10+ times", "Tap any legend to open its dossier",
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
env_corr_scan <- function(d, env, layer, max_lag = 12) {
  meta <- ENV_LAYERS[[layer]]
  if (is.null(meta) || is.null(env) || !(meta$col %in% names(env))) return(NULL)
  m <- d %>% dplyr::filter(!is.na(.data$ym)) %>%
    dplyr::group_by(.data$ym) %>%
    dplyr::summarise(cap = sum(!is.na(.data$tagID)),
                     tn  = sum(.data$trap_effort, na.rm = TRUE), .groups = "drop")
  m <- m[m$tn > 0, , drop = FALSE]
  if (nrow(m) < 8) return(NULL)                 # honest overlap floor (was 4)
  m$cpue <- 100 * m$cap / m$tn
  m$date <- as.Date(paste0(m$ym, "-01"))
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
    dplyr::summarise(cap = sum(!is.na(.data$tagID)),
                     tn  = sum(.data$trap_effort, na.rm = TRUE), .groups = "drop")
  m <- m[m$tn > 0, , drop = FALSE]
  if (!nrow(m)) return(NULL)
  m$cpue <- 100 * m$cap / m$tn
  m$date <- as.Date(paste0(m$ym, "-01"))
  e <- shift_env(env, lag)
  e$.v <- suppressWarnings(as.numeric(e[[meta$col]]))
  e <- e[!is.na(e$.v), c("date", ".v"), drop = FALSE]
  j <- merge(m[, c("date", "cpue")], e, by = "date")
  if (nrow(j) < 3) return(NULL)
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
