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

# Largest gap (days) between an individual's consecutive captures — feeds the
# tag-reuse suspicion flag.
max_gap_days <- function(dates) {
  dates <- sort(dates[!is.na(dates)])
  if (length(dates) < 2) return(NA_integer_)
  as.integer(max(diff(dates)))
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
      max_gap_days   = max_gap_days(.data$date),
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
      # Tag-reuse guard (conservative, so it doesn't fire on NEON's normal
      # seasonal winter gaps): a "career" beyond what these small heteromyids/
      # cricetids plausibly live (~550 d ≈ 1.5 yr) OR a gap of a full missed
      # year+ (>300 d) is almost certainly a recycled ear-tag = two animals.
      tag_suspect = (.data$career_days > 550) | (!is.na(.data$max_gap_days) & .data$max_gap_days > 300),
      id_uncertain = .data$n_species_ids > 1
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
  # for the "career" board, hide the obvious tag-reuse artifacts so the ranking
  # is honest; everywhere else keep all rows
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
    species        = dplyr::n_distinct(handled$scientificName[!is.na(handled$scientificName)]),
    plots          = dplyr::n_distinct(d$plotID[!is.na(d$plotID)]),
    trap_nights    = sum(d$is_set, na.rm = TRUE),
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
  dplyr::filter(d, !is.na(.data$tagID), !is.na(.data$scientificName)) %>%
    dplyr::group_by(.data$scientificName) %>%
    dplyr::summarise(
      individuals = dplyr::n_distinct(.data$tagID),
      captures    = dplyr::n(),
      avg_weight  = round(safe_mean(.data$weight), 1),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(.data$captures)) %>%
    dplyr::mutate(emoji = genus_emoji(.data$scientificName),
                  nickname = species_nickname(.data$scientificName))
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
    dplyr::summarise(trap_nights = sum(.data$is_set, na.rm = TRUE),
                     captures = sum(!is.na(.data$tagID)), .groups = "drop")
  out <- eff %>% dplyr::rowwise() %>%
    dplyr::mutate(mnka = sum(span$plotID == .data$plotID &
                             span$first <= .data$ym & span$last >= .data$ym)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(cpue = dplyr::if_else(.data$trap_nights > 0,
                                        round(100 * .data$captures / .data$trap_nights, 1), NA_real_),
                  date = as.Date(paste0(.data$ym, "-01")))
  out
}

# Sample-based species accumulation over monthly bouts (Gotelli & Colwell 2001),
# averaged over permutations, + a Chao1 asymptotic richness estimate.
species_accum <- function(d, perms = 40) {
  h <- dplyr::filter(d, !is.na(.data$tagID), !is.na(.data$scientificName), !is.na(.data$ym))
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

  # Chao1 from per-species individual counts
  cnt <- h %>% dplyr::distinct(.data$tagID, .data$scientificName) %>%
    dplyr::count(.data$scientificName)
  f1 <- sum(cnt$n == 1); f2 <- sum(cnt$n == 2); sobs <- nrow(cnt)
  chao1 <- if (f2 > 0) sobs + f1^2 / (2 * f2) else sobs + f1 * (f1 - 1) / 2

  list(curve = tibble::tibble(bouts = seq_len(k), richness = mean_rich,
                              lo = pmax(0, mean_rich - sd_rich), hi = mean_rich + sd_rich),
       sobs = sobs, chao1 = round(chao1, 1))
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

  if (nrow(sp) > 0) {
    top <- sp[1, ]
    nn <- if (!is.na(top$nickname)) sprintf(" (the %s)", top$nickname) else ""
    out <- c(out, sprintf(
      "The most-trapped mammal here is the <b><i>%s</i></b>%s — <b>%s</b> individuals across <b>%s</b> captures.",
      top$scientificName, nn, fmt_int(top$individuals), fmt_int(top$captures)))
    if (nrow(sp) >= 2) {
      hv <- sp[which.max(replace(sp$avg_weight, is.na(sp$avg_weight), -Inf)), ]
      if (is.finite(hv$avg_weight) && hv$scientificName != top$scientificName)
        out <- c(out, sprintf(
          "The heaviest species caught is the <b><i>%s</i></b>, averaging about <b>%s g</b> — many times the weight of the little pocket mice.",
          hv$scientificName, hv$avg_weight))
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
  if (!is.null(sa))
    out <- c(out, sprintf(
      "<b>%s species</b> were found; a Chao1 estimate suggests about <b>%s</b> are really present — sampling looks %s.",
      sa$sobs, sa$chao1,
      if (sa$sobs >= 0.85 * sa$chao1) "close to complete" else "like it may still be missing a rare species or two"))

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
      dplyr::summarise(tn = sum(.data$is_set, na.rm = TRUE),
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
