# ---------------------------------------------------------------------------
# helpers.R — the analytical engine for the NEON Ground Beetle Tracker
#
# Pure(ish) functions that turn a NEON ground-beetle (carabid) long table —
# one row per site / plot / bout / species with an individualCount and the
# trap-night effort — into the metrics that power the app: community
# composition, Hill-number diversity, individual-based rarefaction, species
# accumulation across bouts, and seasonal activity.
#
# Defensive throughout: NEON tables carry NAs, morphospecies, and uneven
# effort, so every function guards against empty / all-NA inputs and always
# normalises abundance to catch-per-trap-night where effort matters.
# ---------------------------------------------------------------------------

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a

mode_chr <- function(x) {
  x <- x[!is.na(x) & x != ""]
  if (length(x) == 0) return(NA_character_)
  names(sort(table(x), decreasing = TRUE))[1]
}

# Normalise a raw/bundled beetle table into the columns the app leans on.
clean_beetle <- function(d) {
  if (is.null(d) || nrow(d) == 0) return(NULL)
  d <- tibble::as_tibble(d)
  need <- c("siteID", "plotID", "collectDate", "taxonID", "scientificName",
            "individualCount", "trapnights")
  for (col in need) if (!col %in% names(d)) d[[col]] <- NA
  d$individualCount <- suppressWarnings(as.numeric(d$individualCount))
  d$trapnights      <- suppressWarnings(as.numeric(d$trapnights))
  d$date <- as.Date(substr(as.character(d$collectDate), 1, 10))
  d$year <- as.integer(format(d$date, "%Y"))
  d$ym   <- substr(as.character(d$date), 1, 7)
  d$mon  <- as.integer(format(d$date, "%m"))
  # drop rows with no count or no species id; keep genus/morphospecies as-is
  d <- d[!is.na(d$individualCount) & d$individualCount > 0 &
         !is.na(d$scientificName) & d$scientificName != "", , drop = FALSE]
  d
}

# Per-species totals for the loaded site/window, most abundant first. `cpn` is
# catch per 100 trap-nights so sites/windows with different effort compare.
community_table <- function(d) {
  if (is.null(d) || nrow(d) == 0) return(NULL)
  tn <- effort_trapnights(d)
  out <- d %>%
    dplyr::group_by(.data$scientificName) %>%
    dplyr::summarise(individuals = sum(.data$individualCount, na.rm = TRUE),
                     bouts = dplyr::n_distinct(.data$collectDate),
                     .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(.data$individuals))
  out$cpn <- if (tn > 0) round(100 * out$individuals / tn, 2) else NA_real_
  out
}

# Total trap-night effort = sum of unique (plot, bout) trap-night values, so a
# species count isn't multiplied by however many species shared that sample.
effort_trapnights <- function(d) {
  if (is.null(d) || !"trapnights" %in% names(d)) return(NA_real_)
  u <- unique(d[, c("plotID", "collectDate", "trapnights"), drop = FALSE])
  sum(u$trapnights, na.rm = TRUE)
}

# ---- diversity: Hill numbers ----------------------------------------------
# Three views of diversity in one unit — an EFFECTIVE number of species —
# indexed by q (how much rare species count): q0 = richness, q1 = exp(Shannon)
# (common species), q2 = inverse Simpson (dominant species). Hill 1973; Jost 2006.
hill_numbers <- function(counts) {
  n <- counts[!is.na(counts) & counts > 0]
  if (length(n) == 0) return(list(q0 = NA, q1 = NA, q2 = NA, even = NA))
  p <- n / sum(n)
  q0 <- length(n)
  shannon <- -sum(p * log(p))
  q1 <- exp(shannon)
  q2 <- 1 / sum(p^2)
  list(q0 = q0, q1 = round(q1, 2), q2 = round(q2, 2),
       even = if (q0 > 1) round(q1 / q0, 2) else NA)  # Pielou-style evenness
}

# ---- individual-based rarefaction (Hurlbert 1971) -------------------------
# Expected species E[S_n] in a random subsample of n individuals, with an
# analytic SD (Heck et al. 1975). Lets you compare richness at equal sample
# size instead of being fooled by who simply caught more beetles.
rarefaction_curve <- function(counts, step = NULL) {
  Ni <- counts[!is.na(counts) & counts > 0]
  N  <- sum(Ni)
  if (N < 2 || length(Ni) < 1) return(NULL)
  if (is.null(step)) step <- max(1L, floor(N / 40))
  ns <- unique(c(seq(1, N, by = step), N))
  es <- sd <- numeric(length(ns))
  lcN <- lchoose(N, ns)
  for (j in seq_along(ns)) {
    n <- ns[j]
    # P(species i absent from subsample) = C(N-Ni, n)/C(N, n)
    pa <- exp(lchoose(N - Ni, n) - lcN[j])
    pa[!is.finite(pa)] <- 0
    es[j] <- sum(1 - pa)
    # variance per Heck/Smith-Grassle approximation
    term <- pa * (1 - pa)
    sd[j] <- sqrt(max(0, sum(term)))
  }
  tibble::tibble(n = ns, richness = es, lo = pmax(0, es - sd), hi = es + sd)
}

# ---- species accumulation across bouts (sample-based) ---------------------
# As trapping bouts accumulate, how many species have been seen? A flattening
# curve means the site's beetle fauna is well sampled. Averaged over random
# bout orderings (Gotelli & Colwell 2001) for a smooth, order-free curve.
accumulation_by_bout <- function(d, perms = 50) {
  if (is.null(d) || nrow(d) == 0) return(NULL)
  d$boutid <- paste(d$plotID, d$collectDate)
  bouts <- unique(d$boutid)
  K <- length(bouts)
  if (K < 2) return(NULL)
  by_b <- split(d$scientificName, d$boutid)
  set.seed(1)
  acc <- matrix(NA_real_, nrow = perms, ncol = K)
  for (p in seq_len(perms)) {
    ord <- sample(bouts)
    seen <- character(0)
    for (k in seq_len(K)) {
      seen <- union(seen, by_b[[ord[k]]])
      acc[p, k] <- length(seen)
    }
  }
  tibble::tibble(bouts = seq_len(K),
                 richness = colMeans(acc),
                 sd = apply(acc, 2, stats::sd))
}

# ---- seasonality ----------------------------------------------------------
# Mean catch-per-100-trap-night by calendar month — the activity-density
# curve. Optionally per species (top `top_n` by total abundance).
seasonality <- function(d, by_species = FALSE, top_n = 6) {
  if (is.null(d) || nrow(d) == 0) return(NULL)
  eff <- unique(d[, c("plotID", "collectDate", "trapnights")])
  eff$mon <- as.integer(format(as.Date(eff$collectDate), "%m"))
  mon_eff <- stats::aggregate(trapnights ~ mon, eff, sum)
  if (!by_species) {
    cap <- stats::aggregate(individualCount ~ mon, d, sum)
    m <- merge(cap, mon_eff, by = "mon")
    m$cpn <- 100 * m$individualCount / m$trapnights
    return(tibble::as_tibble(m[order(m$mon), c("mon", "cpn")]))
  }
  keep <- names(sort(tapply(d$individualCount, d$scientificName, sum),
                     decreasing = TRUE))[seq_len(min(top_n, length(unique(d$scientificName))))]
  sub <- d[d$scientificName %in% keep, ]
  cap <- stats::aggregate(individualCount ~ mon + scientificName, sub, sum)
  m <- merge(cap, mon_eff, by = "mon")
  m$cpn <- 100 * m$individualCount / m$trapnights
  tibble::as_tibble(m[order(m$scientificName, m$mon), c("mon", "scientificName", "cpn")])
}

# Stable species -> color map so a species is the same color everywhere.
make_species_pal <- function(d) {
  sp <- sort(unique(d$scientificName[!is.na(d$scientificName)]))
  if (length(sp) == 0) return(character(0))
  cols <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, "Dark2"))(length(sp))
  stats::setNames(cols, sp)
}

# Genus -> a one-line natural-history blurb for the "meet the beetles" cards.
beetle_blurb <- function(scientificName) {
  g <- sub(" .*$", "", scientificName %||% "")
  lut <- c(
    Pterostichus = "Glossy black woodland predators — fast night hunters of soft-bodied prey and a classic forest-floor carabid.",
    Carabus      = "Big, sculptured 'caterpillar hunters' that can't fly; flagship beetles of healthy forest soils.",
    Calosoma     = "Iridescent 'searchers' that climb plants to hunt caterpillars — voracious agricultural allies.",
    Harpalus     = "Stout seed-eating ground beetles (granivores) common in open prairies and fields.",
    Poecilus     = "Metallic-green active hunters of grasslands, abundant through the warm season.",
    Pasimachus   = "Large flightless predators with massive jaws that patrol bare desert and prairie soil at night.",
    Scarites     = "Pincer-jawed burrowers that ambush prey from tunnels in loose soil.",
    Cyclotrachelus = "Robust late-season woodland and prairie predators active into autumn.",
    Cicindela    = "Tiger beetles — dazzling, fast-running visual hunters of sunbaked open ground.",
    Synuchus     = "Small forest carabids that forage in leaf litter and shelter under bark.",
    Amara        = "Sun-loving seed and plant feeders of open, dry habitats.",
    Notiobia     = "Warm-climate seed-eating beetles of sandy southern soils.",
    Cratacanthus = "Small granivorous ground beetles of arid grassland and desert.",
    Sphaeroderus = "Snail-hunting woodland beetles with narrow heads built to reach into shells.",
    Agonum       = "Slender, often metallic beetles of damp ground and wetland margins.",
    Cychrus      = "Snail-specialist forest beetles with elongate snouts."
  )
  unname(lut[g]) %||% "A NEON-sampled ground beetle (Carabidae) — a sensitive bioindicator of habitat and climate."
}

# Number formatting used across cards/tables.
fmt_int <- function(x) formatC(x, format = "d", big.mark = ",")

# ---------------------------------------------------------------------------
# assemble_beetles() — turn a neonUtilities loadByProduct() result for
# DP1.10022.001 into the app's tidy long schema:
#   siteID, plotID, collectDate, taxonID, scientificName, individualCount, trapnights
#
# Steps (the canonical EFI/neonDivData recipe, kept pragmatic for a scaffold):
#  1. carabid counts from bet_sorting (sampleType == "carabid")
#  2. reconcile taxonomy: override the parataxonomist call with the authoritative
#     EXPERT call where one exists for that taxonID
#  3. effort: trap-nights per plot × bout from bet_fielddata$daysOfTrapping,
#     summed over the traps actually set — this absorbs the 2018 trap-count and
#     2023 plot-count protocol changes, so abundance stays per-trap-night.
#
# Defensive about column names (NEON renames tables occasionally); a maintainer
# should confirm names once with names(loadByProduct(...)). Used by both the
# live-fetch path (global.R) and the bundle builder (scripts/refresh_data.R).
# ---------------------------------------------------------------------------
assemble_beetles <- function(raw) {
  if (is.null(raw) || is.null(raw$bet_sorting)) stop("no bet_sorting table in result")
  srt <- tibble::as_tibble(raw$bet_sorting)
  if ("sampleType" %in% names(srt))
    srt <- srt[!is.na(srt$sampleType) & srt$sampleType == "carabid", , drop = FALSE]
  if (!nrow(srt)) return(NULL)
  for (col in c("individualCount", "taxonID", "scientificName", "plotID",
                "collectDate", "siteID"))
    if (!col %in% names(srt)) srt[[col]] <- NA
  srt$individualCount <- suppressWarnings(as.numeric(srt$individualCount))

  # 2) expert-ID override (authoritative). Build a taxonID -> expert name lookup.
  exp <- raw$bet_expertTaxonomistIDProcessed
  if (!is.null(exp) && nrow(exp)) {
    exp <- tibble::as_tibble(exp)
    if (all(c("taxonID", "scientificName") %in% names(exp))) {
      lut <- unique(exp[!is.na(exp$taxonID) & !is.na(exp$scientificName),
                        c("taxonID", "scientificName")])
      lut <- lut[!duplicated(lut$taxonID), , drop = FALSE]
      hit <- match(srt$taxonID, lut$taxonID)
      srt$scientificName <- ifelse(is.na(hit), srt$scientificName,
                                   lut$scientificName[hit])
    }
  }

  counts <- srt %>%
    dplyr::filter(!is.na(.data$individualCount), .data$individualCount > 0,
                  !is.na(.data$scientificName)) %>%
    dplyr::group_by(.data$siteID, .data$plotID, .data$collectDate,
                    .data$taxonID, .data$scientificName) %>%
    dplyr::summarise(individualCount = sum(.data$individualCount), .groups = "drop")

  # 3) trap-night effort per plot × bout from field data
  fd <- raw$bet_fielddata
  eff <- NULL
  if (!is.null(fd) && nrow(fd)) {
    fd <- tibble::as_tibble(fd)
    dcol <- intersect(c("daysOfTrapping", "trappingDays"), names(fd))
    if (length(dcol) && all(c("plotID", "collectDate") %in% names(fd))) {
      fd$.days <- suppressWarnings(as.numeric(fd[[dcol[1]]]))
      eff <- fd %>% dplyr::filter(!is.na(.data$.days)) %>%
        dplyr::group_by(.data$plotID, .data$collectDate) %>%
        dplyr::summarise(trapnights = sum(.data$.days), .groups = "drop")
    }
  }
  out <- if (is.null(eff)) dplyr::mutate(counts, trapnights = NA_real_)
         else dplyr::left_join(counts, eff, by = c("plotID", "collectDate"))
  out$source <- "neon"
  tibble::as_tibble(out)
}
