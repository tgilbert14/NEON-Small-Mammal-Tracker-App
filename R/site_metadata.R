# ---------------------------------------------------------------------------
# NEON terrestrial site metadata
# Lookup of site code -> human-readable name, NEON domain, and region.
# Used to make the site picker friendly and to label headers / overview map.
# Source: NEON field site list (https://www.neonscience.org/field-sites)
# ---------------------------------------------------------------------------

neon_sites <- tibble::tribble(
  ~site,  ~domain, ~name,                                          ~state, ~lat,     ~lng,
  "HARV", "D01",  "Harvard Forest",                                "MA",   42.5369,  -72.1727,
  "BART", "D01",  "Bartlett Experimental Forest",                  "NH",   44.0639,  -71.2874,
  "BLAN", "D02",  "Blandy Experimental Farm",                      "VA",   39.0337,  -78.0419,
  "SCBI", "D02",  "Smithsonian Conservation Biology Institute",    "VA",   38.8929,  -78.1395,
  "SERC", "D02",  "Smithsonian Environmental Research Center",     "MD",   38.8901,  -76.5600,
  "DSNY", "D03",  "Disney Wilderness Preserve",                    "FL",   28.1250,  -81.4362,
  "JERC", "D03",  "Jones Ecological Research Center",              "GA",   31.1948,  -84.4686,
  "OSBS", "D03",  "Ordway-Swisher Biological Station",             "FL",   29.6893,  -81.9934,
  "GUAN", "D04",  "Guanica Forest",                                "PR",   17.9696, -66.8687,
  "LAJA", "D04",  "Lajas Experimental Station",                    "PR",   18.0213, -67.0769,
  "STEI", "D05",  "Steigerwaldt-Chequamegon",                      "WI",   45.5089, -89.5864,
  "TREE", "D05",  "Treehaven",                                     "WI",   45.4937, -89.5857,
  "UNDE", "D05",  "University of Notre Dame Env. Research Center",  "MI",   46.2339, -89.5373,
  "KONA", "D06",  "Konza Prairie Agricultural Site",               "KS",   39.1104, -96.6129,
  "KONZ", "D06",  "Konza Prairie Biological Station",              "KS",   39.1008, -96.5631,
  "UKFS", "D06",  "University of Kansas Field Station",             "KS",   39.0404, -95.1921,
  "GRSM", "D07",  "Great Smoky Mountains National Park",           "TN",   35.6890, -83.5019,
  "MLBS", "D07",  "Mountain Lake Biological Station",              "VA",   37.3783, -80.5248,
  "ORNL", "D07",  "Oak Ridge",                                     "TN",   35.9641, -84.2826,
  "DELA", "D08",  "Dead Lake",                                     "AL",   32.5417, -87.8039,
  "LENO", "D08",  "Lenoir Landing",                                "AL",   31.8539, -88.1612,
  "TALL", "D08",  "Talladega National Forest",                     "AL",   32.9505, -87.3933,
  "DCFS", "D09",  "Dakota Coteau Field School",                    "ND",   47.1617, -99.1066,
  "NOGP", "D09",  "Northern Great Plains Research Laboratory",     "ND",   46.7697, -100.9154,
  "WOOD", "D09",  "Woodworth",                                     "ND",   47.1282, -99.2413,
  "CPER", "D10",  "Central Plains Experimental Range",             "CO",   40.8155, -104.7456,
  "RMNP", "D10",  "Rocky Mountain National Park",                  "CO",   40.2759, -105.5455,
  "STER", "D10",  "North Sterling",                                "CO",   40.4619, -103.0293,
  "CLBJ", "D11",  "Lyndon B. Johnson National Grassland",          "TX",   33.4012, -97.5700,
  "OAES", "D11",  "Klemme Range Research Station",                 "OK",   35.4106, -99.0588,
  "YELL", "D12",  "Yellowstone National Park",                     "WY",   44.9535, -110.5391,
  "MOAB", "D13",  "Moab",                                          "UT",   38.2483, -109.3883,
  "NIWO", "D13",  "Niwot Ridge",                                   "CO",   40.0543, -105.5824,
  "JORN", "D14",  "Jornada Experimental Range",                    "NM",   32.5907, -106.8425,
  "SRER", "D14",  "Santa Rita Experimental Range",                 "AZ",   31.9107, -110.8355,
  "ONAQ", "D15",  "Onaqui",                                        "UT",   40.1776, -112.4524,
  "ABBY", "D16",  "Abby Road",                                     "WA",   45.7624, -122.3303,
  "WREF", "D16",  "Wind River Experimental Forest",               "WA",   45.8205, -121.9519,
  "SJER", "D17",  "San Joaquin Experimental Range",               "CA",   37.1088, -119.7323,
  "SOAP", "D17",  "Soaproot Saddle",                               "CA",   37.0334, -119.2622,
  "TEAK", "D17",  "Lower Teakettle",                               "CA",   37.0058, -119.0060,
  "BARR", "D18",  "Utqiagvik (Barrow)",                            "AK",   71.2824, -156.6194,
  "TOOL", "D18",  "Toolik Lake",                                   "AK",   68.6611, -149.3705,
  "BONA", "D19",  "Caribou-Poker Creeks Research Watershed",       "AK",   65.1540, -147.5026,
  "DEJU", "D19",  "Delta Junction",                                "AK",   63.8811, -145.7514,
  "HEAL", "D19",  "Healy",                                         "AK",   63.8758, -149.2133
)

# Convenience: named vector for selectInput choices -> "JORN — Jornada Experimental Range (NM)"
site_choices <- function() {
  labels <- sprintf("%s — %s (%s)", neon_sites$site, neon_sites$name, neon_sites$state)
  setNames(neon_sites$site, labels)
}

site_label <- function(code) {
  row <- neon_sites[neon_sites$site == code, ]
  if (nrow(row) == 0) return(code)
  sprintf("%s · %s, %s · %s", row$name[1], row$state[1], "NEON", row$domain[1])
}
