# ---------------------------------------------------------------------------
# NEON terrestrial site metadata
# Lookup of site code -> human-readable name, NEON domain, region, coordinates,
# and a one-line ecosystem bio (so the site picker teaches as you choose).
# Source: NEON field site descriptions (https://www.neonscience.org/field-sites)
# ---------------------------------------------------------------------------

neon_sites <- tibble::tribble(
  ~site,  ~domain, ~name,                                          ~state, ~lat,     ~lng,      ~bio,
  "HARV", "D01",  "Harvard Forest",                                "MA",   42.5369,  -72.1727,  "Transition hardwood forest of central Massachusetts — maple, oak, and hemlock; NEON's flagship Northeast forest.",
  "BART", "D01",  "Bartlett Experimental Forest",                  "NH",   44.0639,  -71.2874,  "Northern hardwood forest in New Hampshire's White Mountains — beech, birch, and maple on glaciated slopes.",
  "BLAN", "D02",  "Blandy Experimental Farm",                      "VA",   39.0337,  -78.0419,  "Rolling pasture and oak–hickory woodlots in Virginia's northern Piedmont.",
  "SCBI", "D02",  "Smithsonian Conservation Biology Institute",    "VA",   38.8929,  -78.1395,  "Mature deciduous forest and old fields in the Blue Ridge foothills of Virginia.",
  "SERC", "D02",  "Smithsonian Environmental Research Center",     "MD",   38.8901,  -76.5600,  "Tidewater hardwood forest and wetlands along Maryland's Chesapeake Bay.",
  "DSNY", "D03",  "Disney Wilderness Preserve",                    "FL",   28.1250,  -81.4362,  "Restored longleaf-pine flatwoods, prairie, and cypress wetlands in central Florida.",
  "JERC", "D03",  "Jones Ecological Research Center",              "GA",   31.1948,  -84.4686,  "Longleaf-pine and wiregrass uplands of Georgia's southeastern Coastal Plain.",
  "OSBS", "D03",  "Ordway-Swisher Biological Station",             "FL",   29.6893,  -81.9934,  "Fire-maintained longleaf-pine sandhills and scrub on the central Florida ridge.",
  "GUAN", "D04",  "Guanica Forest",                                "PR",   17.9696, -66.8687,   "Subtropical dry forest on Puerto Rico's limestone south coast — a rare, well-preserved Caribbean dry forest.",
  "LAJA", "D04",  "Lajas Experimental Station",                    "PR",   18.0213, -67.0769,   "Agricultural valley and dry-forest margins of southwestern Puerto Rico.",
  "STEI", "D05",  "Steigerwaldt-Chequamegon",                      "WI",   45.5089, -89.5864,   "Northern mixed forest and managed timberland in north-central Wisconsin.",
  "TREE", "D05",  "Treehaven",                                     "WI",   45.4937, -89.5857,   "Second-growth northern hardwood–conifer forest in Wisconsin's Northwoods.",
  "UNDE", "D05",  "UNDERC",                                        "MI",   46.2339, -89.5373,   "Lake-dotted northern hardwood and conifer forest of Michigan's Upper Peninsula.",
  "KONA", "D06",  "Konza Prairie Agricultural Site",               "KS",   39.1104, -96.6129,   "Cropland and tallgrass-prairie edge in the Kansas Flint Hills.",
  "KONZ", "D06",  "Konza Prairie Biological Station",              "KS",   39.1008, -96.5631,   "Iconic tallgrass prairie of the Kansas Flint Hills — fire-managed and bison-grazed.",
  "UKFS", "D06",  "University of Kansas Field Station",             "KS",   39.0404, -95.1921,   "Successional oak–hickory woodland and old fields in eastern Kansas.",
  "GRSM", "D07",  "Great Smoky Mountains National Park",           "TN",   35.6890, -83.5019,   "Species-rich southern Appalachian forest spanning cove hardwoods to spruce–fir.",
  "MLBS", "D07",  "Mountain Lake Biological Station",              "VA",   37.3783, -80.5248,   "High-elevation Appalachian forest in the mountains of southwest Virginia.",
  "ORNL", "D07",  "Oak Ridge",                                     "TN",   35.9641, -84.2826,   "Oak–hickory and pine forest on the ridges of the Tennessee Valley.",
  "DELA", "D08",  "Dead Lake",                                     "AL",   32.5417, -87.8039,   "Bottomland hardwood forest and swamp along Alabama's Tombigbee River.",
  "LENO", "D08",  "Lenoir Landing",                                "AL",   31.8539, -88.1612,   "Floodplain forest and wetlands of the lower Tombigbee in Alabama.",
  "TALL", "D08",  "Talladega National Forest",                     "AL",   32.9505, -87.3933,   "Longleaf and loblolly pine uplands of Alabama's Piedmont.",
  "DCFS", "D09",  "Dakota Coteau Field School",                    "ND",   47.1617, -99.1066,   "Northern mixed-grass prairie and prairie potholes of North Dakota.",
  "NOGP", "D09",  "Northern Great Plains Research Laboratory",     "ND",   46.7697, -100.9154,  "Mixed-grass prairie research rangeland on the Northern Great Plains.",
  "WOOD", "D09",  "Woodworth",                                     "ND",   47.1282, -99.2413,   "Prairie-pothole grassland and wetlands of central North Dakota.",
  "CPER", "D10",  "Central Plains Experimental Range",             "CO",   40.8155, -104.7456,  "Shortgrass steppe of the Colorado High Plains — a century of grazing research.",
  "RMNP", "D10",  "Rocky Mountain National Park",                  "CO",   40.2759, -105.5455,  "Montane forest and meadow on Colorado's Front Range.",
  "STER", "D10",  "North Sterling",                                "CO",   40.4619, -103.0293,  "Irrigated and dryland cropland of the Colorado eastern plains.",
  "CLBJ", "D11",  "Lyndon B. Johnson National Grassland",          "TX",   33.4012, -97.5700,   "Cross Timbers oak savanna and grassland of north-central Texas.",
  "OAES", "D11",  "Klemme Range Research Station",                 "OK",   35.4106, -99.0588,   "Southern mixed-grass prairie and rangeland of western Oklahoma.",
  "YELL", "D12",  "Yellowstone National Park",                     "WY",   44.9535, -110.5391,  "Sagebrush steppe and conifer forest on the Yellowstone plateau of Wyoming.",
  "MOAB", "D13",  "Moab",                                          "UT",   38.2483, -109.3883,  "Cold-desert shrubland on the Colorado Plateau near Moab, Utah.",
  "NIWO", "D13",  "Niwot Ridge",                                   "CO",   40.0543, -105.5824,  "Alpine tundra above treeline in Colorado's Front Range.",
  "JORN", "D14",  "Jornada Experimental Range",                    "NM",   32.5907, -106.8425,  "Chihuahuan Desert grassland and shrubland of southern New Mexico — kangaroo-rat country.",
  "SRER", "D14",  "Santa Rita Experimental Range",                 "AZ",   31.9107, -110.8355,  "Sonoran Desert semi-desert grassland south of Tucson, Arizona.",
  "ONAQ", "D15",  "Onaqui",                                        "UT",   40.1776, -112.4524,  "Great Basin sagebrush steppe and pinyon–juniper of western Utah.",
  "ABBY", "D16",  "Abby Road",                                     "WA",   45.7624, -122.3303,  "Douglas-fir forest and clearcut mosaic in the foothills of the Washington Cascades.",
  "WREF", "D16",  "Wind River Experimental Forest",               "WA",   45.8205, -121.9519,  "Old-growth Douglas-fir and hemlock forest of the southern Washington Cascades.",
  "SJER", "D17",  "San Joaquin Experimental Range",               "CA",   37.1088, -119.7323,  "Oak savanna and annual grassland of California's Sierra Nevada foothills.",
  "SOAP", "D17",  "Soaproot Saddle",                               "CA",   37.0334, -119.2622,  "Mixed-conifer and oak forest of the central Sierra Nevada, California.",
  "TEAK", "D17",  "Lower Teakettle",                               "CA",   37.0058, -119.0060,  "High Sierra Nevada mixed-conifer forest in California.",
  "BARR", "D18",  "Utqiagvik (Barrow)",                            "AK",   71.2824, -156.6194,  "Arctic coastal tundra at the northern tip of Alaska.",
  "TOOL", "D18",  "Toolik Lake",                                   "AK",   68.6611, -149.3705,  "Arctic foothills tundra on Alaska's North Slope.",
  "BONA", "D19",  "Caribou-Poker Creeks Research Watershed",       "AK",   65.1540, -147.5026,  "Boreal black-spruce forest and permafrost of interior Alaska.",
  "DEJU", "D19",  "Delta Junction",                                "AK",   63.8811, -145.7514,  "Boreal forest and fire mosaic of interior Alaska.",
  "HEAL", "D19",  "Healy",                                         "AK",   63.8758, -149.2133,  "Boreal-to-tundra transition near Denali, Alaska."
)

# full state name for grouping the picker
state_names <- c(
  AK = "Alaska", AL = "Alabama", AZ = "Arizona", CA = "California", CO = "Colorado",
  FL = "Florida", GA = "Georgia", KS = "Kansas", MA = "Massachusetts", MD = "Maryland",
  MI = "Michigan", ND = "North Dakota", NH = "New Hampshire", NM = "New Mexico",
  OK = "Oklahoma", PR = "Puerto Rico", TN = "Tennessee", TX = "Texas", UT = "Utah",
  VA = "Virginia", WA = "Washington", WI = "Wisconsin", WY = "Wyoming"
)

# State -> full-name choices, for the first (state) dropdown
state_choices <- function() {
  st <- sort(unique(neon_sites$state))
  setNames(st, sprintf("%s (%d %s)", state_names[st], table(neon_sites$state)[st],
                       ifelse(table(neon_sites$state)[st] == 1, "site", "sites")))
}

# Sites within one state -> "JORN — Jornada Experimental Range" choices
sites_in_state <- function(st) {
  rows <- neon_sites[neon_sites$state == st, ]
  rows <- rows[order(rows$name), ]
  if (nrow(rows) == 0) return(character(0))
  setNames(rows$site, sprintf("%s — %s", rows$site, rows$name))
}

site_bio <- function(code) {
  row <- neon_sites[neon_sites$site == code, ]
  if (nrow(row) == 0) return(NULL)
  row$bio[1]
}

site_label <- function(code) {
  row <- neon_sites[neon_sites$site == code, ]
  if (nrow(row) == 0) return(code)
  sprintf("%s · %s, %s · NEON %s", row$name[1], row$site[1], row$state[1], row$domain[1])
}
