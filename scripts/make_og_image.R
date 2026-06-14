#----------------------------------------------------------------------
# make_og_image.R — draws docs/og-image.png (1200x630), the social card for
# the landing page. Self-contained base-R graphics in the Desert Data Labs /
# "Girth Index" house palette (navy + gold + cardinal), with a faint scatter
# of paw prints for texture.
#   "C:\Program Files\R\R-4.3.1\bin\Rscript.exe" scripts/make_og_image.R
#----------------------------------------------------------------------
ROOT <- getwd()
out  <- file.path(ROOT, "docs", "og-image.png")
dir.create(dirname(out), showWarnings = FALSE, recursive = TRUE)

navy <- "#0C234B"; navy2 <- "#16386e"; gold <- "#FFD200"; cardinal <- "#AB0520"; sky <- "#2f7fb5"

png(out, width = 1200, height = 630, res = 144)
op <- par(mar = c(0, 0, 0, 0), bg = navy); on.exit({ par(op); dev.off() })
plot.new(); plot.window(xlim = c(0, 1200), ylim = c(0, 630), xaxs = "i", yaxs = "i")

# background: navy with a soft top-left glow
rect(0, 0, 1200, 630, col = navy, border = NA)
for (i in seq(0, 1, length.out = 60)) {
  col <- grDevices::adjustcolor(navy2, alpha.f = 0.014)
  symbols(170, 560, circles = 30 + i * 760, inches = FALSE, add = TRUE, bg = col, fg = NA)
}

# one paw print (a big metacarpal pad + four toe pads), faint
paw <- function(x, y, s, col) {
  symbols(x, y, circles = s, inches = FALSE, add = TRUE, bg = col, fg = NA)        # palm pad
  ang <- c(118, 152, 28, 62) * pi / 180                                           # four toes
  for (a in ang) symbols(x + cos(a) * s * 1.7, y + sin(a) * s * 1.7,
                         circles = s * 0.55, inches = FALSE, add = TRUE, bg = col, fg = NA)
}
set.seed(11)
for (k in 1:9) paw(runif(1, 80, 1130), runif(1, 70, 560), runif(1, 9, 20),
                   grDevices::adjustcolor("white", alpha.f = runif(1, .025, .05)))

# badge
text(70, 556, "NEON · SMALL MAMMAL BOX TRAPPING · DP1.10072.001",
     col = grDevices::adjustcolor(gold, .95), cex = .9, font = 2, adj = 0)

# title
text(68, 472, "NEON Small Mammal", col = "white", cex = 3.5, font = 2, adj = 0)
text(68, 396, "Tracker",            col = "white", cex = 3.5, font = 2, adj = 0)
# a small gold paw accent, clear to the right of the "Tracker" wordmark
paw(470, 398, 16, grDevices::adjustcolor(gold, .92))

# subtitle
text(70, 322, "Tap a site on the national map and explore who lives where — home",
     col = grDevices::adjustcolor("white", .92), cex = 1.12, adj = 0)
text(70, 292, "ranges, diversity, and detection-corrected abundance, on real NEON data.",
     col = grDevices::adjustcolor("white", .92), cex = 1.12, adj = 0)

# stat chips
chips <- list(c("46", "field sites"), c("~178k", "captures"),
              c("145", "species"), c("instant", "no API waits"))
x0 <- 70; gap <- 14; w <- 250; h <- 96; y1 <- 64
chipfill <- grDevices::adjustcolor("white", .10)
for (i in seq_along(chips)) {
  xl <- x0 + (i - 1) * (w + gap)
  rect(xl, y1, xl + w, y1 + h, col = chipfill, border = NA)
  rect(xl, y1, xl + 6, y1 + h, col = gold, border = NA)                 # gold spine
  text(xl + 22, y1 + 62, chips[[i]][1], col = "white", cex = 1.95, font = 2, adj = 0)
  text(xl + 22, y1 + 28, chips[[i]][2], col = grDevices::adjustcolor("white", .85), cex = .96, adj = 0)
}
cat("wrote", out, "\n")
