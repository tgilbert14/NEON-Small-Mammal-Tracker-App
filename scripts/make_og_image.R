# ---------------------------------------------------------------------------
# make_og_image.R — draws docs/og-image-fallback.png (1200 x 630).
#
# The reviewed delivery asset is docs/og-image.png and its code-native layout is
# docs/social-card.html. This script creates a current-copy fallback from the
# same Living Poster art without overwriting the reviewed social card.
# ---------------------------------------------------------------------------

root <- getwd()
source_path <- file.path(root, "docs", "assets", "small-mammal-living-poster.png")
out <- file.path(root, "docs", "og-image-fallback.png")

if (!requireNamespace("png", quietly = TRUE)) {
  stop("Package 'png' is required to render the social-card fallback.")
}
if (!file.exists(source_path)) stop("Missing Living Poster source: ", source_path)

art <- png::readPNG(source_path)
# East-weighted 1.105:1 crop: keeps the mouse and the oversized humane trap.
target_cols <- floor(dim(art)[1] * (696 / 630))
start_col <- dim(art)[2] - target_cols + 1L
art_crop <- art[, start_col:dim(art)[2], , drop = FALSE]

night <- "#111512"
acid <- "#DCE319"
paper <- "#F3E8CB"
paper_2 <- "#DFD2B3"
ember <- "#E87531"

grDevices::png(out, width = 1200, height = 630, res = 96)
op <- par(mar = c(0, 0, 0, 0), bg = night, family = "serif")
plot.new()
plot.window(xlim = c(0, 1200), ylim = c(0, 630), xaxs = "i", yaxs = "i")

rect(0, 0, 1200, 630, col = night, border = NA)
rasterImage(art_crop, 504, 0, 1200, 630, interpolate = TRUE)
for (i in seq_len(42)) {
  alpha <- (1 - (i - 1) / 41)^2 * .82
  x0 <- 504 + (i - 1) * 4
  rect(x0, 0, x0 + 5, 630, col = grDevices::adjustcolor(night, alpha.f = alpha), border = NA)
}

text(70, 506, "NEON SMALL MAMMAL TRACKER", col = acid, cex = 1.02,
     font = 2, adj = 0, family = "sans")
text(68, 370, "Who moves", col = paper, cex = 4.35, font = 2, adj = 0)
text(68, 284, "after dark?", col = acid, cex = 4.35, font = 2, adj = 0)
text(70, 195, "Meet the tiny lives reshaping", col = paper_2, cex = 1.34,
     adj = 0, family = "sans")
text(70, 160, "the landscape.", col = paper_2, cex = 1.34,
     adj = 0, family = "sans")

symbols(940, 48, circles = 8, inches = FALSE, add = TRUE,
        bg = NA, fg = ember, lwd = 4)
text(958, 48, "NEON Explorer Suite · unofficial", col = paper,
     cex = .82, font = 2, adj = 0, family = "sans")

par(op)
grDevices::dev.off()
cat("wrote", out, "\n")
