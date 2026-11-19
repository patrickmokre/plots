# AI circular-deals map: who funds whom among 11 AI firms, plus money from the
# "outside world" (investors, banks, bondholders outside the network).
#
# Bubbles: area proportional to market value / private valuation; all bubbles
#          are scaled by one common factor so the closest pair almost touches.
# Arrows:  direction in which money or an obligation flows.
#          Colour = type, thickness = dollar amount (square-root scale, for
#          ordering only; thinnest = no amount disclosed).
#          Outside-world arrows to non-lab firms run around the map.
#          Set show_world (section 6) to TRUE/FALSE to show/hide the outside world.
# Every number carries a [n] key that points to its source in the caption.
# Contingent amounts (milestone tranches, guarantees, warrants, unconfirmed
# reports) are left out. Amounts are headline, often multi-year figures and
# overlap: do not add them up.
#
# Requirements: R >= 4.1, ggplot2 >= 3.4 (linewidth), ggrepel >= 0.9 (point.size)
#   install.packages(c("ggplot2", "ggrepel"))
# Output: ai_circular_deals_map.png in the working directory (13 x 13.5 in, 200 dpi).

library(ggplot2)
library(ggrepel)   # keeps labels clear of each other, the arrows and the bubbles

# ---- 1. Companies + the outside world ---------------------------------------
# The three AI labs sit in a row in the middle. OpenAI's partners sit on the
# right, next to the outside world (upper right), because most outside money
# in this map flows to OpenAI and Oracle. Broadcom sits at the bottom centre.
nodes <- data.frame(
  name  = c("Nvidia", "Broadcom", "OpenAI", "Anthropic",
            "Amazon", "Microsoft", "Oracle", "AMD",
            "Google", "Intel", "SpaceX (xAI)", "Outside world"),
  value = c(5.2, 1.6, 0.852, 0.965,
            2.7, 4.0, 0.382, 0.867,
            3.9, 0.478, 2.0, NA),
  x = c(-4.5,  0.0,  3.8, -3.8,
        6.0, -6.0,  8.6,  8.6,
        -9.4,  2.4,  0.0,  8.2),
  y = c( 6.4, -9.0,  0.0,  0.0,
         -7.4, -6.2,  1.2, -4.0,
         -1.6,  8.2,  0.6,  8.2)
)
nodes$world <- nodes$name == "Outside world"
nodes$r <- ifelse(nodes$world, 2.2, sqrt(nodes$value))
spread <- 1.3                     # stretch the layout to make room for labels
nodes$x <- nodes$x * spread
nodes$y <- nodes$y * spread

# EXPERIMENT: grow all company bubbles by one common factor k, the largest
# that keeps every pair of bubbles from overlapping (leaving `gap` between
# the closest pair). Area stays proportional to valuation.
gap <- 0.15
co  <- which(!nodes$world)
d   <- as.matrix(dist(nodes[co, c("x", "y")]))
rr  <- outer(nodes$r[co], nodes$r[co], "+")
k   <- min(((d - gap) / rr)[upper.tri(d)])
nodes$r[co] <- nodes$r[co] * k
message(sprintf("Bubbles scaled by k = %.2f", k))

# EXPERIMENT: arrows start and end inside the bubbles, at this fraction of the
# radius from the centre (1 = at the edge). Kept away from the centre so the
# company name stays readable.
inset <- 0.6

# ---- 2. Deals ------------------------------------------------------------------
# label_t: where along the arrow the label sits (0 = start, 1 = end)
# nudge_x / nudge_y: manual fine-tuning of a label (data units)
# on_line = TRUE: label sits on the arrow itself
deal <- function(from, to, type, amount_bn, label,
                 curvature = 0.25, label_t = 0.5, nudge_x = 0, nudge_y = 0,
                 on_line = FALSE) {
  data.frame(from, to, type, amount_bn, label, curvature, label_t,
             nudge_x, nudge_y, on_line)
}
DI <- "Direct investment"
CP <- "Obligation to buy computing power"
CH <- "Obligation to buy chips"
LO <- "Loan or bond"
SH <- "Stock holding"
W  <- "Outside world"

ipo_bn <- 1   # Anthropic IPO, 18 Nov 2026 - amount as specified; see [21]

edges <- rbind(
  # inside the network
  deal("Nvidia",    "OpenAI",    DI,  30, "Direct investment\n$30bn [2]"),
  deal("Microsoft", "OpenAI",    DI,  13, "Direct investment\n$13bn, ~23% stake [3, 11]"),
  deal("Amazon",    "OpenAI",    DI,  50, "Direct investment\n$50bn [2]"),
  deal("OpenAI",    "Microsoft", CP, 250, "Azure compute\n$250bn [3]"),
  deal("OpenAI",    "Oracle",    CP, 300, "Cloud compute\n$300bn [4]"),
  deal("OpenAI",    "Amazon",    CP, 138, "AWS compute\n$138bn [2]"),
  deal("OpenAI",    "AMD",       CH,  NA, "GPU purchases\n6 GW [5]"),
  deal("OpenAI",    "Broadcom",  CH,  NA, "Custom chips\n10 GW [6]",
       curvature = -0.2),
  deal("Oracle",    "Nvidia",    CH,  NA, "GPU purchases\n(no amount disclosed)"),
  deal("Amazon",    "Anthropic", DI,  13, "Direct investment\n$13bn [7]"),
  deal("Anthropic", "Amazon",    CP, 100, "AWS compute\n>$100bn [7]"),
  deal("Google",    "Anthropic", DI,  10, "Direct investment\n$10bn [8]"),
  deal("Nvidia",    "Anthropic", DI,  10, "Direct investment\nup to $10bn [9]"),
  deal("Microsoft", "Anthropic", DI,   5, "Direct investment\nup to $5bn [9]"),
  deal("Anthropic", "Microsoft", CP,  30, "Azure compute\n$30bn [9]"),
  deal("Nvidia",    "Intel",     DI,   5, "Direct investment\n$5bn [10]"),
  deal("Nvidia",    "SpaceX (xAI)", DI, NA, "Direct investment\n(no amount disclosed)"),
  # from the outside world: equity
  deal(W, "OpenAI",    DI,  30, "SoftBank investment\n$30bn [2]",        curvature = 0.1),
  deal(W, "OpenAI",    SH,  NA, "SoftBank stake\n~13% [11]",             curvature = 0.3),
  deal(W, "Oracle",    DI,  30, "Share sales\n$30bn [14]",               curvature = -0.2),
  deal(W, "Anthropic", DI,  30, "Funding round\n$30bn [13]",             curvature = 0.12),
  deal(W, "Anthropic", SH, ipo_bn, sprintf("IPO shares\n$%gbn [21]", ipo_bn),
       curvature = 0.22),
  # from the outside world: loans and bonds, one arrow per borrower (2026)
  deal(W, "OpenAI",    LO,   5, "Bank credit lines\n>$5bn [12]",         curvature = -0.15),
  deal(W, "Oracle",    LO,  35, "Bonds\n$35bn [14]",                     curvature = 0.2),
  deal(W, "Amazon",    LO, 172, "Bonds $92bn +\nloan facility $80bn [15]", curvature = -0.45),
  deal(W, "Google",    LO,  42, "Bonds\n>$42bn [15]",                    curvature = 0.08),
  deal(W, "Microsoft", LO,  25, "Bonds\n$25bn [16]",                     curvature = 0.3,
       label_t = 0.9),
  deal(W, "Nvidia",    LO,  25, "Bonds\n$25bn [17]",                     curvature = -0.2),
  deal(W, "SpaceX (xAI)", LO, 25, "Bonds\n$25bn [18]",                   curvature = -0.1,
       label_t = 0.7),
  deal(W, "Anthropic", LO,  50, "Credit facility $15bn +\nchip-lease debt $35bn [19]", curvature = -0.05),
  deal(W, "Intel",     LO, 6.5, "Bonds\n$6.5bn [20]",                    curvature = 0.2),
  deal(W, "AMD",       LO, 4.75, "Bonds\n$4.75bn [20]",                  curvature = 0.12,
       label_t = 0.8)
)
edges$type <- factor(edges$type, levels = c(DI, CP, CH, LO, SH))

# ---- 3. Geometry ----------------------------------------------------------------
pad <- 0.18
i_from <- match(edges$from, nodes$name)
i_to   <- match(edges$to,   nodes$name)
dx <- nodes$x[i_to] - nodes$x[i_from]
dy <- nodes$y[i_to] - nodes$y[i_from]
len <- sqrt(dx^2 + dy^2)
ux <- dx / len; uy <- dy / len
r_from <- ifelse(nodes$world[i_from], nodes$r[i_from] + pad, nodes$r[i_from] * inset)
r_to   <- ifelse(nodes$world[i_to],   nodes$r[i_to]   + pad, nodes$r[i_to]   * inset)
edges$x    <- nodes$x[i_from] + ux * r_from
edges$y    <- nodes$y[i_from] + uy * r_from
edges$xend <- nodes$x[i_to]   - ux * r_to
edges$yend <- nodes$y[i_to]   - uy * r_to

# geom_curve bends to the right of travel for positive curvature; the bulge is
# about curvature * length / 2 at the midpoint, tapering to the ends
# (approximated as a parabola). curve_point() gives a point on an arrow.
seg_len <- sqrt((edges$xend - edges$x)^2 + (edges$yend - edges$y)^2)
nx <- uy; ny <- -ux
curve_point <- function(i, t) {
  b <- edges$curvature[i] * seg_len[i] / 2 * 4 * t * (1 - t)
  c(x = edges$x[i] + t * (edges$xend[i] - edges$x[i]) + nx[i] * b,
    y = edges$y[i] + t * (edges$yend[i] - edges$y[i]) + ny[i] * b)
}

# Each label is anchored on its own arrow; ggrepel then moves it just far
# enough to clear other labels, arrows and bubbles.
anchors <- t(sapply(seq_len(nrow(edges)),
                    function(i) curve_point(i, edges$label_t[i])))
edges$lab_x <- anchors[, "x"] + edges$nudge_x
edges$lab_y <- anchors[, "y"] + edges$nudge_y

edges$width_bn <- ifelse(is.na(edges$amount_bn), 1, edges$amount_bn)

# ---- 3b. Route outside-world arrows to non-lab firms AROUND the map --------
# These arrows leave the outside world outwards, run along a ring outside all
# bubbles, and come in to the target bubble from its outer side. Arrows to the
# three AI labs stay as direct curves through the middle.
labs <- c("OpenAI", "Anthropic", "SpaceX (xAI)")
edges$routed <- edges$from == W & !(edges$to %in% labs)
cw_targets <- c("Oracle", "AMD", "Amazon", "Microsoft")  # go round clockwise

co <- !nodes$world
cx <- mean(range(nodes$x[co])); cy <- mean(range(nodes$y[co]))
rho   <- sqrt((nodes$x - cx)^2 + (nodes$y - cy)^2)
theta <- atan2(nodes$y - cy, nodes$x - cx)
# The "ring" hugs the outline of the map: in each direction it sits just
# beyond the furthest bubble edge (the support function of the bubbles).
ring_margin <- 1.0                             # gap between map and inner arrow
ring_gap    <- 0.8                             # spacing between parallel arrows
hull_r0 <- function(th) sapply(th, function(a)
  max((nodes$x[co] - cx) * cos(a) + (nodes$y[co] - cy) * sin(a) + nodes$r[co]))
# smooth out the kinks where the outermost bubble changes (averaging only
# raises the outline at a kink, so the arrows stay clear of the bubbles)
hull_r <- function(th) rowMeans(sapply(seq(-0.3, 0.3, length.out = 13),
                                       function(o) hull_r0(th + o)))
iw <- which(nodes$world)

routed_idx <- which(edges$routed)
it <- match(edges$to[routed_idx], nodes$name)
dth <- theta[it] - theta[iw]
cw  <- edges$to[routed_idx] %in% cw_targets
dth <- ifelse(cw,  -((-dth) %% (2 * pi)), dth %% (2 * pi))
# shorter trips get inner rings so the arrows nest without crossing
ring_k <- integer(length(routed_idx))
for (side in c(TRUE, FALSE)) {
  j <- which(cw == side)
  ring_k[j] <- rank(abs(dth[j]), ties.method = "first") - 1
}

smooth_out <- function(u) pmax(0, 1 - u)^2
paths <- do.call(rbind, lapply(seq_along(routed_idx), function(m) {
  i  <- routed_idx[m]; tg <- it[m]
  ts <- seq(0, 1, length.out = 240)
  th <- theta[iw] + dth[m] * ts
  R  <- hull_r(th) + ring_margin + ring_k[m] * ring_gap
  r_start <- rho[iw] + nodes$r[iw]          # outer edge of the outside world
  r_end   <- rho[tg] + nodes$r[tg] * inset
  ramp <- min(0.3, 0.6 / abs(dth[m]))           # share of the trip spent turning
  rr <- R + (r_start - R) * smooth_out(ts / ramp) +
    (r_end   - R) * smooth_out((1 - ts) / ramp)
  data.frame(id = i, t = ts, x = cx + rr * cos(th), y = cy + rr * sin(th),
             type = edges$type[i], width_bn = edges$width_bn[i])
}))

# labels of routed arrows sit on their path
for (i in routed_idx) {
  pp <- paths[paths$id == i, ]
  k  <- which.min(abs(pp$t - edges$label_t[i]))
  edges$lab_x[i] <- pp$x[k] + edges$nudge_x[i]
  edges$lab_y[i] <- pp$y[k] + edges$nudge_y[i]
}

# Obstacles: points along every arrow and inside every company bubble.
# (ggrepel avoids rows with an empty label without drawing anything.)
arrow_pts <- do.call(rbind, lapply(which(!edges$routed), function(i) {
  ts <- seq(0.04, 0.96, by = 0.04)
  ts <- ts[abs(ts - edges$label_t[i]) > 0.06]   # leave room at the label
  pts <- t(sapply(ts, function(tt) curve_point(i, tt)))
  data.frame(x = pts[, "x"], y = pts[, "y"], size = 1.5)
}))
bubble_pts <- do.call(rbind, lapply(which(!nodes$world), function(i) {
  g <- expand.grid(a = seq(-1, 1, by = 0.25), b = seq(-1, 1, by = 0.25))
  g <- g[g$a^2 + g$b^2 <= 1, ]
  g$a <- g$a * 0.45; g$b <- g$b * 0.3      # protect only the centre text
  data.frame(x = nodes$x[i] + g$a * nodes$r[i],
             y = nodes$y[i] + g$b * nodes$r[i], size = 4)
}))
path_pts <- do.call(rbind, lapply(routed_idx, function(i) {
  pp <- paths[paths$id == i & abs(paths$t - edges$label_t[i]) > 0.04, ]
  pp <- pp[seq(1, nrow(pp), by = 6), ]
  data.frame(x = pp$x, y = pp$y, size = 1.5)
}))
obstacles <- rbind(arrow_pts, path_pts, bubble_pts)
obstacles$label <- ""

# ---- 4. Circles -------------------------------------------------------------------
theta <- seq(0, 2 * pi, length.out = 150)
circles <- do.call(rbind, lapply(seq_len(nrow(nodes)), function(i) {
  data.frame(name = nodes$name[i], world = nodes$world[i],
             x = nodes$x[i] + nodes$r[i] * cos(theta),
             y = nodes$y[i] + nodes$r[i] * sin(theta))
}))
nodes$label <- ifelse(
  nodes$world,
  "Outside world\ninvestors, banks,\nbondholders",
  sprintf("%s\n$%s [1]", nodes$name,
          ifelse(!is.na(nodes$value) & nodes$value >= 1,
                 sprintf("%.1ftn", nodes$value),
                 sprintf("%.0fbn", nodes$value * 1000))))

# ---- 5. Sources --------------------------------------------------------------------
sources <- c(
  "[1] Bloomberg, map of circular AI deals; market values / valuations as of 26 Oct 2026, rounded.",
  "[2] OpenAI-Amazon announcement, 27 Feb 2026 ($110bn round: Amazon $50bn, Nvidia $30bn, SoftBank $30bn; AWS deal +$100bn on the $38bn of Nov 2025), via GeekWire.",
  "[3] Microsoft-OpenAI agreement, 28 Oct 2025 (~$13bn invested, $250bn Azure commitment; stake ~27% then, ~23% after the Feb 2026 round [11]).",
  "[4] OpenAI-Oracle cloud contract, reported Sep 2025 ($300bn over ~5 years).",
  "[5] AMD press release, 6 Oct 2025.   [6] OpenAI-Broadcom announcement, Oct 2025.",
  "[7] Amazon announcement, 20 Apr 2026 ($5bn new + $8bn earlier; Anthropic commits >$100bn to AWS over 10 years).",
  "[8] Alphabet, 24 Apr 2026 ($10bn cash investment).",
  "[9] Microsoft-Nvidia-Anthropic announcement, 18 Nov 2025.   [10] Nvidia-Intel announcement, Sep 2025 ($5bn for a stake of about 4%).",
  "[11] Ownership estimates by M.G. Siegler, Spyglass, Mar 2026 (updated Jun 2026); later rounds not included.",
  "[12] $4.7bn bank revolver + $520m Bank of America line, reported Jul 2026 (Bloomberg via Pulse 2.0).",
  "[13] Anthropic funding round, Feb 2026 (TechRepublic, citing Bloomberg); may include investors shown on the map.",
  "[14] Oracle bonds: $25bn, Feb 2026 (Bloomberg) + $10bn, 2 Nov 2026 (SEC term sheet, via FourWeekMBA). Shares: $20bn Jun-Aug 2026 (The Next Web) + $10bn Nov 2026.",
  "[15] Amazon: $92bn of bonds in 2026 (Fortune, 8 Jul 2026, citing Bank of America); $80bn loan facility (DCD, 3 Nov 2026).",
  "       Alphabet: ~$32bn by May (Quartz) + EUR 6bn and ~$3.8bn in Nov 2026 (Fitch via TNGlobal; Analysis.org); deals in between may be missing.",
  "[16] Microsoft, $25bn bonds priced 29 Oct 2026 (SEC prospectus, via NewsBytes).   [17] Nvidia, $25bn bonds, 15 Jun 2026 (Reuters).",
  "[18] SpaceX, $25bn bonds, Jun 2026 (Bloomberg); refinances the $20bn bridge loan that repaid X and xAI debt, so the loan is not counted separately.",
  "[19] Anthropic: $15bn bank credit facility, 2026 (Dealroom); $35bn chip-lease debt raised by an SPV (Apollo, Blackstone), Jun 2026.",
  "[20] Intel, $6.5bn bonds, Apr 2026 (Bloomberg). AMD, $4.75bn bonds, Aug 2026. Broadcom: no new bonds found in 2026 (it bought back ~$2.9bn).",
  "[21] Anthropic IPO, priced 18 Nov 2026, trading on Nasdaq as ANT from 19 Nov (TMGM). Arrow amount as set in the script; the proposed offering was $61bn (Renaissance Capital, 10 Nov 2026)."
)
caption <- paste0(
  paste(sources, collapse = "\n"),
  "\nContingent amounts (milestone tranches, guarantees, warrants, unconfirmed reports) are excluded, as is project debt raised by data-centre",
  "\ndevelopers for sites leased to these firms. Amounts are headline, often multi-year figures and overlap; do not add them up."
)

# ---- 6. Plot ----------------------------------------------------------------------
type_cols <- setNames(c("#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#e87ba4"),
                      levels(edges$type))

# Show or hide the outside world. When hidden, its label, its arrows and their
# labels are drawn with colour NA: they stay in the data and keep their place
# in the layout, but are invisible. Set to TRUE to bring them back.
show_world <- FALSE
edges$from_world <- edges$from == W
hide <- function(world_rows) !show_world & world_rows

curve_layers <- lapply(split(edges[!edges$routed, ], edges$curvature[!edges$routed]), function(e) {
  vis <- e[!hide(e$from_world), ]
  inv <- e[hide(e$from_world), ]
  list(
    if (nrow(vis)) geom_curve(data = vis,
                              aes(x = x, y = y, xend = xend, yend = yend,
                                  colour = type, linewidth = width_bn),
                              curvature = e$curvature[1], angle = 90, ncp = 10,
                              arrow = arrow(length = unit(2.2, "mm"), type = "closed"),
                              lineend = "round", alpha = 0.9),
    if (nrow(inv)) geom_curve(data = inv,
                              aes(x = x, y = y, xend = xend, yend = yend, linewidth = width_bn),
                              colour = NA, na.rm = TRUE,
                              curvature = e$curvature[1], angle = 90, ncp = 10,
                              arrow = arrow(length = unit(2.2, "mm"), type = "closed"))
  )
})

# label colours, row by row: hidden labels get NA everywhere
lab_df <- rbind(data.frame(x = edges$lab_x, y = edges$lab_y,
                           label = edges$label, size = 0,
                           hidden = hide(edges$from_world)),
                transform(obstacles, hidden = FALSE))

p <- ggplot() +
  # company bubbles; the outside world is a circle with no fill and no border
  geom_polygon(data = subset(circles, !world), aes(x, y, group = name),
               fill = "#efeeea", colour = "white", linewidth = 1) +
  geom_polygon(data = subset(circles, world), aes(x, y, group = name),
               fill = NA, colour = NA) +
  curve_layers +
  (if (show_world)
    geom_path(data = paths, aes(x, y, group = id, colour = type, linewidth = width_bn),
              arrow = arrow(length = unit(2.2, "mm"), type = "closed"),
              lineend = "round", linejoin = "round", alpha = 0.9)
   else
     geom_path(data = paths, aes(x, y, group = id, linewidth = width_bn),
               colour = NA, na.rm = TRUE,   # silences the "removed rows" warning
               arrow = arrow(length = unit(2.2, "mm"), type = "closed"))) +
  geom_label_repel(
    data = lab_df,
    aes(x, y, label = label, point.size = size),
    size = 2.4, lineheight = 0.9,
    colour = ifelse(lab_df$hidden, NA, "grey15"),
    fill = ifelse(lab_df$hidden, NA, alpha("white", 0.9)), label.size = 0,
    label.padding = unit(0.12, "lines"), box.padding = unit(0.08, "lines"),
    point.padding = 0, min.segment.length = unit(0.25, "lines"),
    segment.colour = ifelse(lab_df$hidden, NA, "grey45"), segment.size = 0.3,
    force = 1.5, force_pull = 2, max.overlaps = Inf, max.iter = 50000,
    max.time = 10, seed = 7) +
  geom_label(data = subset(nodes, !world), aes(x, y, label = label),
             fontface = "bold", size = 3.2, lineheight = 0.9, colour = "grey10",
             fill = alpha("#efeeea", 0.85), label.size = 0) +
  geom_text(data = subset(nodes, world), aes(x, y, label = label),
            fontface = "italic", size = 3.4, lineheight = 0.95, colour = if (show_world) "grey30" else NA) +
  scale_colour_manual(values = type_cols, name = NULL) +
  scale_linewidth(range = c(0.35, 2.6), trans = "sqrt", guide = "none") +
  coord_fixed(clip = "off") +
  guides(colour = guide_legend(ncol=3, override.aes = list(linewidth = 1.2))) +
  labs(
    title = "",
    subtitle = paste0(" ",
                      "\n",
                      " "),
    caption = caption
  ) +
  theme_void(base_size = 12) +
  theme(legend.position = "top",
        legend.text = element_text(size = 9),
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(colour = "grey30", margin = margin(b = 6)),
        plot.caption = element_text(colour = "grey35", hjust = 0, size = 6.8,
                                    lineheight = 1.1, margin = margin(t = 12)),
        plot.background = element_rect(fill = "white", colour = NA),
        plot.margin = margin(15, 20, 15, 20))

ggsave("20261119-ai-circular.png", p, width = 13, height = 13.5, dpi = 200)
rstudioapi::viewer(file.path("20261119-ai-circular.png"))
