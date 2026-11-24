source("src/paper-figures-code/setup-common.R")
library(grid)

deaths <- as_tsibble(USAccDeaths) |>
  as_tibble() |>
  transmute(
    date = as.Date(index), year = year(date), month = month(date),
    time = mixtime::yearmonth(date), thousands = value / 1000
  ) |>
  filter(year <= 1976)

# The transitions between years, from December to January
turns <- deaths |>
  filter(month %in% c(1, 12)) |>
  mutate(turn = year + (month == 12)) |>
  filter(n() == 2, .by = turn)

highlight <- "#FB8C58"
wrong <- "#980000"
right <- "#1B7837"

# Draw a grob over a panel (annotation_custom() isn't compatible with time scales)
GeomGrob <- ggproto("GeomGrob", Geom, draw_panel = function(data, panel_params, coord, grob) grob)
annotate_grob <- function(grob) {
  layer(
    stat = "identity", position = "identity", geom = GeomGrob,
    data = data.frame(grob = 1), mapping = aes(), inherit.aes = FALSE,
    show.legend = FALSE, params = list(grob = grob)
  )
}

# A tick or cross in the top left corner, for valid and invalid cycles (nudged
# above panels with notes, and away from the axis labels of radial panels)
mark <- function(valid, nudge = c(0, 0)) {
  x <- function(mm) unit(0.9 + nudge[1] + mm, "mm")
  y <- function(mm) unit(1, "npc") - unit(0.9 - nudge[2] + mm, "mm")
  if (valid) {
    polylineGrob(x(c(0, 0.7, 1.9)), y(c(1, 1.7, 0.1)), gp = gpar(col = right, lwd = 1.5))
  } else {
    polylineGrob(
      x(c(0, 1.6, 0, 1.6)), y(c(0.1, 1.7, 1.7, 0.1)), id = c(1, 1, 2, 2),
      gp = gpar(col = wrong, lwd = 1.5)
    )
  }
}

# An explanation beside the cross, with an arrow to the problem
arrow_note <- function(label, from, to, curvature, nudge = c(0, 0)) {
  grobTree(
    textGrob(
      label, unit(3.3 + nudge[1], "mm"), unit(1, "npc") - unit(0.9 - nudge[2], "mm"), hjust = 0, vjust = 1,
      gp = gpar(col = wrong, fontsize = 6, lineheight = 0.95)
    ),
    curveGrob(
      from[1], from[2], to[1], to[2],
      curvature = curvature, square = FALSE, ncp = 8,
      arrow = arrow(length = unit(1.1, "mm"), type = "closed"),
      gp = gpar(col = wrong, fill = wrong, lwd = 0.9)
    )
  )
}
# Notes are above the panels, at the same height in both coordinate systems
nudges <- list(cartesian = c(-0.9, 3.9), radial = c(-3, 3.9))
notes <- list(
  cartesian = arrow_note(
    "No line from Dec to Jan", c(0.82, 1.05), c(0.965, 0.6), -0.3, nudge = nudges$cartesian
  ),
  radial = arrow_note(
    "Dec and Jan overlap", c(0.66, 1.03), c(0.53, 0.74), -0.2, nudge = nudges$radial
  )
)

initials <- substr(month.abb, 1, 1)
loop_panel <- function(loop, radial) {
  type <- if (radial) "radial" else "cartesian"
  coord <- if (radial) {
    coord_radial(expand = FALSE, inner.radius = 0.1, clip = "off")
  } else {
    coord_cartesian(clip = "off")
  }
  p <- if (loop) {
    ggplot(deaths, aes(time, thousands)) +
      geom_line(linewidth = 0.3) +
      geom_line(aes(group = turn), data = turns, colour = highlight, linewidth = 0.6) +
      scale_x_mixtime(
        time_breaks = mixtime::cal_gregorian$month(1L), minor_breaks = NULL,
        labels = \(x) initials[seq_along(x)]
      ) +
      coord_loop(
        time_loops = mixtime::cal_gregorian$year(1L), coord = coord,
        clip = if (radial) "off" else "on"
      )
  } else {
    # Both ends of the radial axis share a position, and so share a label
    ggplot(deaths, aes(month, thousands, group = year)) +
      geom_line(linewidth = 0.3) +
      scale_x_continuous(
        breaks = 1:12, minor_breaks = NULL,
        labels = initials,
        expand = expansion(if (radial) 0 else 0.04)
      ) +
      coord
  }
  p +
    geom_point(size = 0.4) +
    geom_point(data = turns, colour = highlight, size = 0.8) +
    scale_y_continuous(
      limits = c(6.5, 11.6), breaks = c(8, 10), minor_breaks = NULL, expand = expansion(0)
    ) +
    annotate_grob(mark(loop, if (!loop) nudges[[type]] else c(if (radial) -3 else 0, 0))) +
    (if (!loop) annotate_grob(notes[[type]])) +
    labs(x = NULL, y = NULL) +
    theme(
      axis.text = element_text(size = 5.5),
      axis.text.r = element_blank(),
      axis.ticks = element_blank(),
      axis.line = element_blank(),
      panel.background = if (radial) element_blank() else element_rect(
        # Half of the border is clipped in the looped panel
        fill = "white", colour = "black", linewidth = if (loop) 0.5 else 0.25
      ),
      panel.grid.major = element_line(linewidth = 0.3),
      panel.grid.minor = element_blank(),
      plot.margin = margin(if (loop) 1 else 11, 1, 1, 1)
    )
}

# Headings for each dimension, with the code that sets it
col_label <- function(label, code) {
  wrap_elements(
    panel = grobTree(
      textGrob(label, y = 0.78, gp = gpar(fontsize = 7, fontface = "bold")),
      textGrob(code, y = 0.35, gp = gpar(fontsize = 5.5)),
      segmentsGrob(0, 0.04, 1, 0.04, gp = gpar(col = "grey60", lwd = 1.5))
    ),
    clip = FALSE
  ) +
    theme(plot.margin = margin(0, 1, 0, 1))
}
row_label <- function(label, code) {
  wrap_elements(
    panel = grobTree(
      textGrob(label, x = 0.18, rot = 90, gp = gpar(fontsize = 7, fontface = "bold")),
      textGrob(code, x = 0.62, rot = 90, gp = gpar(fontsize = 5.5)),
      segmentsGrob(0.93, 0, 0.93, 1, gp = gpar(col = "grey60", lwd = 1.5))
    ),
    clip = FALSE
  ) +
    theme(plot.margin = margin(1, 1, 1, 0))
}

# Sized for a single column
p <- wrap_plots(
  A = col_label("Cartesian", "ggplot2::coord_cartesian()"),
  B = col_label("Radial", "ggplot2::coord_radial()"),
  C = row_label("Cyclical time", "ggplot2"),
  D = loop_panel(FALSE, FALSE),
  E = loop_panel(FALSE, TRUE),
  F = row_label("Looped linear time", "ggtime::coord_loop()"),
  G = loop_panel(TRUE, FALSE),
  H = loop_panel(TRUE, TRUE),
  design = "#AB\nCDE\n###\nFGH",
  widths = unit(c(7, 1, 1), c("mm", "null", "null")),
  heights = unit(c(7, 1, 1.5, 1), c("mm", "null", "mm", "null"))
)
ggsave("plots/plot-coord-loop.pdf", p, width = 8.5, height = 8, units = "cm", device = cairo_pdf)
ggsave("plots/plot-coord-loop.png", p, width = 8.5, height = 8, units = "cm", dpi = 400, bg = "white")
