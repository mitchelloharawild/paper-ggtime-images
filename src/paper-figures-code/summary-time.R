source("src/paper-figures-code/setup-common.R")
library(grid)

# Requires stat_summary_time(), from the development version of ggtime

# Quarterly electricity production in Australia, which ends in June 2010
electricity <- as_tibble(aus_production) |>
  filter(year(Quarter) >= 1990) |>
  mutate(
    quarter = mixtime::yearquarter(as.Date(Quarter)),
    year = mixtime::year(quarter),
    twh = Electricity / 1000
  )

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

# A tick or cross in the top left corner, for valid and invalid summaries
mark <- function(valid) {
  x <- function(mm) unit(0.9 + mm, "mm")
  y <- function(mm) unit(1, "npc") - unit(0.9 + mm, "mm")
  if (valid) {
    polylineGrob(x(c(0, 0.7, 1.9)), y(c(1, 1.7, 0.1)), gp = gpar(col = right, lwd = 1.5))
  } else {
    polylineGrob(
      x(c(0, 1.6, 0, 1.6)), y(c(0.1, 1.7, 1.7, 0.1)), id = c(1, 1, 2, 2),
      gp = gpar(col = wrong, lwd = 1.5)
    )
  }
}

# Positions within a panel, from years and values
limits_x <- c(1990, 2011)
limits_y <- c(95, 280)
npc_x <- function(year) unit((year - limits_x[1]) / diff(limits_x) * 0.96 + 0.02, "npc")
npc_y <- function(value) unit((value - limits_y[1]) / diff(limits_y), "npc")

# An explanation beside the cross, with an arrow to the incomplete year
note <- grobTree(
  textGrob(
    "Only two quarters of 2010", unit(3.3, "mm"), unit(1, "npc") - unit(0.9, "mm"),
    hjust = 0, vjust = 1, gp = gpar(col = wrong, fontsize = 6)
  ),
  # Passes above the series, to point at the fall
  xsplineGrob(
    npc_x(c(1999, 2009, 2010.4, 2010.15)), npc_y(c(265, 267, 255, 185)),
    shape = c(0, 1, 1, 0),
    arrow = arrow(length = unit(1.1, "mm"), type = "closed"),
    gp = gpar(col = wrong, fill = wrong, lwd = 0.9)
  )
)

# Both panels share limits, so space for the incomplete year is retained
summary_panel <- function(layers, valid, label) {
  ggplot(electricity, aes(y = twh)) +
    layers +
    annotate_grob(mark(valid)) +
    (if (!valid) annotate_grob(note)) +
    scale_x_mixtime(
      limits = mixtime::yearquarter(as.Date(c("1990-01-01", "2010-10-01"))),
      breaks = mixtime::yearquarter(as.Date(paste0(seq(1990, 2010, 5), "-01-01"))),
      labels = seq(1990, 2010, 5), minor_breaks = NULL, expand = expansion(0.02)
    ) +
    scale_y_continuous(breaks = c(100, 150, 200, 250), minor_breaks = NULL) +
    # Zoom, since scale limits would remove the quarterly values being summarized
    coord_cartesian(ylim = limits_y, expand = c(top = FALSE, bottom = FALSE)) +
    labs(subtitle = label, x = NULL, y = NULL) +
    theme(
      plot.subtitle = element_text(size = 6),
      axis.text.y = element_text(size = 5.5),
      axis.text.x = if (valid) element_text(size = 5.5) else element_blank(),
      axis.ticks.x = if (valid) element_line(colour = "black", linewidth = 0.3) else element_blank(),
      axis.ticks.length.x = unit(if (valid) 0.8 else 0, "mm"),
      panel.grid.major = element_line(linewidth = 0.3),
      plot.margin = margin(1, 1, 1, 1)
    )
}

usual <- summary_panel(
  list(
    stat_summary(aes(x = year), fun = sum, geom = "line", linewidth = 0.3),
    # The apparent fall into the incomplete year
    stat_summary(
      aes(x = year), data = \(x) filter(x, year(Quarter) >= 2009),
      fun = sum, geom = "line", colour = highlight, linewidth = 0.8
    ),
    stat_summary(aes(x = year), fun = sum, geom = "point", size = 0.6)
  ),
  valid = FALSE,
  label = "ggplot2::stat_summary()"
)
time_aware <- summary_panel(
  list(
    stat_summary_time(aes(x = quarter), granule = year(1L), fun = sum, linewidth = 0.3),
    stat_summary_time(aes(x = quarter), granule = year(1L), fun = sum, geom = "point", size = 0.6)
  ),
  valid = TRUE,
  label = "ggtime::stat_summary_time()"
)

y_label <- wrap_elements(
  panel = textGrob("Electricity production (TWh)", x = 0.3, rot = 90, gp = gpar(fontsize = 6)),
  clip = FALSE
) + theme(plot.margin = margin(1, 1, 1, 1))

p <- wrap_plots(
  A = y_label, B = usual, C = time_aware,
  design = "AB\nAC", widths = unit(c(3.5, 1), c("mm", "null"))
)
ggsave("plots/plot-summary-time.pdf", p, width = 8.5, height = 6.5, units = "cm", device = cairo_pdf)
ggsave("plots/plot-summary-time.png", p, width = 8.5, height = 6.5, units = "cm", dpi = 400, bg = "white")
