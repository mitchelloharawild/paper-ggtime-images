source("src/paper-figures-code/setup-common.R")
library(grid)

# Requires stat_summary_time() from ggtime

# Quarterly gas production in Australia, as available after the first quarter of
# 2010. The first quarter is always low, so its mean isn't comparable with a year
production <- as_tibble(aus_production) |>
  filter(Quarter >= yearquarter("2000 Q1"), Quarter <= yearquarter("2010 Q1")) |>
  transmute(quarter = mixtime::yearquarter(as.Date(Quarter)), gas = Gas)
year <- mixtime::year

highlight <- "#FB8C58"
wrong <- "#980000"
right <- "#1B7837"

# The displayed code is also the code used to produce each panel
code_shared <- 'p <- ggplot(production, aes(quarter, gas)) + geom_line()'
code_usual <- 'p + stat_summary(
  aes(year(quarter)),
  fun = mean,
  geom = "line"
)'
code_time <- 'p + stat_summary_time(
  granule = year(1L),
  fun = mean
)'
eval(parse(text = code_shared))

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
limits_y <- c(158, 290)
npc_x <- function(year) unit((year - 2000) / 11 * 0.96 + 0.02, "npc")
npc_y <- function(value) unit((value - limits_y[1]) / diff(limits_y), "npc")

# An explanation beside the cross, with an arrow to the incomplete summary
note <- grobTree(
  textGrob(
    "Incomplete summary of 1 quarter", unit(3.3, "mm"), unit(1, "npc") - unit(0.9, "mm"),
    hjust = 0, vjust = 1, gp = gpar(col = wrong, fontsize = 6)
  ),
  curveGrob(
    npc_x(2008.5), npc_y(279), npc_x(2010.3), npc_y(214),
    curvature = -0.45, square = FALSE, ncp = 8,
    arrow = arrow(length = unit(1.1, "mm"), type = "closed"),
    gp = gpar(col = wrong, fill = wrong, lwd = 0.9)
  )
)

summary_panel <- function(code, valid) {
  g <- eval(parse(text = code), globalenv())
  g$layers[[1]]$aes_params <- list(colour = "grey70", linewidth = 0.3)
  g$layers[[2]]$aes_params <- list(linewidth = 0.6)
  if (!valid) {
    # The apparent fall into the incomplete year
    g <- g +
      stat_summary(
        aes(year(quarter)),
        data = \(x) x[x$quarter >= mixtime::yearquarter(as.Date("2009-01-01")), ],
        fun = mean, geom = "line", colour = highlight, linewidth = 0.8
      ) +
      annotate_grob(note)
  }
  quarters <- function(x) mixtime::yearquarter(as.Date(x))
  g +
    annotate_grob(mark(valid)) +
    scale_x_mixtime(
      limits = quarters(c("2000-01-01", "2010-10-01")),
      breaks = quarters(c("2000-01-01", "2005-01-01", "2010-01-01")),
      labels = c(2000, 2005, 2010), minor_breaks = NULL, expand = expansion(0.02)
    ) +
    scale_y_continuous(breaks = c(200, 250), minor_breaks = NULL) +
    # Zoom, since scale limits would remove values being summarized
    coord_cartesian(ylim = limits_y, expand = c(top = FALSE, bottom = FALSE)) +
    labs(x = NULL, y = NULL) +
    theme(
      axis.text.y = element_text(size = 5.5),
      axis.text.x.bottom = if (valid) element_text(size = 5.5, margin = margin(t = 1)) else element_blank(),
      axis.ticks.x = if (valid) element_line(colour = "black", linewidth = 0.3) else element_blank(),
      axis.ticks.length.x = unit(if (valid) 0.8 else 0, "mm"),
      panel.grid.major = element_line(linewidth = 0.3),
      plot.background = element_blank(),
      plot.margin = margin(1, 2, 0.5, 0)
    )
}

code_label <- function(code) {
  wrap_elements(
    full = textGrob(
      code, unit(1.2, "mm"), hjust = 0,
      gp = gpar(fontfamily = "DejaVu Sans Mono", fontsize = 5.2, lineheight = 1.2, col = "grey20")
    ),
    clip = FALSE
  ) + theme(plot.margin = margin(0, 0, 0, 0))
}

# Bands which join the code to its graphic
band <- function() {
  wrap_elements(full = rectGrob(gp = gpar(fill = "grey95", col = NA))) +
    theme(plot.margin = margin(0, 0, 0, 0))
}

# Sized for a single column
p_all <- wrap_plots(
  code_label(code_shared),
  band(), band(),
  code_label(code_usual), summary_panel(code_usual, FALSE),
  code_label(code_time), summary_panel(code_time, TRUE),
  design = c(
    area(1, 1, 1, 2), area(2, 1, 2, 2), area(4, 1, 4, 2),
    area(2, 1), area(2, 2), area(4, 1), area(4, 2)
  ),
  widths = unit(c(26.2, 1), c("mm", "null")),
  heights = unit(c(4, 1, 1, 1), c("mm", "null", "mm", "null"))
)
ggsave("plots/plot-summary-time.pdf", p_all, width = 8.5, height = 5.1, units = "cm", device = cairo_pdf)
ggsave("plots/plot-summary-time.png", p_all, width = 8.5, height = 5.1, units = "cm", dpi = 400, bg = "white")
