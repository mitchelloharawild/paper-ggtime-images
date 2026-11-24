source("src/paper-figures-code/setup-common.R")
library(grid)

tz <- "Australia/Melbourne"

# Observations at exact instants, so the transitions are located precisely
instant <- function(x, tz) {
  mixtime::mixtime(x, chronon = mixtime::cal_gregorian$second(1L, tz = tz))
}

# Hourly observations around the 2025 daylight saving transitions in Melbourne
# (specified in UTC, since local times are ambiguous)
dst <- list(
  ends = list(
    data = tibble(
      posix = as.POSIXct("2025-04-05 16:00", tz = "UTC") + 3600 * -3:2,
      values = c(0, 0.274, 0.393, 0.666, 0.752, 1)
    ),
    midnight = as.POSIXct("2025-04-06", tz = "UTC"),
    hour = 3:4
  ),
  starts = list(
    data = tibble(
      posix = as.POSIXct("2025-10-04 16:00", tz = "UTC") + 3600 * -2:2,
      values = c(0, 0.18, 0.606, 0.689, 1)
    ),
    midnight = as.POSIXct("2025-10-05", tz = "UTC"),
    hour = 2:3
  )
)
dst <- lapply(dst, function(x) {
  x$data$time <- instant(with_tz(x$data$posix, tz), tz)
  x
})

highlight <- "#FB8C58"
wrong <- "#980000"
right <- "#1B7837"
clock <- function(x, tz) paste0(as.integer(format(x, "%I", tz = tz)), tolower(format(x, "%p", tz = tz)))

# Draw a grob over a panel (annotation_custom() isn't compatible with time scales)
GeomGrob <- ggproto("GeomGrob", Geom, draw_panel = function(data, panel_params, coord, grob) grob)
annotate_grob <- function(grob) {
  layer(
    stat = "identity", position = "identity", geom = GeomGrob,
    data = data.frame(grob = 1), mapping = aes(), inherit.aes = FALSE,
    show.legend = FALSE, params = list(grob = grob)
  )
}

# Positions within a panel, from hours since midnight and the values
npc_x <- function(hour) unit((hour * 3600 + 600 + 0.02 * 19200) / (1.04 * 19200), "npc")
npc_y <- function(value) unit((value + 0.05) / 1.1, "npc")

# A tick or cross in the top left corner, for valid and invalid slopes
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

# An explanation beside the cross, with an arrow to the incorrect slope
arrow_note <- function(label, from, to, curvature) {
  grobTree(
    textGrob(
      label, unit(3.3, "mm"), unit(1, "npc") - unit(0.9, "mm"), hjust = 0, vjust = 1,
      gp = gpar(col = wrong, fontsize = 6)
    ),
    curveGrob(
      npc_x(from[1]), npc_y(from[2]), npc_x(to[1]), npc_y(to[2]),
      curvature = curvature, square = FALSE, ncp = 8,
      arrow = arrow(length = unit(1.1, "mm"), type = "closed"),
      gp = gpar(col = wrong, fill = wrong, lwd = 0.9)
    )
  )
}
notes <- list(
  ends = arrow_note("Saw-toothing", c(1.2, 0.87), c(1.9, 0.54), 0.25),
  starts = arrow_note("Half slope", c(1.2, 0.87), c(1.93, 0.45), 0.25)
)

# All panels span five hours from midnight, so their slopes are comparable
dst_panel <- function(event, local, time_line = FALSE, title = NULL, label = NULL) {
  x <- dst[[event]]
  valid <- !local || time_line
  show_x <- valid
  axis_tz <- if (local) NA else tz
  hours <- if (local) x$midnight + 3600 * 0:5 else with_tz(x$data$posix[1] + 3600 * 0:5, tz)
  limits <- instant(range(hours) + c(-600, 600), axis_tz)

  lines <- if (time_line) {
    list(
      geom_time_line(linewidth = 0.3),
      geom_time_line(
        data = x$data[x$hour, ], colour = highlight, linewidth = 0.8,
        # The transition is at the edge of the highlighted hour
        transitions = mixtime::tz_transitions(
          with_tz(min(x$data$posix), tz) - 86400, with_tz(max(x$data$posix), tz) + 86400
        ),
        transition_aesthetics = list(linetype = 2, colour = "black", linewidth = 0.3)
      )
    )
  } else {
    list(
      geom_line(linewidth = 0.3),
      geom_line(data = x$data[x$hour, ], colour = highlight, linewidth = 0.8)
    )
  }

  ggplot(x$data, aes(time, values)) +
    lines +
    geom_point(size = 0.6) +
    annotate_grob(mark(valid)) +
    (if (!valid) annotate_grob(notes[[event]])) +
    scale_x_mixtime(
      time_chronon = mixtime::cal_gregorian$second(1L, tz = axis_tz),
      breaks = instant(hours, axis_tz), minor_breaks = NULL,
      labels = clock(hours, if (local) "UTC" else tz),
      limits = limits, expand = expansion(0.02)
    ) +
    scale_y_continuous(limits = c(0, 1), labels = NULL, minor_breaks = NULL) +
    labs(subtitle = title, x = NULL, y = label) +
    theme(
      axis.title.y = element_text(size = 5.5, lineheight = 1),
      plot.subtitle = element_text(size = 7),
      axis.text.x = if (show_x) element_text(size = 5.5) else element_blank(),
      axis.ticks.x = if (show_x) element_line(colour = "black", linewidth = 0.3) else element_blank(),
      axis.ticks.length.x = unit(if (show_x) 0.8 else 0, "mm"),
      panel.grid.major = element_line(linewidth = 0.3),
      plot.margin = margin(1, 1, 1, 1)
    )
}

# Vertical labels which group the rows by absolute and local time
group_label <- function(label) {
  wrap_elements(
    panel = grobTree(
      textGrob(label, x = 0.35, rot = 90, gp = gpar(fontsize = 7, fontface = "bold")),
      segmentsGrob(0.9, 0, 0.9, 1, gp = gpar(col = "grey60", lwd = 1.5))
    ),
    clip = FALSE
  ) +
    theme(plot.margin = margin(1, 1, 1, 1))
}

# Sized for a single column. Both geometries give the same lines in absolute time.
# The local time rows share an axis and are close together to guide comparisons
# between them, and the columns are further apart
p <- wrap_plots(
  A = group_label("Absolute time"),
  B = dst_panel("ends", FALSE, title = "DST ends", label = "ggplot2::geom_line() and\nggtime::geom_time_line()"),
  C = dst_panel("starts", FALSE, title = "DST starts"),
  D = group_label("Local time"),
  E = dst_panel("ends", TRUE, label = "ggplot2::geom_line()"),
  F = dst_panel("starts", TRUE),
  G = dst_panel("ends", TRUE, TRUE, label = "ggtime::geom_time_line()"),
  H = dst_panel("starts", TRUE, TRUE),
  design = "AB#C\n####\nDE#F\nDG#H",
  widths = unit(c(3.5, 1, 2.5, 1), c("mm", "null", "mm", "null")),
  heights = unit(c(1, 1.5, 1, 1), c("null", "mm", "null", "null"))
)
ggsave("plots/plot-dst-slopes.pdf", p, width = 8.5, height = 9, units = "cm", device = cairo_pdf)
ggsave("plots/plot-dst-slopes.png", p, width = 8.5, height = 9, units = "cm", dpi = 400, bg = "white")
