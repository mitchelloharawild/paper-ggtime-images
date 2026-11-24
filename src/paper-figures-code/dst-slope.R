source("src/paper-figures-code/setup-common.R")

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
clock <- function(x, tz) paste0(as.integer(format(x, "%I", tz = tz)), tolower(format(x, "%p", tz = tz)))

# All panels span five hours from midnight, so their slopes are comparable
dst_panel <- function(event, local, time_line = FALSE, note = NULL, title = NULL, ylab = NULL) {
  x <- dst[[event]]
  axis_tz <- if (local) NA else tz
  hours <- if (local) x$midnight + 3600 * 0:5 else with_tz(x$data$posix[1] + 3600 * 0:5, tz)
  limits <- instant(range(hours) + c(-600, 600), axis_tz)

  lines <- if (time_line) {
    list(
      geom_time_line(linewidth = 0.4),
      geom_time_line(
        data = x$data[x$hour, ], colour = highlight, linewidth = 1.2,
        # The transition is at the edge of the highlighted hour
        transitions = mixtime::tz_transitions(
          with_tz(min(x$data$posix), tz) - 86400, with_tz(max(x$data$posix), tz) + 86400
        ),
        transition_aesthetics = list(linetype = 2, colour = "black", linewidth = 0.4)
      )
    )
  } else {
    list(
      geom_line(linewidth = 0.4),
      geom_line(data = x$data[x$hour, ], colour = highlight, linewidth = 1.2)
    )
  }

  ggplot(x$data, aes(time, values)) +
    lines +
    geom_point(size = 1) +
    # Only used in local time
    (if (!is.null(note)) annotate(
      "text", x = instant(x$midnight + 3600 * 4.9, NA), y = 0.04, label = note,
      hjust = 1, vjust = 0, size = 8 / .pt, colour = "#980000"
    )) +
    scale_x_mixtime(
      time_chronon = mixtime::cal_gregorian$second(1L, tz = axis_tz),
      breaks = instant(hours, axis_tz), minor_breaks = NULL,
      labels = clock(hours, if (local) "UTC" else tz),
      limits = limits, expand = expansion(0.02)
    ) +
    scale_y_continuous(limits = c(0, 1), labels = NULL, minor_breaks = NULL) +
    labs(subtitle = title, x = NULL, y = ylab) +
    theme(
      plot.subtitle = element_text(size = 8),
      axis.text.x = element_text(size = 7),
      axis.title.y = element_text(size = 9),
      axis.ticks.x = element_line(colour = "black", linewidth = 0.3),
      axis.ticks.length.x = unit(1.2, "mm"),
      plot.margin = margin(1, 3, 1, 3)
    )
}

# Headings which group the columns by absolute and local time
heading <- function(label) {
  ggplot() +
    labs(title = label) +
    theme_void() +
    theme(
      plot.title = element_text(size = 10, face = "bold", margin = margin(b = 2)),
      panel.border = element_blank(),
      panel.background = element_rect(fill = "grey60", colour = NA),
      plot.margin = margin(1, 3, 1, 3)
    )
}

p <- wrap_plots(
  A = heading("Absolute time"), B = heading("Local time"),
  C = dst_panel("ends", FALSE, title = "geom_line() (ggplot2)", ylab = "DST ends"),
  D = dst_panel("ends", TRUE, title = "geom_line() (ggplot2)", note = "Saw-toothing"),
  E = dst_panel("ends", TRUE, TRUE, title = "geom_time_line() (ggtime)"),
  F = dst_panel("starts", FALSE, ylab = "DST starts"),
  G = dst_panel("starts", TRUE, note = "Half the slope"),
  H = dst_panel("starts", TRUE, TRUE),
  design = "A#BB\nC#DE\nF#GH",
  widths = c(1, 0.06, 1, 1),
  heights = unit(c(0.4, 1, 1), c("mm", "null", "null"))
)
ggsave("plots/plot-dst-slopes.pdf", p, width = 18, height = 8.5, units = "cm", device = cairo_pdf)
ggsave("plots/plot-dst-slopes.png", p, width = 18, height = 8.5, units = "cm", dpi = 300, bg = "white")
