source("src/paper-figures-code/setup-common.R")
library(dplyr)

# Weeks around the 1989 Australian pilots' dispute
ansett_strike <- tsibbledata::ansett |>
  as_tibble() |>
  filter(
    Airports == "MEL-SYD", Class == "Economy",
    Week >= tsibble::yearweek("1989 W30"), Week <= tsibble::yearweek("1989 W44")
  ) |>
  mutate(
    # The first day (Monday) of each week
    date = as.Date(Week),
    # The same weeks as discrete time spans
    week = mixtime::yearweek(date),
    thousands = Passengers / 1000
  )

# Weeks without any passengers
strike <- ansett_strike |>
  filter(Passengers == 0) |>
  summarise(
    date_start = min(date), date_end = max(date),
    week_start = min(week), week_end = max(week)
  )

# Both panels show the same days, with gridlines at the start of each week
day <- function(x) {
  mixtime::mixtime(as.numeric(x), chronon = mixtime::cal_gregorian$day(1L), discrete = FALSE)
}
limits <- as.Date(c("1989-07-21", "1989-11-09"))
weeks <- seq(as.Date("1989-07-24"), as.Date("1989-11-06"), by = "week")
labelled <- weeks[c(1, 5, 9, 13)]

ansett_common <- function(title, subtitle) {
  list(
    geom_point(size = 1.5),
    scale_y_continuous(limits = c(0, 21), minor_breaks = NULL),
    labs(title = title, subtitle = subtitle, x = NULL, y = "Passengers (thousands)"),
    theme(
      plot.title = element_text(size = 10, face = "bold"),
      plot.subtitle = element_text(size = 8, lineheight = 1.1),
      axis.ticks.x = element_line(colour = "black", linewidth = 0.3),
      axis.ticks.length.x = unit(1.2, "mm"),
      axis.title.y = element_text(size = 9),
      panel.grid.major.x = element_line(linewidth = rel(0.5))
    )
  )
}
shade <- "#FB8C58"

p_date <- ggplot(ansett_strike, aes(date, thousands)) +
  geom_rect(
    aes(xmin = date_start, xmax = date_end, ymin = -Inf, ymax = Inf),
    data = strike, inherit.aes = FALSE, fill = shade, alpha = 0.4
  ) +
  geom_line() +
  ansett_common(
    "Weeks as dates (ggplot2)",
    "• Points at the start of each week\n• Shaded region omits the last week"
  ) +
  scale_x_date(
    breaks = labelled, minor_breaks = weeks, date_labels = "%b %d",
    limits = limits, expand = expansion(0)
  )

p_week <- ggplot(ansett_strike, aes(week, thousands)) +
  geom_rect(
    aes(xmin = week_start, xmax = week_end, ymin = -Inf, ymax = Inf),
    data = strike, inherit.aes = FALSE, fill = shade, alpha = 0.4
  ) +
  geom_line() +
  ansett_common(
    "Weeks as time spans (ggtime)",
    "• Points at the middle of each week\n• Shaded region covers all weeks"
  ) +
  # A common chronon of days, to match the dates
  scale_x_mixtime(
    time_chronon = mixtime::cal_gregorian$day(1L),
    # Breaks and limits at the start of days
    breaks = day(labelled), minor_breaks = day(weeks),
    labels = format(labelled, "%b %d"),
    limits = day(limits), expand = expansion(0)
  )

p <- p_date + p_week + plot_layout(axes = "collect")
ggsave("plots/plot-ansett-spans.pdf", p, width = 16, height = 7, units = "cm", device = cairo_pdf)
ggsave("plots/plot-ansett-spans.png", p, width = 16, height = 7, units = "cm", dpi = 300, bg = "white")
