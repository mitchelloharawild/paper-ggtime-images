source("src/paper-figures-code/setup-common.R")
source("src/paper-figures-code/setup-annotations.R")
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

# Position of a date, as a proportion of the panel
npc_date <- function(x) as.numeric(as.Date(x) - limits[1]) / as.numeric(diff(limits))

# The shading stops below the annotations
shade_top <- 22.5
shade <- function(mapping) {
  geom_rect(mapping, data = strike, inherit.aes = FALSE, fill = highlight, alpha = 0.4)
}

# A bracket which shows that the gridlines are weeks
week_bracket <- annotate_grob(grobTree(
  segmentsGrob(
    npc_date("1989-10-16"), 0.2, npc_date("1989-10-23"), 0.2,
    arrow = arrow(length = unit(0.7, "mm"), ends = "both", angle = 90),
    gp = gpar(col = "grey30", lwd = 0.9)
  ),
  textGrob("1 week", npc_date("1989-10-19"), 0.27, gp = gpar(fontsize = 6, col = "grey30"))
))

ansett_common <- list(
  geom_line(linewidth = 0.4),
  geom_point(size = 1),
  scale_y_continuous(limits = c(0, 25), breaks = c(0, 10, 20), minor_breaks = NULL),
  labs(y = "Passengers (thousands)"),
  theme(panel.grid.minor.x = element_line(linewidth = 0.3, colour = "grey92")),
  week_bracket
)

p_date <- ggplot(ansett_strike, aes(date, thousands)) +
  shade(aes(xmin = date_start, xmax = date_end, ymin = -Inf, ymax = shade_top)) +
  # The week omitted from the shading
  geom_rect(
    aes(xmin = date_end, xmax = date_end + 7, ymin = -Inf, ymax = shade_top),
    data = strike, inherit.aes = FALSE,
    fill = "grey80", colour = "grey40", linetype = 2, linewidth = 0.3
  ) +
  ansett_common +
  scale_x_date(
    breaks = labelled, minor_breaks = weeks, date_labels = "%b %d",
    limits = limits, expand = expansion(0)
  ) +
  annotate_mark(FALSE, "Last week without passengers is not shaded") +
  annotate_arrow(c(0.635, 0.96), c(npc_date(strike$date_end + 3.5), 0.75), -0.4)

p_week <- ggplot(ansett_strike, aes(week, thousands)) +
  shade(aes(xmin = week_start, xmax = week_end, ymin = -Inf, ymax = shade_top)) +
  ansett_common +
  # A common chronon of days, to match the dates
  scale_x_mixtime(
    time_chronon = mixtime::cal_gregorian$day(1L),
    # Breaks and limits at the start of days
    breaks = day(labelled), minor_breaks = day(weeks),
    labels = format(labelled, "%b %d"),
    limits = day(limits), expand = expansion(0)
  ) +
  annotate_mark(TRUE, "All seven weeks are shaded")

# The displayed code is abbreviated from the code above
p <- header_layout(
  "p <- ggplot(ansett, aes(y = passengers)) + geom_line() + geom_point()",
  titles = c("Weeks as dates (ggplot2)", "Weeks as time spans (ggtime)"),
  codes = c(
    "p + aes(date) + geom_rect(aes(xmin = date_start, xmax = date_end), strike)",
    "p + aes(week) + geom_rect(aes(xmin = week_start, xmax = week_end), strike)"
  ),
  plots = list(p_date, p_week)
)
ggsave("plots/plot-ansett-spans.pdf", p, width = 18, height = 7, units = "cm", device = cairo_pdf)
ggsave("plots/plot-ansett-spans.png", p, width = 18, height = 7, units = "cm", dpi = 300, bg = "white")
