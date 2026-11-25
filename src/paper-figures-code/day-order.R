source("src/paper-figures-code/setup-common.R")
source("src/paper-figures-code/setup-annotations.R")

# Four weeks (Monday to Sunday) of daily pedestrian counts
pedestrian_daily <- tsibble::pedestrian |>
  as_tibble() |>
  filter(
    Sensor == "Southern Cross Station",
    Date >= as.Date("2016-10-03"), Date <= as.Date("2016-10-30")
  ) |>
  summarise(thousands = sum(Count) / 1000, .by = Date) |>
  mutate(
    week = floor_date(Date, "1 week", week_start = 1L),
    # Weeks from Sunday, which pair each Sunday with the next Monday
    sunday = floor_date(Date, "1 week", week_start = 7L),
    day_text = format(Date, "%a"),
    day_cyclical = mixtime::day_of_week(Date),
    day_linear = mixtime::date(Date)
  )

sunday_monday <- pedestrian_daily |>
  filter(day_text %in% c("Sun", "Mon"))
sunday_thursday <- pedestrian_daily |>
  filter(day_text %in% c("Sun", "Thu"))
connected <- sunday_monday |>
  filter(n() == 2, .by = sunday)

day_order_common <- list(
  geom_line(linewidth = 0.35),
  geom_point(size = 0.8),
  scale_y_continuous(limits = c(0, 25), breaks = c(0, 10, 20), expand = expansion(0)),
  labs(y = "Pedestrians (thousands)"),
  theme(panel.grid.minor = element_blank())
)

p_text <- ggplot(pedestrian_daily, aes(day_text, thousands, group = week)) +
  day_order_common +
  geom_line(data = sunday_thursday, colour = highlight, linewidth = 0.8) +
  scale_x_discrete(expand = expansion(add = 0.5)) +
  annotate_mark(FALSE, "Sunday is joined to Thursday") +
  annotate_arrow(c(0.56, 0.9), c(0.56, 0.48), -0.15)

# ggtime's scales do not support cyclical time yet, so a discrete scale is used
# with the days ordered by mixtime
day_levels <- format(sort(unique(pedestrian_daily$day_cyclical)))

p_cyclical <- ggplot(pedestrian_daily, aes(format(day_cyclical), thousands, group = week)) +
  day_order_common +
  geom_point(data = sunday_monday, colour = highlight, size = 1.3) +
  scale_x_discrete(limits = day_levels, expand = expansion(add = 0.5)) +
  annotate_mark(FALSE, "No line from Sunday to Monday") +
  annotate_arrow(c(0.73, 0.95), c(0.93, 0.13), -0.35)

p_linear <- ggplot(pedestrian_daily, aes(day_linear, thousands)) +
  day_order_common +
  geom_line(aes(group = sunday), data = connected, colour = highlight, linewidth = 0.8) +
  geom_point(data = connected, colour = highlight, size = 1.1) +
  coord_loop(time_loops = mixtime::cal_isoweek$week(1L)) +
  annotate_mark(TRUE, "Weeks stay connected")

# The displayed code is abbreviated from the code above
p <- header_layout(
  "p <- ggplot(pedestrians, aes(y = count)) + geom_line() + geom_point()",
  titles = c("Time as text", "Cyclical time (mixtime)", "Looped linear time (ggtime)"),
  codes = c(
    'p + aes(format(date, "%a"), group = week)',
    "p + aes(day_of_week(date), group = week)",
    "p + aes(date) + coord_loop(time_loops = week(1L))"
  ),
  plots = list(p_text, p_cyclical, p_linear)
)
ggsave("plots/plot-day-order.pdf", p, width = 18, height = 6.6, units = "cm", device = cairo_pdf)
ggsave("plots/plot-day-order.png", p, width = 18, height = 6.6, units = "cm", dpi = 300, bg = "white")
