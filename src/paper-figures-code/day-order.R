source("src/paper-figures-code/setup-common.R")

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
    day_text = format(Date, "%a"),
    day_cyclical = mixtime::day_of_week(Date),
    day_linear = mixtime::date(Date)
  )

day_order_common <- function(title, subtitle) {
  list(
    geom_line(linewidth = 0.4),
    geom_point(size = 1),
    scale_y_continuous(limits = c(0, 21), minor_breaks = NULL, expand = expansion(0)),
    labs(title = title, subtitle = subtitle, x = NULL, y = "Pedestrians (thousands)"),
    theme(
      plot.title = element_text(size = 9, face = "bold"),
      plot.subtitle = element_text(size = 8, lineheight = 1.1),
      axis.text.x = element_text(size = 7),
      axis.title.y = element_text(size = 9),
      axis.ticks.x = element_line(colour = "black", linewidth = 0.3),
      axis.ticks.length.x = unit(1.2, "mm"),
      panel.grid.minor.x = element_blank()
    )
  )
}

p_text <- ggplot(pedestrian_daily, aes(day_text, thousands, group = week)) +
  day_order_common("Time as text", "• Days in lexicographic order\n• Pattern is scrambled") +
  scale_x_discrete(expand = expansion(add = 0.5))

# ggtime's scales do not support cyclical time yet, so a discrete scale is used
# with the days ordered by mixtime
day_levels <- format(sort(unique(pedestrian_daily$day_cyclical)))

p_cyclical <- ggplot(pedestrian_daily, aes(format(day_cyclical), thousands, group = week)) +
  day_order_common("Cyclical time (mixtime)", "• Days in order within weeks\n• Weeks are disconnected") +
  scale_x_discrete(limits = day_levels, expand = expansion(add = 0.5))

p_linear <- ggplot(pedestrian_daily, aes(day_linear, thousands)) +
  day_order_common("Looped linear time (ggtime)", "• Days in order within weeks\n• Weeks are connected") +
  coord_loop(time_loops = mixtime::cal_isoweek$week(1L))

p <- p_text + p_cyclical + p_linear + plot_layout(axes = "collect")
ggsave("plots/plot-day-order.pdf", p, width = 18, height = 6.5, units = "cm", device = cairo_pdf)
ggsave("plots/plot-day-order.png", p, width = 18, height = 6.5, units = "cm", dpi = 300, bg = "white")
