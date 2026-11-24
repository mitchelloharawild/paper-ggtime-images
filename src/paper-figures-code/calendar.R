source("src/paper-figures-code/setup-common.R")

# Two months of hourly pedestrian counts, including the Easter long weekend
pedestrian_hourly <- tsibble::pedestrian |>
  as_tibble() |>
  filter(Sensor == "Southern Cross Station", Date >= as.Date("2015-04-01"), Date < as.Date("2015-06-01")) |>
  mutate(time = mixtime::datetime(Date_Time), thousands = Count / 1000)
easter <- pedestrian_hourly |>
  filter(Date >= as.Date("2015-04-03"), Date <= as.Date("2015-04-06"))

# Days are named at the middle of their cells, from the first week of the calendar
noon <- as.POSIXct("2015-03-30 12:00", tz = "Australia/Melbourne") + days(0:6)

# Months are columns (rather than panes) to fit a single column of the paper
p <- ggplot(pedestrian_hourly, aes(time, thousands)) +
  geom_line(linewidth = 0.25) +
  geom_line(data = easter, colour = "#FB8C58", linewidth = 0.6) +
  coord_calendar(
    cells = mixtime::cal_gregorian$day(1L), rows = mixtime::cal_isoweek$week(1L),
    panes = NULL, cols = mixtime::cal_gregorian$month(1L), col_spacing = 0.06,
    label_cells = "{cyc(day, month)}", label_panes = NULL,
    label_cols = "{cyc(month, year, label = TRUE, abbreviate = TRUE)}"
  ) +
  scale_x_mixtime(
    breaks = mixtime::datetime(noon), minor_breaks = NULL,
    labels = format(noon, "%a")
  ) +
  scale_y_continuous(breaks = NULL, limits = c(0, 4.6), expand = expansion(0)) +
  labs(x = NULL, y = NULL) +
  theme(
    axis.text.x = element_text(size = 5),
    text = element_text(size = 5),
    ggtime.calendar.cell.text = element_text(size = 4.5, colour = "grey50", hjust = 1, vjust = 1, margin = margin(1.5, 1.5, 1.5, 1.5)),
    ggtime.calendar.col.text = element_text(size = 6, face = "bold", hjust = 0, vjust = 1, margin = margin(2, 2, 2, 2)),
    ggtime.calendar.cell.line = element_line(linewidth = 0.3),
    plot.margin = margin(1, 1, 1, 1)
  )
ggsave("plots/plot-calendar.pdf", p, width = 8.5, height = 4.5, units = "cm", device = cairo_pdf)
ggsave("plots/plot-calendar.png", p, width = 8.5, height = 4.5, units = "cm", dpi = 400, bg = "white")
