source("setup-common.R")
p <- tsibble::pedestrian |>
  dplyr::filter(Date < "2015-02-01") |>
  mutate(
    Sensor = factor(Sensor, levels = c("Southern Cross Station", "Birrarung Marr", "QV Market-Elizabeth St (West)"), labels = c("Southern Cross Station", "Birrarung Marr", "QV Market"))
  ) |> 
  ggplot(aes(x = Date_Time, y = Count, color = Sensor)) +
  geom_line() +
  coord_calendar(time_rows = "1 week") +
  scale_x_datetime(date_breaks = "1 day", date_labels = "%a") +
  scale_y_continuous(breaks = NULL) +
  facet_grid(cols=vars(yearmonth(Date))) +
  theme(
    strip.background = element_rect(fill = "grey90", colour = "black"),
    panel.background = element_rect(fill = "white", colour = "black")
  ) +
  labs(x = NULL, y = "Hourly pedestrian counts", colour = NULL) +
  theme(legend.position = "bottom")

ggsave("plots/plot-pedestrian-calendar.png", plot = p, width = 12, height = 8, units = "cm")