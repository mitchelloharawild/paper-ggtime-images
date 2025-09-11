source("setup-common.R")

icon_theme <- list(
  theme_void(), 
  theme(
    plot.background = element_rect(colour = "black", fill = "grey85"),
    strip.background = element_rect(fill = "grey70", colour = "black")
  )
)

(pedestrian |> 
  filter(Sensor == "Southern Cross Station") |> 
  filter(Date >= ymd("2016-10-03") & Date <= ymd("2016-10-09")) |> 
  ggplot(aes(x = Date_Time, y = Count)) +
  geom_line() + 
  labs(x = NULL, y = NULL) + 
  icon_theme) |> 
  ggsave(
    "graphics/navigation-subfig/pedestrian-cartesian.svg", plot = _, width = 2, height = 2, units = "cm"
  )

(pedestrian |> 
  filter(Sensor == "Southern Cross Station") |> 
  filter(Date >= ymd("2016-10-03") & Date <= ymd("2016-10-09")) |> 
  ggplot(aes(x = hour(Date_Time), group = Date, y = Count)) +
  geom_line() + 
  labs(x = NULL, y = NULL) + 
  icon_theme) |> 
  ggsave(
    "graphics/navigation-subfig/pedestrian-ggplot-loop.svg", plot = _, width = 2, height = 2, units = "cm"
  )

(pedestrian |> 
  filter(Sensor == "Southern Cross Station") |> 
  filter(Date >= ymd("2016-10-03") & Date <= ymd("2016-10-16")) |> 
  ggplot(aes(x = Date_Time, y = Count)) +
  geom_line() + 
  facet_wrap(
    vars(floor_date(Date_Time, "1 week", week_start = 1L)),
    ncol = 1L,
    scales = "free_x"
  ) +
  labs(x = NULL, y = NULL) + 
  icon_theme) |> 
  ggsave(
    "graphics/navigation-subfig/pedestrian-ggplot-calendar.svg", plot = _, width = 2, height = 2, units = "cm"
  )

# tz forced because we're using lubridate::floor_date for now which ignores tz
# this is fixed in mixtime
(tsibble::pedestrian |> 
  filter(Sensor == "Southern Cross Station") |> 
  filter(Date >= ymd("2016-10-03") & Date <= ymd("2016-10-09")) |> 
  ggplot(aes(x = force_tz(Date_Time, "UTC"), y = Count)) +
  geom_line() + 
  coord_loop(time_loops = "1 day") +
  labs(x = NULL, y = NULL) + 
  icon_theme) |> 
  ggsave(
    "graphics/navigation-subfig/pedestrian-ggtime-loop.svg", plot = _, width = 2, height = 2, units = "cm"
  )

(tsibble::pedestrian |> 
  filter(Sensor == "Southern Cross Station") |> 
  filter(Date >= ymd("2016-10-03") & Date <= ymd("2016-10-16")) |> 
  ggplot(aes(x = force_tz(Date_Time, "UTC"), y = Count)) +
  geom_line() + 
  coord_calendar(time_rows = "1 week") +
  labs(x = NULL, y = NULL) + 
  icon_theme) |> 
  ggsave(
    "graphics/navigation-subfig/pedestrian-ggtime-calendar.svg", plot = _, width = 2, height = 2, units = "cm"
  )