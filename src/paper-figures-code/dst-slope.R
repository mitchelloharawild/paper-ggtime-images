source("src/paper-figures-code/setup-common.R")
library(dplyr)
# Australia/Melbourne Daylight savings time changes 2025
dst_end_mel <- as.POSIXct("2025-04-06 03:00:00", tz = "Australia/Melbourne")
dst_start_mel <- as.POSIXct("2025-10-05 02:00:00", tz = "Australia/Melbourne")

dst_start <- tibble(
  time = dst_start_mel + 3600 * (-2:2),
  values = c(0, 0.18, 0.606, 0.689, 1)
)
dst_end <- tibble(
  time = dst_end_mel + 3600 * (-4:1),
  values = c(0, 0.274, 0.393, 0.666, 0.752, 1)
)

dst_change <- bind_rows(
  `DST Starts` = dst_start,
  `DST Ends` = dst_end,
  .id = "DST"
) |>
  group_by(DST) |>
  mutate(
    reftime = lubridate::force_tz(time[1], "UTC") + 3600 * (1:n() - 1L),
    # Observations at exact instants, so the DST transition is located precisely
    localtime = mixtime::mixtime(
      time,
      chronon = mixtime::cal_gregorian$second(1L, tz = "Australia/Melbourne")
    )
  )

dst_abs <- dst_change |>
  ggplot(aes(x = time, y = values)) +
  geom_line() +
  geom_line(
    data = dst_change[c(2:3, 8:9), ],
    colour = "red",
    linewidth = 1
  ) +
  geom_line(
    data = tibble(
      time = dst_start_mel + c(0, 3600 * 3),
      values = 3,
      DST = "DST Starts"
    ),
    colour = NA
  ) +
  facet_grid(
    rows = vars("Absolute Time\n(ggplot2::geom_line)"),
    cols = vars(DST),
    scales = "free_x"
  ) +
  scale_y_continuous(minor_breaks = NULL) +
  scale_x_datetime(minor_breaks = NULL, date_labels = "%I%p", date_breaks = "1 hour") +
  labs(x = NULL, y = NULL)
ggsave("graphics/dst-subfig/dst-absolute.svg", dst_abs, width = 6, height = 2)

# Breaks at the start of each hour (hourly granule breaks are centred by default)
hour_starts <- mixtime::mixtime(
  c(
    as.POSIXct("2025-04-06", tz = "UTC") + 3600 * (0:5),
    as.POSIXct("2025-10-05", tz = "UTC") + 3600 * (0:5)
  ),
  chronon = mixtime::cal_gregorian$second(1L, tz = NA)
)

dst_civil_ggtime <- dst_change |>
  ggplot(aes(x = localtime, y = values)) +
  geom_time_line() +
  geom_line(aes(x = reftime), colour = NA) +
  geom_time_line(
    data = dst_change[c(2:3, 8:9), ],
    colour = "red",
    linewidth = 1,
    transitions = mixtime::tz_transitions(
      min(dst_change$time) - 86400, max(dst_change$time) + 86400
    ),
    transition_aesthetics = list(linetype = 2, colour = "black", linewidth = 0.5)
  ) +
  facet_grid(
    rows = vars("Local Time\n(ggtime::geom_time_line)"),
    cols = vars(DST),
    scales = "free_x"
  ) +
  scale_y_continuous(minor_breaks = NULL) +
  # A common chronon without a timezone (tz = NA) places time in local time
  scale_x_mixtime(
    time_chronon = mixtime::cal_gregorian$hour(1L, tz = NA),
    minor_breaks = NULL, breaks = hour_starts,
    time_labels = "{cyc(hour, ampm)}{cyc(ampm, day, label = TRUE)}"
  ) +
  labs(x = NULL, y = NULL)
ggsave(
  "graphics/dst-subfig/dst-civil-ggtime.svg",
  dst_civil_ggtime,
  width = 6,
  height = 1.5
)

dst_civil_ggplot2 <- dst_change |>
  ggplot(aes(x = localtime, y = values)) +
  geom_line() +
  geom_line(aes(x = reftime), colour = NA) +
  geom_line(
    data = dst_change[c(2:3, 8:9), ],
    colour = "red",
    linewidth = 1
  ) +
  facet_grid(
    rows = vars("Local Time\n(ggplot2::geom_line)"),
    cols = vars(DST),
    scales = "free_x"
  ) +
  scale_y_continuous(minor_breaks = NULL) +
  # A common chronon without a timezone (tz = NA) places time in local time
  scale_x_mixtime(
    time_chronon = mixtime::cal_gregorian$hour(1L, tz = NA),
    minor_breaks = NULL, breaks = hour_starts,
    time_labels = "{cyc(hour, ampm)}{cyc(ampm, day, label = TRUE)}"
  ) +
  labs(x = NULL, y = NULL)
ggsave(
  "graphics/dst-subfig/dst-civil-ggplot2.svg",
  dst_civil_ggplot2,
  width = 6,
  height = 1.5
)