source("setup-common.R")
library(dplyr)
# Australia/Melbourne Daylight savings time changes 2025
dst_end_mel <- as.POSIXct("2025-04-06 03:00:00", tz = "Australia/Melbourne")
dst_start_mel <- as.POSIXct("2025-10-05 02:00:00", tz = "Australia/Melbourne")

dst_start <- tibble(
  time = dst_start_mel + 3600 * (-2:2),
  values = 1:5 + rnorm(5, sd = 0.3)
)
dst_end <- tibble(
  time = dst_end_mel + 3600 * (-4:1),
  values = 1:6 + rnorm(6, sd = 0.3)
)

dst_change <- bind_rows(
  `DST Starts` = dst_start,
  `DST Ends` = dst_end,
  .id = "DST"
) |>
  group_by(DST) |>
  mutate(reftime = lubridate::force_tz(time[1], "UTC") + 3600 * (1:n() - 1L))

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

dst_civil_ggtime <- dst_change |>
  ggplot(aes(x = time, y = values)) +
  geom_time_line() +
  geom_line(aes(x = reftime), colour = NA) +
  geom_time_line(
    data = dst_change[c(2:3, 8:9), ],
    colour = "red",
    linewidth = 1
  ) +
  facet_grid(
    rows = vars("Civil Time\n(ggtime::geom_time_line)"),
    cols = vars(DST),
    scales = "free_x"
  ) +
  scale_y_continuous(minor_breaks = NULL) +
  scale_x_datetime(minor_breaks = NULL, date_labels = "%I%p", date_breaks = "1 hour") +
  labs(x = NULL, y = NULL)
ggsave(
  "graphics/dst-subfig/dst-civil-ggtime.svg",
  dst_civil_ggtime,
  width = 6,
  height = 2
)

dst_civil_ggplot2 <- dst_change |>
  ggplot(aes(x = time, y = values)) +
  geom_line(position = position_time_civil()) +
  geom_line(aes(x = reftime), colour = NA) +
  geom_line(
    position = position_time_civil(),
    data = dst_change[c(2:3, 8:9), ],
    colour = "red",
    linewidth = 1
  ) +
  facet_grid(
    rows = vars("Civil Time\n(ggplot2::geom_line)"),
    cols = vars(DST),
    scales = "free_x"
  ) +
  scale_y_continuous(minor_breaks = NULL) +
  scale_x_datetime(minor_breaks = NULL, date_labels = "%I%p", date_breaks = "1 hour") +
  labs(x = NULL, y = NULL)
ggsave(
  "graphics/dst-subfig/dst-civil-ggplot2.svg",
  dst_civil_ggplot2,
  width = 6,
  height = 2
)