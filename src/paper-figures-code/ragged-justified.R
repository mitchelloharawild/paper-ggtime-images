source("setup-common.R")
month_progress <- unlist(lapply(lubridate:::N_DAYS_IN_MONTHS[seq_len(3)], \(x) seq(1, x)/x))

time <- make_date(year = 2025) + 0:89
# y <- rnorm(length(month_progress), mean = month_progress^4, sd = month_progress*0.1)
y <- sin(month_progress*2*pi) + rnorm(length(time), sd = 0.1)


tbl_ragged <- tibble(time, y, month_progress)
plt_ragged <- tbl_ragged |> 
  ggplot(aes(x = time - floor_date(time, "month"), y = y, group = month(time))) + 
  geom_line() +
  geom_point(
    data = tbl_ragged |> group_by(yearmonth(time)) |> filter(time %in% range(time)),
    size = 3
  ) + 
  labs(x = "Day of month")

time <- make_date(year = 2025) + 0:89
# y <- rnorm(length(month_progress), mean = month_progress^4, sd = month_progress*0.1)
y <- sin(month_progress*2*pi) + rnorm(90, sd = 0.1)

tbl_justified <- tibble(time, y, month_progress)
plt_justified <- tbl_justified |> 
  ggplot(aes(x = month_progress, y = y, group = month(time))) + 
  scale_x_continuous(labels = scales::percent) +
  geom_line() +
  geom_point(
    data = tbl_justified|> group_by(yearmonth(time)) |> filter(time %in% range(time)), 
    size = 3
  ) +
    labs(x = "Percentage of month")
  

((plt_ragged + guides(colour = "none")) / plt_justified) & aes(colour = month(time, label = TRUE)) & plot_layout(guides = "collect", axes = "collect") & theme(legend.position = "bottom") & labs(colour = "Month", y = NULL)
((plt_ragged + 
  facet_grid(rows = vars(month(time, label = TRUE)))) |
(plt_justified +
  facet_grid(rows = vars(month(time, label = TRUE))))) & aes(colour = month(time, label = TRUE))

wrap_plots(
  plt_ragged + facet_grid(rows = vars(month(time, label = TRUE))),
  plt_justified + facet_grid(rows = vars(month(time, label = TRUE))),
  ncol = 2, guides = "collect", axes = "collect"
) & labs(y = NULL)