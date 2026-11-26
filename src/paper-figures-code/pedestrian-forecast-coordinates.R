source("src/paper-figures-code/setup-common.R")
# library(fable)
# uad <- as_tsibble(USAccDeaths) 
# p <- uad |> 
#   model(
#     ETS = ETS(value),
#   ) |>
#   forecast(h = "3 years") |>
#   autoplot(uad) + 
#   theme(legend.position = "none") + 
#   labs(x = NULL, y = "Monthly US accidental deaths") + 
#   scale_x_yearmonth(date_labels = "%Y", date_breaks = "2 year")
# p

library(fable)
ped_sth_cross <- pedestrian |> 
  filter(
    Sensor == "Southern Cross Station",
    Date < as.Date("2015-03-27")
  )
is_weekend <- function(time) {
  wday(time) %in% c(1, 7)
}
p <- ped_sth_cross |>
  model(
    mdl = TSLM(log(Count + 1) ~ trend() + is_weekend(Date_Time) * fourier(K = 10)),
  ) |>
  forecast(h = "10 days") |> 
  autoplot(filter(ped_sth_cross, Date > as.Date("2015-03-15")), level = c(50, 80), colour = "#FB8C58") + 
  coord_calendar(rows = mixtime::weeks(1L), panes = NULL, cols = NULL, label_cells = NULL) + 
  # Complete the final week, otherwise its row is left empty
  expand_limits(x = as.POSIXct("2015-04-06", tz = "Australia/Melbourne")) +
  scale_y_continuous(breaks = c(2000, 4000)) +
  labs(x = NULL, y = "Hourly pedestrian count") +
  # The right margin fits the final day's axis label
  theme(legend.position = "none", plot.margin = margin(5.5, 11, 5.5, 5.5))

ggsave("plots/plot-forecast.pdf", plot = p, width = 12, height = 5.5, units = "cm")
ggsave("plots/plot-forecast.png", plot = p, width = 12, height = 5.5, units = "cm", dpi = 400, bg = "white")
