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
  autoplot(filter(ped_sth_cross, Date > as.Date("2015-03-15"))) + 
  coord_calendar(time_rows = "1 week") + 
  labs(x = NULL, y = "Hourly pedestrian count") +
  theme(legend.position = "none")

ggsave("plots/plot-forecast.pdf", plot = p, width = 12, height = 8, units = "cm")