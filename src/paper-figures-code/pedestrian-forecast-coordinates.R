source("src/paper-figures-code/setup-common.R")
library(fable)
uad <- as_tsibble(USAccDeaths) 
p <- uad |> 
  model(
    ETS = ETS(value),
  ) |>
  forecast(h = "3 years") |>
  autoplot(uad) + 
  theme(legend.position = "none") + 
  labs(x = NULL, y = "Monthly US accidental deaths") + 
  scale_x_yearmonth(date_labels = "%Y", date_breaks = "2 year")
p

ggsave("plots/plot-forecast.png", plot = p, width = 12, height = 8, units = "cm")