source("src/paper-figures-code/setup-common.R")
p <- aus_production |> 
  filter(year(Quarter) >= 1991) |>
  feasts::ACF(Beer) |> 
  autoplot() + 
  labs(x = "Lag (Quarters)", y = "Autocorrelation (ACF)")
ggsave("plots/plot-acf.png", plot = p, width = 12, height = 8, units = "cm")
