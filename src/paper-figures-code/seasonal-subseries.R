source("src/paper-figures-code/setup-common.R")
p <- aus_production |> 
  filter(year(Quarter) >= 1991) |>
  ggtime::gg_subseries(Beer) + 
  theme(
    strip.background = element_rect(fill = "grey90", colour = "black"),
    panel.background = element_rect(fill = "white", colour = "black")
  )
p
ggsave("plots/plot-seasonal-subseries.pdf", plot = p, width = 12, height = 8, units = "cm")