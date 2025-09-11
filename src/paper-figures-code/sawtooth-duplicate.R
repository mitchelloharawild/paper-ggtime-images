source("src/paper-figures-code/setup-common.R")
aus_pop <- global_economy |> 
  filter(Country %in% c("Australia")) |> 
  select(Year, Country, Population) |> 
  as_tibble() |> 
  filter(Year >= 2000)
aus_pop <- bind_rows(aus_pop, aus_pop[11,])

aus_pop[19,]$Population <- aus_pop[19,]$Population*1.03

p <- aus_pop |> 
  ggplot(aes(x = Year, y = Population)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 5, data = duplicates(aus_pop, index = Year), colour = "#980000") +
  scale_x_continuous(n.breaks = 6) +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, scale = 1e-6, suffix = "m")) +
  labs(x = "Year", y = "Australian population (millions)") 
p
ggsave("plots/plot-sawtooth-duplicate.png", plot = p, width = 12, height = 8, units = "cm")