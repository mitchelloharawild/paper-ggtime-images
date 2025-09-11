source("src/paper-figures-code/setup-common.R")

col_homicide <- "#FB8C58"
col_police_killings <- "#980000"

# Using the NYT Floyd data to show sawtooth effect by missing the group by type
p <- readr::read_csv("src/chi26-figures/nyt-floyd.csv") |> 
  ggplot(aes(x = year, y = percent_change)) +
  geom_line(linewidth = 1.2) +
  geom_point(aes(colour = type), size = 5) + 
  scale_x_continuous(breaks = 2015:2024) +
  labs(x = "Year", y = "Percent change since 2015", colour = NULL) +
  theme(
    legend.position = c(0.02, 0.98),
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = alpha('white', 0.7)) 
  ) +
  scale_color_manual(values = c("Homicides" = col_homicide, "Police killings" = col_police_killings))
p
ggsave("plots/plot-sawtooth-group.svg", plot = p, width = 12, height = 8, units = "cm")
