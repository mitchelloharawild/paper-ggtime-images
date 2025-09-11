source("src/paper-figures-code/setup-common.R")
# Using the NYT Floyd data to show sawtooth effect by missing the group by type
p <- readr::read_csv("src/chi26-figures/nyt-floyd.csv") |> 
  ggplot(aes(x = year, y = percent_change)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) + 
  scale_x_continuous(breaks = 2015:2024, labels = function(x) scales::number(floor(x))) +
  labs(x = "Year", y = "Percent change since 2015")
ggsave("plots/plot-sawtooth-group.png", plot = p, width = 12, height = 8, units = "cm")