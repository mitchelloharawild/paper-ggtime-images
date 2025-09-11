source("src/paper-figures-code/setup-common.R")
p <- as_tsibble(USAccDeaths) |>
  ggplot(aes(x = month(index), y = value, group = year(index))) +
  geom_line() +
  # geom_point(aes(colour = year(index)), size = 2) +
  geom_vline(
    xintercept = c(1, 12),
    linetype = "dashed",
    colour = "grey50"
  ) +
  coord_polar() +
  scale_x_continuous(
    breaks = 1:12,
    labels = month.abb
  ) +
  labs(x = NULL, y = "Monthly Accidental Deaths in the USA")
p 
ggsave("plots/plot-overlap-polar.png", plot = p, width = 12, height = 8, units = "cm")
