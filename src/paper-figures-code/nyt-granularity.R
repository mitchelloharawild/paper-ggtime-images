source("setup-common.R")
library(ggplot2)
library(dplyr)

col_homicide <- "#FB8C58"
col_police_killings <- "#980000"

nyt_floyd_plot_common <- function(p, xoffset = 0) {
  p +   
  # Add vertical line for George Floyd's death (assuming 2020)
  geom_vline(xintercept = 2020.3686, linetype = "dashed", color = "gray60", linewidth = 0.8) +
  
  # Custom colors to match the reference
  scale_color_manual(values = c("Homicides" = col_homicide, "Police killings" = col_police_killings)) +
  
  # Format y-axis as years
  scale_x_continuous(
    breaks = 2015:2024 + xoffset, labels = \(x) format(floor(x)),
    expand = c(0, 0.2)
  ) +

  # Format y-axis as percentages
  scale_y_continuous(
    breaks = seq(-10, 50, 10), labels = scales::label_percent(scale = 1),
    limits = c(-10, 50), expand = c(0, 0)
  ) +

  # Customize theme
  theme_minimal() +
  theme(
    # Remove legend
    legend.position = "none",
    
    # Grid lines
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", size = 0.5),
    
    # Axis styling
    axis.line.x = element_line(color = "black", size = 0.8),
    axis.text = element_text(color = "gray30", size = 10),
    axis.title = element_blank(),
    axis.ticks.x = element_line(color = "gray30"),
    axis.ticks.y = element_blank(),
    
    # Plot styling
    plot.title = element_text(size = 16, face = "bold", margin = margin(b = 5)),
    plot.subtitle = element_text(size = 12, color = "gray50", margin = margin(b = 20)),
    plot.caption = element_text(size = 9, color = "gray50", hjust = 0, margin = margin(t = 20)),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  ) +
  
  # Add labels
  labs(
    title = "Police killings continued to rise as other homicides fell",
    subtitle = "Percent change in homicides and police killings since 2015"#,
    # caption = "Sources: Analysis of data compiled by The Washington Post and Mapping Police Violence. Homicide data from the\nCenters for Disease Control and Prevention. — The New York Times"
  ) +
  
  # Add shaded region for the year 2020
  annotate("rect", xmin = 2020, xmax = 2021, 
           ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.3) +

  # Expand x-axis to make room for labels
  coord_cartesian(clip = "off")
}

p1 <- (readr::read_csv("src/chi26-figures/nyt-floyd.csv") |> 
  ggplot(aes(x = year, y = percent_change, colour = type)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5)) |> 
  nyt_floyd_plot_common() + 
  # Add annotation for George Floyd's death
  annotate(
    "label", x = 2020.3686, y = 50, label = "Death of George Floyd\nMay 25, 2020", 
     hjust = 0.5, vjust = 0.3, size = 4, color = "gray30", fill = "white", border.colour = "white"
  ) +
  # Add direct labels for the lines
  annotate(
    "text", x = 2022.3, y = 39, label = "Homicides", 
    color = col_homicide, size = 4, hjust = 0, fontface = "bold"
  ) +
  
  annotate(
    "text", x = 2022.3, y = 6, label = "Police killings", 
    color = col_police_killings, size = 4, hjust = 0, fontface = "bold"
  )
  
p1

p2 <- (readr::read_csv("src/chi26-figures/nyt-floyd.csv") |> 
  ggplot(aes(x = year, y = percent_change, colour = type)) +
  geom_step(linewidth = 1.2)) |> 
  nyt_floyd_plot_common() + 
  labs(title = NULL, subtitle = NULL) + 
  # Add annotation for George Floyd's death
  annotate(
    "label", x = 2017.7, y = 40, label = "Death of George Floyd\nMay 25, 2020", 
     hjust = 0.5, vjust = 0.3, size = 4, color = "gray30", fill = "#ffffff99", border.colour = "transparent"
  ) +
  # Add direct labels for the lines
  annotate(
    "text", x = 2022.2 , y = 43, label = "Homicides", 
    color = col_homicide, size = 4, hjust = 0, fontface = "bold"
  ) +
  
  annotate(
    "text", x = 2021.5, y = 0, label = "Police killings", 
    color = col_police_killings, size = 4, hjust = 0, fontface = "bold"
  )
p2

p3 <- (readr::read_csv("src/chi26-figures/nyt-floyd.csv") |> 
  ggplot(aes(x = year + 0.5, y = percent_change, colour = type)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5)) |> 
  nyt_floyd_plot_common(xoffset = 0.5) + 
  labs(title = NULL, subtitle = NULL) +
  # Add annotation for George Floyd's death
  annotate(
    "label", x = 2017.7, y = 40, label = "Death of George Floyd\nMay 25, 2020", 
     hjust = 0.5, vjust = 0.3, size = 4, color = "gray30", fill = "#ffffff99", border.colour = "transparent"
  ) +
  # Add direct labels for the lines
  annotate(
    "text", x = 2022.7 , y = 43, label = "Homicides", 
    color = col_homicide, size = 4, hjust = 0, fontface = "bold"
  ) +
  
  annotate(
    "text", x = 2022, y = 0, label = "Police killings", 
    color = col_police_killings, size = 4, hjust = 0, fontface = "bold"
  )
p3

ggsave("plots/nyt-subfig/plot-nyt-floyd-original.svg", plot = p1, width = 8, height = 3, units = "in")
ggsave("plots/nyt-subfig/plot-nyt-floyd-step.svg", plot = p2, width = 4, height = 3, units = "in")
ggsave("plots/nyt-subfig/plot-nyt-floyd-align.svg", plot = p3, width = 4, height = 3, units = "in")