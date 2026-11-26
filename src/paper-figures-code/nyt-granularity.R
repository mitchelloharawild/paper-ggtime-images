source("src/paper-figures-code/setup-common.R")
library(ggplot2)
library(dplyr)

col_homicide <- "#FB8C58"
col_police_killings <- "#980000"

nyt_floyd_plot_common <- function(p) {
  p +
  # Custom colors to match the reference
  scale_color_manual(values = c("Homicides" = col_homicide, "Police killings" = col_police_killings)) +
  
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
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.5),
    
    # Axis styling
    axis.line.x = element_line(color = "black", linewidth = 0.8),
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
  
  # Expand x-axis to make room for labels
  coord_cartesian(clip = "off")
}

# The original graphic, with each year positioned at its start on a numeric axis
p1 <- (readr::read_csv("src/chi26-figures/nyt-floyd.csv") |> 
  ggplot(aes(x = year, y = percent_change, colour = type)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  # Add vertical line for George Floyd's death (assuming 2020)
  geom_vline(xintercept = 2020.3686, linetype = "dashed", color = "gray60", linewidth = 0.8)) |> 
  nyt_floyd_plot_common() + 
  # Format x-axis as years
  scale_x_continuous(breaks = 2015:2024, expand = c(0, 0.2)) +
  # Add shaded region for the year 2020
  annotate("rect", xmin = 2020, xmax = 2021, 
           ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.3) +
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

# The ggtime alternatives treat each value as a year (a time span), so the
# default guide_axis_mixtime() ticks the start and end of each year and labels
# the span between them
floyd <- readr::read_csv("src/chi26-figures/nyt-floyd.csv") |> 
  mutate(year = mixtime::year(lubridate::make_date(year)))

event <- mixtime::date(as.Date("2020-05-25"))

# Both panels share the same limits so that their time axes line up
scale_x_floyd <- function(...) {
  scale_x_mixtime(
    time_breaks = mixtime::years(1L),
    limits = mixtime::year(as.Date(c("2015-01-01", "2025-01-01"))),
    expand = c(0, 0.2), ...
  )
}

# Annotations are added after the data, since an event (daily) layer coming first
# would produce a daily time axis
nyt_floyd_annotate <- function(p, homicides_at, police_at) {
  label_line <- function(date, y, label, colour) {
    annotate(
      "text", x = mixtime::date(as.Date(date)), y = y, label = label, 
      color = colour, size = 3.4, hjust = 0, fontface = "bold"
    )
  }
  nyt_floyd_plot_common(p) +
  # Add shaded region for the year 2020
  geom_rect(
    aes(xmin = year, xmax = year), ymin = -Inf, ymax = Inf, 
    data = data.frame(year = mixtime::year(as.Date("2020-01-01"))),
    inherit.aes = FALSE, fill = "grey80", alpha = 0.3
  ) +
  # Add vertical line for George Floyd's death
  geom_vline(xintercept = event, linetype = "dashed", color = "gray60", linewidth = 0.8) +
  # Add annotation for George Floyd's death
  annotate(
    "label", x = mixtime::date(as.Date("2017-07-01")), y = 41, label = "Death of George Floyd\nMay 25, 2020", 
    size = 3.2, color = "gray30", fill = "#ffffffcc", border.colour = NA
  ) +
  # Add direct labels for the lines
  label_line(homicides_at, 43, "Homicides", col_homicide) +
  label_line(police_at, 0, "Police killings", col_police_killings) +
  labs(title = NULL, subtitle = NULL) +
  theme(
    axis.text = element_text(color = "gray30", size = 9),
    axis.line.x = element_line(color = "black"),
    panel.grid.major.y = element_line(color = "gray92")
  )
}

# Each value held constant over its year
p2 <- (floyd |>
  # Repeat the last value at the end of its year, so the step spans all of 2024
  (\(x) bind_rows(x, mutate(filter(x, year == max(year)), year = year + 1L)))() |>
  ggplot(aes(x = year, y = percent_change, colour = type)) +
  geom_step(linewidth = 1.2)) |> 
  nyt_floyd_annotate(homicides_at = "2022-03-01", police_at = "2021-08-01") +
  # Steps start at the start of each year (the shaded region still spans 2020)
  scale_x_floyd(align_discrete = aes_nudge(center = 0))
p2

# Each value positioned at the middle of its year (the default)
p3 <- (floyd |> 
  ggplot(aes(x = year, y = percent_change, colour = type)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5)) |> 
  nyt_floyd_annotate(homicides_at = "2022-10-01", police_at = "2022-04-01") +
  scale_x_floyd()
p3

ggsave("plots/nyt-subfig/plot-nyt-floyd-original.svg", plot = p1, width = 8, height = 3, units = "in")
ggsave("plots/nyt-subfig/plot-nyt-floyd-step.svg", plot = p2, width = 4, height = 2.3, units = "in")
ggsave("plots/nyt-subfig/plot-nyt-floyd-align.svg", plot = p3, width = 4, height = 2.3, units = "in")