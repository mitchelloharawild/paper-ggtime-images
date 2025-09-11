library(ggplot2)
library(dplyr)
set.seed(2022)
days_of_the_week <- c("Sunday", "Monday", "Tuesday", "Wednesday", 
                      "Thursday", "Friday", "Saturday")
sales <- data.frame("dayOfMonth" = rep(1:28, 12),
                    "month" = rep(month.name, each = 28),
                    "weekday" = rep(days_of_the_week, 12*4),
                    "paintings" = round(rnorm(28*12, c(sample(1:28, 7)))))

p <- sales %>%
  group_by(weekday) %>%
  summarise(weekdaySales = mean(paintings)) %>%
  ggplot() +
  labs(title = "Mean Painting Sales by Day of the Week",
       x = "Day of the Week",
       y = "Mean Painting Sales") +
  scale_fill_brewer(palette = "Set2") +
  geom_bar(aes(x = weekday, y = weekdaySales, fill = weekday), 
           stat = "identity", show.legend = FALSE)


ggsave(
  "external/gus-lipkin-blogpost.svg",
  p,
  width = 8,
  height = 6
)
