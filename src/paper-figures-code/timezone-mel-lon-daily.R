source("setup-common.R")
MEL_LDN_mockdata <- pedestrian |> 
  filter(
    between(Date, ymd("2015-03-02"), ymd("2015-03-12")),
    Sensor %in% c("Bourke Street Mall (North)", "QV Market-Elizabeth St (West)")
  ) |> 
  index_by(Date) |>
  group_by(Sensor) |>
  summarise(Count = sum(Count)) |>
  mutate(
    Sensor = factor(Sensor, levels = c("Bourke Street Mall (North)", "QV Market-Elizabeth St (West)"), labels = c("Melbourne", "London")),
    Count = case_when(Sensor == "London" ~ Count*1.75, TRUE ~ Count),
  )
  
daily_civil <- MEL_LDN_mockdata |> 
  autoplot(Count) + 
  geom_point()
daily_absolute <- MEL_LDN_mockdata |>
  mutate(
    Date = force_tzs(
      as.POSIXct(Date), 
      tzones = case_match(Sensor, 
        "Melbourne" ~ "Australia/Melbourne", 
        "London" ~ "Europe/London"
      )
    )
  ) |> 
  autoplot(Count) + 
  geom_point()

daily_civil / daily_absolute