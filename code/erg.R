here::i_am("code/erg.R")
source(here::here("code/load.R"))

#load data
dat <- list.files(path = here("data/"),
                  pattern = ".csv",
                  full.names = TRUE) |>
  map_dfr(read_csv)

#cleaning
dat <- dat |>
  #fix dates
  mutate(Date = as.Date(Date),
  #make NAs of Rest Distance 0
  `Rest Distance` = if_else(is.na(`Rest Distance`),
                                    0,
                                    `Rest Distance`),
  #make summed distance measure
  Distance = `Work Distance` + `Rest Distance`,
  # watts per HB
  watts_beat = if_else(`Avg Heart Rate` > 90 ,
                       `Avg Watts` / `Avg Heart Rate`,
                       NA)) |>
  group_by(Description) |>
  mutate(n_rows = n()) |>
  ungroup()

#calculate personal best times up to each date
dat <- dat |>
  group_by(Description) |>
  arrange(Date, `Work Time (Seconds)`) |>  # Arrange the data by date and time
  mutate(cumulative_best_time = cummin(`Work Time (Seconds)` / 60),
         personal_best = case_when(`Work Time (Seconds)` / 60 == min(`Work Time (Seconds)` / 60) ~ "Personal Best",
                                   `Work Time (Seconds)` / 60 == cumulative_best_time ~ "Temporal Best",
                                   .default = "Other")) |>
  ungroup()

#-----
#Analyses using a dataset where the unit is the workout

#Save data for later use in the report
distances <- data.frame(
  cost_per_use = round(1440 / nrow(dat), 2),
  total_rowed = sum(dat$Distance, na.rm = T),
  weekly_dist = dat |>
    filter(Date > Sys.Date() - 7) |>
    summarize(sum(Distance)) |>
    pull(),
  monthly_dist = dat |>
    filter(month(Date) == month(Sys.Date()),
           year(Date) == year(Sys.Date())) |>
    summarize(sum(Distance)) |>
    pull()
)

write.csv(distances, here("data/distances.csv"), row.names = FALSE)

#-----
#graphs

#watts per beat is a good measure *only* if I adjust it for time and distance
#there is certainly a better way to do this, but I don't want to mess with Date
# m1 <- lm(watts_beat ~ poly(`Work Distance`, 2) + poly(`Work Time (Seconds)`, 2), data = dat)
# dat$HB_watt_res[!is.na(dat$watts_beat)] <- residuals(m1)
# 
# dat |>
#   filter(!is.na(HB_watt_res)) |>
#   mutate(HB_watt_res = HB_watt_res / sd(HB_watt_res)) |>
# ggplot(aes(Date, HB_watt_res)) +
#   geom_point(color = "grey", alpha = 0.8) +
#   geom_smooth(se = F, color = "black") +
#   scale_x_date(date_labels = "%b '%y") +
#   labs(x = "",
#        y = "Watts per HB (modeled, SD)")
# ggsave(here("figures/watts_beats.png"), height = 3, width = 3)

#time formatting function
format_time <- function(x) {
  hours <- floor(x)
  minutes <- round((x - hours) * 60)
  sprintf("%d:%02d", hours, minutes)
}

dat |>
  filter(Description == "2000m row") |>
  ggplot(aes(x = Date, y = `Work Time (Seconds)` / 60, color = personal_best)) +
  geom_point(alpha = 0.8) +
  scale_y_continuous(labels = format_time) +
  scale_x_date(date_labels = "%b '%y", date_breaks = "2 months") +
  scale_color_manual(values = c("grey", "red", "black")) +
  guides(color = "none") +
  labs(x = "",
       y = "2k Rowing Time")
ggsave(here("figures/2k.png"), height = 3, width = 3)

dat |>
  filter(Description == "5000m row") |>
  ggplot(aes(x = Date, y = `Work Time (Seconds)` / 60, color = personal_best)) +
  geom_point(alpha = 0.8) +
  scale_y_continuous(labels = format_time) +
  scale_x_date(date_labels = "%b '%y", date_breaks = "1 month") +
  scale_color_manual(values = c("grey", "red", "black")) +
  guides(color = "none") +
  labs(x = "",
       y = "5k Rowing Time")
ggsave(here("figures/5k.png"), height = 3, width = 3)

dat |>
  filter(Description == "6000m row") |>
  ggplot(aes(x = Date, y = `Work Time (Seconds)` / 60, color = personal_best)) +
  geom_point(alpha = 0.8) +
  scale_y_continuous(labels = format_time) +
  scale_x_date(date_labels = "%b '%y", date_breaks = "1 month") +
  scale_color_manual(values = c("grey", "red", "black")) +
  guides(color = "none") +
  labs(x = "",
       y = "6k Rowing Time")
ggsave(here("figures/6k.png"), height = 3, width = 3)

# dat |>
#   ggplot(aes(x = Date, y = Distance,
#              color = as.numeric(substr(Pace, 1, 2)) * 60 + as.numeric(substr(Pace, 4, 5)))) +
#   geom_point(size = 5) +
#   labs(color = "Pace")

#-----
#Analyses where the unit is the date

#merge workouts done on the same day
dat <- dat |>
  group_by(Date) |>
  summarise(`Total Cal` = sum(`Total Cal`),
            `Stroke Count` = sum(`Stroke Count`),
            `Work Time (Seconds)` = sum(`Work Time (Seconds)`), 
            #Avg Watts is weighted by distance rowed per workout
            `Avg Watts` = weighted.mean(`Avg Watts`, Distance),
            Distance = sum(Distance))

#weekly intensity graph
#make a grid of dates over the past 8 weeks
date_grid <- data.frame(Date = seq(as.Date(Sys.Date() - 55),
                                   as.Date(Sys.Date()),
                                   by = "days"))

#merge grid of dates with data, NAs are zero
date_grid <- left_join(date_grid, dat, by = "Date") |>
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .)))

#intensity of daily workouts over the past 2 weeks
date_grid |>
  filter(Date > Sys.Date() - 14) |>
  #speed
  mutate(Speed = if_else(`Work Time (Seconds)` == 0,
                         0,
                         Distance / `Work Time (Seconds)`)) |>
  #rescale all numeric columns
  mutate(across(where(is.numeric), function(x) x / max(x) * 100)) |>
  #Intensity is a function of distance, how hard you pulled, and your speed
  rowwise() |>
  mutate(intensity = mean(c(Distance, `Avg Watts`, Speed))) |>
  ungroup() |>
  #plot over date
  ggplot(aes(x = Date, y = intensity)) +
  geom_line() +
  scale_x_date(date_labels = "%e %b") +
  labs(x = "",
       y = "Two-Week Workout Intensity")
ggsave(here("figures/intensity.png"), height = 3, width = 3)


#looking back over weeks
date_grid |>
  mutate(week = cut(Date, 4, labels = seq(1, 4, 1))) |>
  group_by(week) |>
  mutate(ordered_dates = row_number(),
         intensity = cumsum(`Total Cal`)) |>
  ungroup() |>
  mutate(intensity = intensity - min(intensity),
         intensity = intensity / max(intensity) * 100) |>
  ggplot(aes(x = ordered_dates,
             y = intensity,
             group = week,
             color = week == 4)) +
  geom_line() +
  theme_bw()

date_grid |>
  mutate(week = cut(Date, 4, labels = seq(1, 4, 1))) |>
  group_by(week) |>
  mutate(ordered_dates = row_number(),
         Distance = cumsum(Distance)) |>
  ggplot(aes(x = ordered_dates,
             y = Distance,
             group = week,
             color = week == 4)) +
  geom_line() +
  theme_bw()

#-----
#cumulative distance figures
date_grid <- data.frame(Date = seq(min(dat$Date, na.rm = T),
                                   as.Date(Sys.Date()),
                                   by = "days"))

date_grid <- left_join(date_grid, dat, by = "Date") |>
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .)))

date_grid |>
  arrange(Date) |>
  mutate(cum_dist = cumsum(Distance) / 1000) |>
  ggplot(aes(x = Date, y = cum_dist)) +
  geom_line() +
  scale_x_date(date_labels = "%b '%y", date_breaks = "2 months") +
  labs(x = "",
       y = "Total Kilometres Rowed")
ggsave(here("figures/cum_dist.png"), height = 3, width = 3)

dat |>
  arrange(Date) |>
  mutate(percent_million = cumsum(Distance) / 1000000 * 100) |>
  ggplot(aes(x = Date, y = percent_million)) +
  geom_line() +
  scale_x_date(date_labels = "%b '%y", date_breaks = "2 months") +
  scale_y_continuous(limits = c(0,100),
                     labels = scales::unit_format(
                       unit = "%")) +
  labs(y = "",
       title = "How close am I to 1 million metres?")
ggsave(here("figures/dist_1m.png"), height = 4, width = 4)

