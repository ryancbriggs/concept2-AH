here::i_am("code/ah_analysis.R")
source(here::here("code/load.R"))

#load data
dat <- read_feather(here("data/ah.feather"))

#unique(dat$type)
#test <- dat |>
#  filter(type == "HKQuantityTypeIdentifierHeartRateRecoveryOneMinute")

#------
#nightmare trying to calculate HRR after rowing
# test <- dat |>
#   filter(date == Sys.Date())
# 
# rowing_spells <- dat |>
#   #filter to rowing distance data
#   filter(type == "HKQuantityTypeIdentifierDistanceWalkingRunning",
#          sourceName == "ErgData") |>
#   #grab the end of each spell per day (will miss days I row more than once)
#   group_by(date) |>
#   mutate(end_of_spell = if_else(endDate == max(endDate), 1, 0)) |>
#   #filter to the end of each spells
#   filter(end_of_spell == 1) |>
#   #calculate start and end of 1 min HRR time
#   mutate(HRR_start_time = endDate,
#          HRR_end_time = endDate + 60)
# 
# HRR_dat <- dat |>
#   filter(did_rowing == 1,
#          type == "HKQuantityTypeIdentifierHeartRate")
# 
# map(unique(HRR_dat$date),
#     function(x) HRR_dat |>
#       filter(date == x,
#              endDate > rowing_spells$HRR_start_time[rowing_spells$date == x],
#              endDate < rowing_spells$HRR_end_time[rowing_spells$date == x]) |>
#       summarize(HRR = max(value) - min(value)) |>
#       filter(HRR != Inf,
#              HRR != -Inf)
#     )

unique(dat$type)

## Graphs

#looking up to 1 year back, linear models
#Resting heart rate
dat |>
  filter(type == "HKQuantityTypeIdentifierRestingHeartRate",
         endDate > Sys.Date() - months(24)) |>
  group_by(date) |>
  summarize(value = mean(value)) |>
  #remove most extreme obs
  arrange(value) |>
  slice(-c(1, n())) |>
  ggplot(aes(x = date, y = value)) +
  geom_point(color = "grey", alpha = 0.8) +
  geom_smooth(se = F, color = "black") +
  geom_vline(xintercept = as.Date("2023/02/26"), color = "red") +
  scale_x_date(date_labels = "%b '%y") +
  labs(x = "",
       y = "Average Resting Heart Rate")
ggsave(here("figures/resting_heart.png"), height = 3, width = 3)

#VO2 Max
dat |>
  filter(type == "HKQuantityTypeIdentifierVO2Max",
         date > Sys.Date() - months(24)) |>
  group_by(date) |>
  summarize(value = mean(value)) |>
  #remove most extreme obs
  arrange(value) |>
  slice(-c(1, n())) |>
  ggplot(aes(x = date, y = value)) +
  geom_point(color = "grey", alpha = 0.8) +
  geom_smooth(se = F, color = "black") +
  geom_vline(xintercept = as.Date("2023/02/26"), color = "red") +
  scale_x_date(date_labels = "%b '%y") +
  labs(x = "",
       y = expression(paste("Estimated ", VO[2], " Max", sep = "")))
ggsave(here("figures/VO2max.png"), height = 3, width = 3)

#heart rate variability
dat |>
  filter(type == "HKQuantityTypeIdentifierHeartRateVariabilitySDNN",
         date > Sys.Date() - months(24)) |>
  group_by(date) |>
  summarize(value = mean(value)) |>
  #remove most extreme obs
  arrange(value) |>
  slice(-c(1, n())) |>
  ggplot(aes(x = date, y = value)) +
  geom_point(color = "grey", alpha = 0.8) +
  geom_smooth(se = F, color = "black") +
  geom_vline(xintercept = as.Date("2023/02/26"), color = "red") +
  scale_x_date(date_labels = "%b '%y") +
  labs(x = "",
       y = expression(paste("Average Heart Rate Variability", sep = "")))
ggsave(here("figures/heart_variability.png"), height = 3, width = 3)

#heart rate recovery
dat |>
  filter(type == "HKQuantityTypeIdentifierHeartRateRecoveryOneMinute",
         date > Sys.Date() - months(24)) |>
  group_by(date) |>
  summarize(value = mean(value)) |>
  #remove most extreme obs
  arrange(value) |>
  slice(-c(1, n())) |>
  ggplot(aes(x = date, y = value)) +
  geom_point(color = "grey", alpha = 0.8) +
  #geom_smooth(se = F, color = "black") +
  geom_vline(xintercept = as.Date("2023/02/26"), color = "red") +
  scale_x_date(date_labels = "%b '%y") +
  labs(x = "",
       y = "1 Minute Heart Rate Recovery")
ggsave(here("figures/1min_HR_recovery.png"), height = 3, width = 3)

#exercise time
dat |>
  filter(type == "HKQuantityTypeIdentifierAppleExerciseTime",
         sourceName != "Ryan’s Apple Watch",
         date > Sys.Date() - months(24)) |>
  group_by(date) |>
  summarize(value = sum(value) / 60) |>
  #remove most extreme obs
  arrange(value) |>
  slice(-c(1, n())) |>
  ggplot(aes(x = date, y = value)) +
  geom_point(color = "grey", alpha = 0.8) +
  geom_smooth(se = F, color = "black") +
  geom_vline(xintercept = as.Date("2023/02/26"), color = "red") +
  scale_x_date(date_labels = "%b '%y") +
  labs(x = "",
       y = "Hours of Exercise Time") 
ggsave(here("figures/Exercise_time.png"), height = 3, width = 3)

#active energy burned
dat |>
  filter(type == "HKQuantityTypeIdentifierActiveEnergyBurned",
         sourceName != "Ryan’s Apple Watch",
         date > Sys.Date() - months(24)) |>
  group_by(date) |>
  summarize(value = sum(value)) |>
  #remove most extreme obs
  arrange(value) |>
  slice(-c(1, n())) |>
  ggplot(aes(x = date, y = value)) +
  geom_point(color = "grey", alpha = 0.8) +
  geom_smooth(se = F, color = "black") +
  geom_vline(xintercept = as.Date("2023/02/26"), color = "red") +
  scale_x_date(date_labels = "%b '%y") +
  labs(x = "",
       y = "Daily Active Calories Burned")
ggsave(here("figures/Active_calories_burned.png"), height = 3, width = 3)

