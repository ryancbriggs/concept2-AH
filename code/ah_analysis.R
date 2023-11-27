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

lookback <- months(36)
rowing_start <- as.Date("2023/02/26")

#looking up to 1 year back, linear models
# Create a binary variable for the side of the threshold
dat$ThresholdSide <- ifelse(dat$date <= rowing_start, "Left", "Right")

#Resting heart rate
left_side <- lm(value ~ date,
                data = dat |>
                  subset(ThresholdSide == "Left") |>
                  filter(type == "HKQuantityTypeIdentifierRestingHeartRate",
                         endDate > Sys.Date() - lookback) |>
                  group_by(date) |>
                  summarize(value = mean(value)))

right_intercept <- left_side$coefficients[1] + 
  left_side$coefficients[2] * as.numeric(rowing_start)

right_side <- lm(I(value - right_intercept) ~ 0 + I(date - rowing_start), 
                 data = dat |>
                   subset(ThresholdSide == "Right") |>
                   filter(type == "HKQuantityTypeIdentifierRestingHeartRate",
                          endDate >= Sys.Date() - lookback) |>
                   group_by(date) |>
                   summarize(value = mean(value)))

predict_right <- right_intercept + predict(right_side) 

#graph
dat |>
  filter(type == "HKQuantityTypeIdentifierRestingHeartRate",
         endDate > Sys.Date() - lookback) |>
  group_by(date) |>
  summarize(value = mean(value)) |>
  #remove most extreme obs
  arrange(value) |>
  slice(-c(1, n())) |>
  ggplot(aes(x = date, y = value)) +
  geom_point(color = "darkgrey", alpha = 0.3, shape = 16) +
  geom_smooth(
    data = subset(dat, ThresholdSide == "Left") |>
    filter(type == "HKQuantityTypeIdentifierRestingHeartRate",
           endDate > Sys.Date() - lookback) |>
      group_by(date) |>
      summarize(value = mean(value)) |>
      #remove most extreme obs
      arrange(value) |>
      slice(-c(1, n())),
    method = "lm",
    se = FALSE,
    color = "black") +
  geom_function(fun=Vectorize(function(x) {
    if(x >= as.numeric(rowing_start))
      return(left_side$coefficients[1] +
               left_side$coefficients[2] * as.numeric(rowing_start) -
               right_side$coefficients[1] * as.numeric(rowing_start) + 
               as.numeric(x) * right_side$coefficients[1])
    else
      return(NA)
  }), size = 1, color = "black") +
  #geom_smooth(se = F, color = "black",
  #            method = 'lm', formula = y ~ poly(x, 3)) +
  geom_vline(xintercept = rowing_start, color = "red") +
  scale_x_date(date_labels = "%b '%y") +
  labs(x = "",
       y = "Average Resting Heart Rate")
ggsave(here("figures/resting_heart.png"), height = 3, width = 3)


#VO2 Max
left_side <- lm(value ~ date,
                data = dat |>
                  subset(ThresholdSide == "Left") |>
                  filter(type == "HKQuantityTypeIdentifierVO2Max",
                         endDate > Sys.Date() - lookback) |>
                  group_by(date) |>
                  summarize(value = mean(value)))

right_intercept <- left_side$coefficients[1] + 
  left_side$coefficients[2] * as.numeric(rowing_start)

right_side <- lm(I(value - right_intercept) ~ 0 + I(date - rowing_start), 
                 data = dat |>
                   subset(ThresholdSide == "Right") |>
                   filter(type == "HKQuantityTypeIdentifierVO2Max",
                          endDate >= Sys.Date() - lookback) |>
                   group_by(date) |>
                   summarize(value = mean(value)))

predict_right <- right_intercept + predict(right_side) 

#graph
dat |>
  filter(type == "HKQuantityTypeIdentifierVO2Max",
         date > Sys.Date() - lookback) |>
  group_by(date) |>
  summarize(value = mean(value)) |>
  #remove most extreme obs
  arrange(value) |>
  slice(-c(1, n())) |>
  ggplot(aes(x = date, y = value)) +
  geom_point(color = "darkgrey", alpha = 0.3, shape = 16) +
  geom_smooth(
    data = subset(dat, ThresholdSide == "Left") |>
      filter(type == "HKQuantityTypeIdentifierVO2Max",
             endDate > Sys.Date() - lookback) |>
      group_by(date) |>
      summarize(value = mean(value)) |>
      #remove most extreme obs
      arrange(value) |>
      slice(-c(1, n())),
    method = "lm",
    se = FALSE,
    color = "black") +
  geom_function(fun=Vectorize(function(x) {
    if(x >= as.numeric(rowing_start))
      return(left_side$coefficients[1] +
               left_side$coefficients[2] * as.numeric(rowing_start) -
               right_side$coefficients[1] * as.numeric(rowing_start) + 
               as.numeric(x) * right_side$coefficients[1])
    else
      return(NA)
  }), size = 1, color = "black") +
  geom_vline(xintercept = rowing_start, color = "red") +
  scale_x_date(date_labels = "%b '%y") +
  labs(x = "",
       y = expression(paste("Estimated ", VO[2], " Max", sep = "")))
ggsave(here("figures/VO2max.png"), height = 3, width = 3)

#heart rate variability
left_side <- lm(value ~ date,
                data = dat |>
                  subset(ThresholdSide == "Left") |>
                  filter(type == "HKQuantityTypeIdentifierHeartRateVariabilitySDNN",
                         endDate > Sys.Date() - lookback) |>
                  group_by(date) |>
                  summarize(value = mean(value)))

right_intercept <- left_side$coefficients[1] + 
  left_side$coefficients[2] * as.numeric(rowing_start)

right_side <- lm(I(value - right_intercept) ~ 0 + I(date - rowing_start), 
                 data = dat |>
                   subset(ThresholdSide == "Right") |>
                   filter(type == "HKQuantityTypeIdentifierHeartRateVariabilitySDNN",
                          endDate >= Sys.Date() - lookback) |>
                   group_by(date) |>
                   summarize(value = mean(value)))

predict_right <- right_intercept + predict(right_side) 

dat |>
  filter(type == "HKQuantityTypeIdentifierHeartRateVariabilitySDNN",
         date > Sys.Date() - lookback) |>
  group_by(date) |>
  summarize(value = mean(value)) |>
  #remove most extreme obs
  arrange(value) |>
  slice(-c(1, n())) |>
  ggplot(aes(x = date, y = value)) +
  geom_point(color = "darkgrey", alpha = 0.3, shape = 16) +
  geom_smooth(
    data = subset(dat, ThresholdSide == "Left") |>
      filter(type == "HKQuantityTypeIdentifierHeartRateVariabilitySDNN",
             endDate > Sys.Date() - lookback) |>
      group_by(date) |>
      summarize(value = mean(value)) |>
      #remove most extreme obs
      arrange(value) |>
      slice(-c(1, n())),
    method = "lm",
    se = FALSE,
    color = "black") +
  geom_function(fun=Vectorize(function(x) {
    if(x >= as.numeric(rowing_start))
      return(left_side$coefficients[1] +
               left_side$coefficients[2] * as.numeric(rowing_start) -
               right_side$coefficients[1] * as.numeric(rowing_start) + 
               as.numeric(x) * right_side$coefficients[1])
    else
      return(NA)
  }), size = 1, color = "black") +
  geom_vline(xintercept = rowing_start, color = "red") +
  scale_x_date(date_labels = "%b '%y") +
  labs(x = "",
       y = expression(paste("Average Heart Rate Variability", sep = "")))
ggsave(here("figures/heart_variability.png"), height = 3, width = 3)

#heart rate recovery
dat |>
  filter(type == "HKQuantityTypeIdentifierHeartRateRecoveryOneMinute",
         date > Sys.Date() - lookback) |>
  group_by(date) |>
  summarize(value = mean(value)) |>
  #remove most extreme obs
  arrange(value) |>
  slice(-c(1, n())) |>
  ggplot(aes(x = date, y = value)) +
  geom_point(color = "darkgrey", alpha = 0.3, shape = 16) +
  #geom_smooth(se = F, color = "black") +
  geom_vline(xintercept = rowing_start, color = "red") +
  scale_x_date(date_labels = "%b '%y") +
  labs(x = "",
       y = "1 Minute Heart Rate Recovery")
ggsave(here("figures/1min_HR_recovery.png"), height = 3, width = 3)

#exercise time
left_side <- lm(value ~ date,
                data = dat |>
                  subset(ThresholdSide == "Left") |>
                  filter(type == "HKQuantityTypeIdentifierAppleExerciseTime",
                         endDate > Sys.Date() - lookback) |>
                  group_by(date) |>
                  summarize(value = sum(value) / 60))

right_intercept <- left_side$coefficients[1] + 
  left_side$coefficients[2] * as.numeric(rowing_start)

right_side <- lm(I(value - right_intercept) ~ 0 + I(date - rowing_start), 
                 data = dat |>
                   subset(ThresholdSide == "Right") |>
                   filter(type == "HKQuantityTypeIdentifierAppleExerciseTime",
                          endDate >= Sys.Date() - lookback) |>
                   group_by(date) |>
                   summarize(value = sum(value)  / 60))

predict_right <- right_intercept + predict(right_side) 

#graph
dat |>
  filter(type == "HKQuantityTypeIdentifierAppleExerciseTime",
         sourceName != "Ryan’s Apple Watch",
         date > Sys.Date() - lookback) |>
  group_by(date) |>
  summarize(value = sum(value) / 60) |>
  #remove most extreme obs
  arrange(value) |>
  slice(-c(1, n())) |>
  ggplot(aes(x = date, y = value)) +
  geom_point(color = "darkgrey", alpha = 0.3, shape = 16) +
  geom_smooth(
    data = subset(dat, ThresholdSide == "Left") |>
      filter(type == "HKQuantityTypeIdentifierAppleExerciseTime",
             endDate > Sys.Date() - lookback) |>
      group_by(date) |>
      summarize(value = sum(value) / 60) |>
      #remove most extreme obs
      arrange(value) |>
      slice(-c(1, n())),
    method = "lm",
    se = FALSE,
    color = "black") +
  geom_function(fun=Vectorize(function(x) {
    if(x >= as.numeric(rowing_start))
      return(left_side$coefficients[1] +
               left_side$coefficients[2] * as.numeric(rowing_start) -
               right_side$coefficients[1] * as.numeric(rowing_start) + 
               as.numeric(x) * right_side$coefficients[1])
    else
      return(NA)
  }), size = 1, color = "black") +
  geom_vline(xintercept = rowing_start, color = "red") +
  scale_x_date(date_labels = "%b '%y") +
  labs(x = "",
       y = "Hours of Exercise Time") 
ggsave(here("figures/Exercise_time.png"), height = 3, width = 3)

#active energy burned
left_side <- lm(value ~ date,
                data = dat |>
                  subset(ThresholdSide == "Left") |>
                  filter(type == "HKQuantityTypeIdentifierActiveEnergyBurned",
                         endDate > Sys.Date() - lookback) |>
                  group_by(date) |>
                  summarize(value = sum(value)))

right_intercept <- left_side$coefficients[1] + 
  left_side$coefficients[2] * as.numeric(rowing_start)

right_side <- lm(I(value - right_intercept) ~ 0 + I(date - rowing_start), 
                 data = dat |>
                   subset(ThresholdSide == "Right") |>
                   filter(type == "HKQuantityTypeIdentifierActiveEnergyBurned",
                          endDate >= Sys.Date() - lookback) |>
                   group_by(date) |>
                   summarize(value = sum(value)))

predict_right <- right_intercept + predict(right_side) 

#graph
dat |>
  filter(type == "HKQuantityTypeIdentifierActiveEnergyBurned",
         sourceName != "Ryan’s Apple Watch",
         date > Sys.Date() - lookback) |>
  group_by(date) |>
  summarize(value = sum(value)) |>
  #remove most extreme obs
  arrange(value) |>
  slice(-c(1, n())) |>
  ggplot(aes(x = date, y = value)) +
  geom_point(color = "darkgrey", alpha = 0.3, shape = 16) +
  geom_smooth(
    data = subset(dat, ThresholdSide == "Left") |>
      filter(type == "HKQuantityTypeIdentifierActiveEnergyBurned",
             endDate > Sys.Date() - lookback) |>
      group_by(date) |>
      summarize(value = sum(value)) |>
      #remove most extreme obs
      arrange(value) |>
      slice(-c(1, n())),
    method = "lm",
    se = FALSE,
    color = "black") +
  geom_function(fun=Vectorize(function(x) {
    if(x >= as.numeric(rowing_start))
      return(left_side$coefficients[1] +
               left_side$coefficients[2] * as.numeric(rowing_start) -
               right_side$coefficients[1] * as.numeric(rowing_start) + 
               as.numeric(x) * right_side$coefficients[1])
    else
      return(NA)
  }), size = 1, color = "black") +
  
  geom_vline(xintercept = rowing_start, color = "red") +
  scale_x_date(date_labels = "%b '%y") +
  labs(x = "",
       y = "Daily Active Calories Burned")
ggsave(here("figures/Active_calories_burned.png"), height = 3, width = 3)
