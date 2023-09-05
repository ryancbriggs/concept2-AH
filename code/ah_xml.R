here::i_am("code/ah_xml.R")
source(here::here("code/load.R"))

#load data
#unzip compressed apple health data
unzip(zipfile = here("data/export.zip"),
      exdir = here("data/"))

#The xml has invalid character thanks to 1 error due to a volume alert on cheap headphones
#this removes that error
xml_lines <- readLines(here("data/apple_health_export/export.xml"))
#find the index(es) of the line(s) containing "Tobii"
lines_with_tobii <- grep("Tobii", xml_lines)
#remove the line(s) containing "Tobii"
xml_lines <- xml_lines[-lines_with_tobii]
#write lines back
writeLines(xml_lines, here("data/apple_health_export/export.xml"))
#remove big file
rm(xml_lines)

#read xml file
xml <- xmlParse(here("data/apple_health_export/export.xml"))

#delete unzipped data
unlink(here("data/apple_health_export/"), recursive = TRUE) 

#transform xml file to data frame - select the Record rows from the xml file
dat <- XML:::xmlAttrsToDataFrame(xml["//Record"])

#make value numeric
dat$value <- as.numeric(as.character(dat$value))

#fix dates
dat$endDate <- ymd_hms(dat$endDate, tz="America/New_York")

#add year, month, date, dayofweek, hour, columns
dat$year <- format(dat$endDate,"%Y")
dat$month <- format(dat$endDate,"%m")
dat$week <- format(dat$endDate,"%W")
dat$date <- format(dat$endDate,"%Y-%m-%d") |> as.Date()
dat$time <- format(dat$endDate,"%H:%M:%S")
dat$hour <- format(dat$endDate,"%H") |> as.numeric()
dat$minute <- format(dat$endDate,"%M") |> as.numeric()
dat$second <- format(dat$endDate,"%S") |> as.numeric()
dat$dayofweek <- wday(dat$endDate, label = TRUE, abbr = FALSE)

#-----
#Data cleaning
#ErgData is polluting my heart rate data with duplicates
#remove ErgData heart rate obs
dat <- dat |>
  filter(!(sourceName == "ErgData" & type == "HKQuantityTypeIdentifierHeartRate"))

#add a marker for days you rowed
dat <- dat |>
  group_by(date) |>
  #identify days you rowed
  mutate(did_rowing = ifelse(any(grepl("HKQuantityTypeIdentifierDistanceWalkingRunning ErgData", paste(type, sourceName))), 1, 0))

#-----
#Save data
write_feather(dat, here("data/ah.feather"))