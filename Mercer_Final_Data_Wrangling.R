# Final Project Data Wrangling
# (a modification of "Data_Wrangling_Snapshot_2019")
# Margaret Mercer
# April 18, 2024

# In this file, I will be using deployment (one row for each camera trap set) and observation (one row for each photograph) data for the year 2019 and merging them together. I will create a "Is_Night" column with binary coding.
# Week 8: Solving Bigger Problems

library(tidyverse)
library(suncalc)
library(lutz)
library(doParallel)
library(foreach) # Week 2: Intro to R

# load deployment and observation data
obs_2019 <- read_csv("data/SNAPSHOT_USA_2019_observations.csv") # Week 9: Reproducibility
dep_2019 <- read_csv("data/SNAPSHOT_USA_2019_deployments.csv")

# merge deployment and observation data
left_joined_19 <- left_join(obs_2019, dep_2019, by = "Site_Name") # Week 4: Joins

# get rid of duplicate columns leftover from merge
all_2019 <- left_joined_19[, c("Camera_Trap_Array.x",
                               "Site_Name",
                               "Survey_Days",
                               "Latitude.x",
                               "Longitude.x",
                               "Begin_Time",
                               "Species_Name",
                               "Common_Name",
                               "Count")]

# rename date/time column to LOCAL date/time
colnames(all_2019)[6] <- "Local_Date_Time"

# look up time zones and associate them with each camera
all_2019$Time_Zone <- tz_lookup_coords(all_2019$Latitude.x, all_2019$Longitude.x, method = "accurate", warn = TRUE)

# fix date format
all_2019$Local_Date_Time <- as.POSIXct(strptime(all_2019$Local_Date_Time, format = "%m/%d/%y %H:%M")) # Week 7: Dates and Times

# many of the times are coming out as "NAs". Lines 41 through 68 are to fix this issue.
all_2019 <- separate(all_2019, Local_Date_Time, c("Date", "Time"), sep = " ") # first we separate dates and times
all_2019$Time[is.na(all_2019$Time)] <- "00:00:01" # now replace all NAs with "00:00:01"
sum(is.na(all_2019$Time)) # check for NAs

# it doesn't like the following exact hours, so I'm changing the times and making them one second past the hour x_x
all_2019 <- all_2019 %>%
  mutate(Time = if_else(Time == "14:00:00", "14:00:01", Time),
         Time = if_else(Time == "15:00:00", "15:00:01", Time),
         Time = if_else(Time == "16:00:00", "16:00:01", Time),
         Time = if_else(Time == "17:00:00", "17:00:01", Time),
         Time = if_else(Time == "18:00:00", "18:00:01", Time),
         Time = if_else(Time == "19:00:00", "19:00:01", Time),
         Time = if_else(Time == "20:00:00", "20:00:01", Time)) # Week 3: Data Tables

# unite dates and times and convert to POSIXct format
all_2019 <- unite(all_2019, Local_Date_Time, c("Date", "Time"), sep = " ") # Week 6: Tidy Data
all_2019$Local_Date_Time <- as.POSIXct(all_2019$Local_Date_Time)

# fix formatting of date times
ncores <- parallel::detectCores() - 2
doParallel::registerDoParallel(ncores)
all_2019$time_utc <- foreach::foreach(i = 1:length(all_2019$Site_Name), .combine = c) %dopar% {
  x <- ymd_hms(all_2019$Local_Date_Time[i], tz = all_2019$Time_Zone[i]) # Week 7: Dates and Times
  x <- with_tz(x, tzone = "UTC")
  as.character(x)
} # Week 12: Iteration (kind of)
doParallel::stopImplicitCluster() # Week 14: Parallel Computing

# make sure there are no NAs
sum(is.na(all_2019$time_utc))

# rename columns so suncalc can recognize them
colnames(all_2019)[4] <- "lat"
colnames(all_2019)[5] <- "lon"
colnames(all_2019)[11] <- "date"

# get altitudes of each camera observation
sun_position <- getSunlightPosition(
  data = all_2019,
  keep = c("altitude")
)

all_2019['Altitude'] = sun_position$altitude

# rename columns so they look nice
colnames(all_2019)[1] <- "Array"
colnames(all_2019)[2] <- "Site_Name"
colnames(all_2019)[4] <- "Latitude"
colnames(all_2019)[5] <- "Longitude"
colnames(all_2019)[6] <- "Local_Date_Time"
colnames(all_2019)[11] <- "UTC_Date_Time"


# code day/night binary. in this column, we want a 1 if altitude is negative (sun is below horizon), and a 0 if it's positive (sun is above horizon)
all_2019$Is_Night <- if_else(all_2019$Altitude < 0, 1, 0) # Week 11: Making Choices

# make a graph of times to be sure that visually, the times look correct and unified
data <- separate(all_2019, Local_Date_Time, c("Local_Date", "Local_Time"), sep = " ") # Week 6: Tidy Data
ggplot(data, aes(x = Local_Time, y = Altitude)) +
  geom_point() # Week 5: Data Visualization

# all_2019 now has deployment and observation data, and a day/night column
