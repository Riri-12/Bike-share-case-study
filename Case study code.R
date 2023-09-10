#installing all required packages
install.packages("tidyverse")
install.packages("lubridate")
install.packages("janitor")
install.packages("ggplot2")
install.packages("dplyr")

#loading packages
library(tidyverse)
library(lubridate)
library(ggplot2)
library(janitor)
library(dplyr)

#Importing divvy datasets csv.files

Jan22 <- read.csv("202201-divvy-tripdata.csv")
Feb22 <- read.csv("202202-divvy-tripdata.csv")
Mar22 <- read.csv("202203-divvy-tripdata.csv")
Apr22 <- read.csv("202204-divvy-tripdata.csv")
May22 <- read.csv("202205-divvy-tripdata.csv")
Jun22 <- read.csv("202206-divvy-tripdata.csv")
Jul22 <- read.csv("202207-divvy-tripdata.csv")
Aug22 <- read.csv("202208-divvy-tripdata.csv")
Sep22 <- read.csv("202209-divvy-tripdata.csv")
Oct22 <- read.csv("202210-divvy-tripdata.csv")
Nov22 <- read.csv("202211-divvy-tripdata.csv")
Dec22 <- read.csv("202212-divvy-tripdata.csv")

#combining all data into one file
all_rides <- rbind(Jan22,Feb22,Mar22,Apr22,May22,Jun22,Jul22,Aug22,Sep22,Oct22,Nov22,Dec22)

#checking the structure of the new file

str(all_rides)
colnames(all_rides)
nrow(all_rides)
head(all_rides)
summary(all_rides)

#Converting started_at and ended_at from chr to dttm format

all_rides$start_time <- mdy_hm(all_rides$started_at)
all_rides$end_time <- mdy_hm(all_rides$ended_at)

#checking str for dttm format
str(all_rides)


#removing columns that will not be used for analysis

all_rides <- all_rides %>%
  select(-c(start_lat, start_lng, end_lat, end_lng))

#checking if the columns were removed
colnames(all_rides)

#checking number of observations under each type of user and ride

table(all_rides$member_casual)
table(all_rides$rideable_type)


#Calculate the number of rows in the data frame with unique ride_id values

length(unique(all_rides$ride_id))==nrow(all_rides)

all_rides_unique <- all_rides[!duplicated(all_rides$ride_id), ]
nrow(all_rides_unique)

options(max.print = .Machine$integer.max)



# Calculate the number of rows deleted
num_rows_deleted <- num_rows_original - num_rows_unique

#checking the file for NA,NULL value,empty columns and remove them

colSums(is.na(all_rides_unique))


#removing empty and missing data

all_rides_clean <- all_rides_unique
all_rides_clean <- all_rides_clean %>% filter(!(start_station_id=="" | start_station_name == "" | end_station_id=="" | end_station_name==""))

options(max.print = .Machine$integer.max)


#checking number of rows removed

cat("number of rows removed: ", nrow(all_rides_unique) - nrow(all_rides_clean))


# Add date, month, day, year, and day of week

all_rides_clean$date <- as.Date(all_rides_clean$start_time)
all_rides_clean$month <- format(as.Date(all_rides_clean$date), "%m")
all_rides_clean$day <- format(as.Date(all_rides_clean$date), "%d")
all_rides_clean$year <- format(as.Date(all_rides_clean$date), "%Y")
all_rides_clean$day_of_week <- format(as.Date(all_rides_clean$date), "%A")

#adding ride length column

all_rides_clean$ride_length <- difftime(all_rides_clean$end_time, all_rides_clean$start_time, units = "secs")
 
str(all_rides_clean)
head(all_rides_clean)

#checking the ride_length < 0
 all_rides_clean %>%
   filter(ride_length <0)

#removing negative values as it might affect the analysis
 str(all_rides_clean)
 
 all_rides_clean <- all_rides_clean[ all_rides_clean$ride_length > 0, ]
 
 head(all_rides_clean)
 all_rides_clean$ride_length
 
 #descriptive analysis on ride length
 
 mean(all_rides_clean$ride_length)
 median(all_rides_clean$ride_length)
 max(all_rides_clean$ride_length)
 min(all_rides_clean$ride_length)
 
 #condensed the four lines into one line using summary() on the specific attribute
 
 summary(all_rides_clean$ride_length)


 #comparing members and casual users
 
 aggregate(all_rides_clean$ride_length ~ all_rides_clean$member_casual, FUN = mean)
 aggregate(all_rides_clean$ride_length ~ all_rides_clean$member_casual, FUN = median)
 aggregate(all_rides_clean$ride_length ~ all_rides_clean$member_casual, FUN = max)
 aggregate(all_rides_clean$ride_length ~ all_rides_clean$member_casual, FUN = min)


 
#Average ride time by each day for members vs. casual users
 
 aggregate (all_rides_clean$ride_length ~ all_rides_clean$member_casual + all_rides_clean$day_of_week, FUN = mean)
 
 
#fixing the days of the week in order
 
 all_rides_clean$day_of_week <- ordered(all_rides_clean$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))


#Running the Average ride time by each day for members vs. casual again after fixing the days of the week order
 aggregate (all_rides_clean$ride_length ~ all_rides_clean$member_casual + all_rides_clean$day_of_week, FUN = mean)
 
 
 #Analyzing ridership data by type and day of the week
 all_rides_clean%>%  
   group_by(member_casual, day_of_week) %>%
   summarise(number_of_rides = n(),
             average_ride_length = mean(ride_length)) %>% 
   arrange(member_casual, day_of_week)
 
 
#Removing scientific notation from the plot to get entire number dispalyed on the ggplot
 options(scipen=999)

#Visualizing the data
# Number of rides by rider type (member vs.casual) in a week
 all_rides_clean %>%
 group_by(member_casual, day_of_week) %>%
   summarise(number_of_rides = n(), 
   average_ride_length = mean(ride_length))%>%
   arrange(member_casual, day_of_week) %>% 
   ggplot(aes(x = day_of_week, y = number_of_rides, fill = member_casual)) + 
   geom_col(position = "dodge") + 
   labs(title="Total Number of Rides by Member vs. Casual in A Week", x = "Day of Week", y = "number of rides") + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
 
 
#Average ride length by rider type (Member vs. Casual)in a week
 
 all_rides_clean %>%
   group_by(member_casual, day_of_week) %>%
   summarise(number_of_rides = n(), 
             average_ride_length = mean(ride_length))%>%
   arrange(member_casual, day_of_week) %>% 
   ggplot(aes(x = day_of_week, y = average_ride_length, fill = member_casual)) + 
   geom_col(position = "dodge") + 
   labs(title="Average ride length by Member vs. Casual in A week", x = "Day of Week", y = "average ride length") + 
   theme(axis.text.x = element_text(angle = 60, hjust = 1))
 
#Total Number of rides by Ride type

 all_rides_clean %>%
   group_by(member_casual, rideable_type) %>%
   summarise(number_of_rides = n(), 
             average_ride_length = mean(ride_length))%>%
   arrange(member_casual, rideable_type) %>% 
   ggplot(aes(x = rideable_type, number_of_rides, fill = member_casual)) + 
   geom_col(position = "dodge") + 
   labs(title="Total Number of rides by Ride Types", x = "Ride Type", y = "Number of Rides") + 
   theme(axis.text.x = element_text(angle = 60, hjust = 1))
 
#Average ride time by Month
 all_rides_clean %>%
   group_by(member_casual, month) %>%
   summarise(number_of_rides = n(), 
             average_ride_length = mean(ride_length))%>%
   arrange(member_casual, month) %>% 
   ggplot(aes(x = month, y = average_ride_length, fill = member_casual)) + 
   geom_col(position = "dodge") + 
   labs(title="Average ride time by Month", x = "Month", y = "average ride length") + 
   theme(axis.text.x = element_text(angle = 60, hjust = 1))

#Number of rides by month
 all_rides_clean %>%
   group_by(member_casual, month) %>%
   summarise(number_of_rides = n(), 
             average_ride_length = mean(ride_length))%>%
   arrange(member_casual, month) %>% 
   ggplot(aes(x = month, y = number_of_rides, fill = member_casual)) + 
   geom_col(position = "dodge") + 
   labs(title="Number of Rides by Month", x = "Month", y = "Number of Rides") + 
   theme(axis.text.x = element_text(angle = 60, hjust = 1))
 

 
 
 
 
 
 
 
