# # # # # # # # # # # # # # # # # # # # # # # 
# Install required packages
# tidyverse for data import and wrangling
# lubridate for date functions
# ggplot for visualization
# # # # # # # # # # # # # # # # # # # # # # #  

## STEP 1: COLLECT DATA

install.packages("tidyverse")
library(lubridate)
library(ggplot2)
library(readxl)

# Upload Divvy datasets (xlsx files) here

X202210_divvy_tripdata <- read_excel("C:/Users/maxla/Downloads/202210-divvy-tripdata.xlsx")
X202211_divvy_tripdata <- read_excel("C:/Users/maxla/Downloads/202211-divvy-tripdata.xlsx")
X202212_divvy_tripdata <- read_excel("C:/Users/maxla/Downloads/202212-divvy-tripdata.xlsx")
X202301_divvy_tripdata <- read_excel("C:/Users/maxla/Downloads/202301-divvy-tripdata.xlsx")
X202209_divvy_publictripdata <- read_excel("C:/Users/maxla/Downloads/202209-divvy-publictripdata.xlsx")
X202208_divvy_tripdata <- read_excel("C:/Users/maxla/Downloads/202208-divvy-tripdata.xlsx")
X202206_divvy_tripdata <- read_excel("C:/Users/maxla/Downloads/202206-divvy-tripdata.xlsx")
X202205_divvy_tripdata <- read_excel("C:/Users/maxla/Downloads/202205-divvy-tripdata.xlsx")
X202204_divvy_tripdata <- read_excel("C:/Users/maxla/Downloads/202204-divvy-tripdata.xlsx")
X202203_divvy_tripdata <- read_excel("C:/Users/maxla/Downloads/202203-divvy-tripdata.xlsx")
X202202_divvy_tripdata <- read_excel("C:/Users/maxla/Downloads/202202-divvy-tripdata.xlsx")
X202201_divvy_tripdata <- read_excel("C:/Users/maxla/Downloads/202201-divvy-tripdata.xlsx")
X202112_divvy_tripdata <- read_excel("C:/Users/maxla/Downloads/202112-divvy-tripdata.xlsx")
X202107_divvy_tripdata <- read_excel("C:/Users/maxla/Downloads/202107-divvy-tripdata.xlsx")

## STEP 2: WRANGLE DATA AND COMBINE INTO A SINGLE FILE

# Compare column names each of the files
# While the names don't have to be in the same order, they DO need to match perfectly before 

colnames(X202107_divvy_tripdata)
colnames(X202112_divvy_tripdata)
colnames(X202201_divvy_tripdata)
colnames(X202202_divvy_tripdata)
colnames(X202203_divvy_tripdata)
colnames(X202204_divvy_tripdata)
colnames(X202205_divvy_tripdata)
colnames(X202206_divvy_tripdata)
colnames(X202208_divvy_tripdata)
colnames(X202209_divvy_publictripdata)
colnames(X202301_divvy_tripdata)
colnames(X202212_divvy_tripdata)
colnames(X202211_divvy_tripdata)
colnames(X202210_divvy_tripdata)

# Inspect the dataframes and look for incongruencies

str(X202107_divvy_tripdata)
str(X202112_divvy_tripdata)
str(X202201_divvy_tripdata)
str(X202202_divvy_tripdata)
str(X202203_divvy_tripdata)
str(X202204_divvy_tripdata)
str(X202205_divvy_tripdata)
str(X202206_divvy_tripdata)
str(X202208_divvy_tripdata)
str(X202209_divvy_publictripdata)
str(X202301_divvy_tripdata)
str(X202212_divvy_tripdata)
str(X202211_divvy_tripdata)
str(X202210_divvy_tripdata)

#We noticed end_station_id in X202209_divvy_publictripdata is formated as number while others are characters
#We need to also format it as characters

X202209_divvy_publictripdata<-mutate(X202209_divvy_publictripdata,end_station_id=as.character(end_station_id))

#Stack individual quarter's data frames into one big data frame

all_trips<-bind_rows(X202210_divvy_tripdata,X202211_divvy_tripdata,X202212_divvy_tripdata,X202107_divvy_tripdata,X202112_divvy_tripdata,X202201_divvy_tripdata,X202202_divvy_tripdata,X202203_divvy_tripdata,X202204_divvy_tripdata,X202205_divvy_tripdata,X202206_divvy_tripdata,X202208_divvy_tripdata,X202209_divvy_publictripdata,X202301_divvy_tripdata)

#We now have 6,104,480 rows and 15 columns

#Remove lat, long columns

all_trips<-all_trips%>%
  select(-c(start_lat,start_lng,end_lat,end_lng))

#We've reduced the columns to 11

### STEP 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS

# Inspect the new table that has been created
colnames(all_trips)  #List of column names
nrow(all_trips)  #How many rows are in data frame?
dim(all_trips)  #Dimensions of the data frame?
head(all_trips)  #See the first 6 rows of data frame.  Also tail(all_trips)
str(all_trips)  #See list of columns and data types (numeric, character, etc)
summary(all_trips)  #Statistical summary of data. Mainly for numerics

# Add columns that list the date, month, day, and year of each ride
# This will allow us to aggregate ride data for each month, day, or year ... before completing these operations we could only aggregate at the ride level

all_trips$date <- as.Date(all_trips$started_at) #The default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

# Add a "ride_length" calculation to all_trips (in seconds)

all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)

colnames(all_trips)

# Convert "ride_length" from Factor to numeric so we can run calculations on the data
is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)


str(all_trips)

#Dropped any occasional bad data(negative datas) and created a new Dataframe

all_trips_v2 <- all_trips[!(all_trips$ride_length<0),]

#Dropped ride_lenght in the excel files i imported, because its not needed here

all_trips<-all_trips%>%
  select(-c(ride_lenght))



## STEP 4: CONDUCT DESCRIPTIVE ANALYSIS

# Descriptive analysis on ride_length (all figures in seconds)
mean(all_trips_v2$ride_length) #straight average (total ride length / rides)
median(all_trips_v2$ride_length) #midpoint number in the ascending array of ride lengths
max(all_trips_v2$ride_length) #longest ride
min(all_trips_v2$ride_length) #shortest ride

# Lets condense the four lines above to one line using summary() on the specific attribute for a better view

summary(all_trips_v2$ride_length)

# Compare members and casual users

aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)

# See the average ride time by each day for members vs casual users

aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

# Notice that the days of the week are out of order. Let's fix that.

all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Now, let's run the average ride time by each day for members vs casual users

aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

# analyze ridership data by type and weekday.  To use the wday function, lets make sure we install the lubridate package
install.packages("lubridate")
library(lubridate)

all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()						#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(member_casual, weekday)								# sorts

# Let's visualize the number of rides by rider type
#Lets install ggplot2

library(ggplot2)

all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")


# Let's create a visualization for average duration

all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")


## trying out new colors
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration)) +
  geom_col(position = "dodge")+labs(color ="member_casual")+ scale_colour_manual(values=c("casual"= "blue", "member" = "white"))










#Download as Csv file for further analysis

counts <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

write.csv(all_trips, file = "All_Trips_data.csv")

            


