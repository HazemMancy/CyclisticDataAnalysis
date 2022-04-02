# First load libraries =======================
library(tidyverse)
library(tidyr)
library(readr)
library(ggplot2)
library(DataExplorer)
library(stringr)
library(lubridate)
library(janitor)
library(skimr)

# Import and prepare data ===============================
Mar_2021 <-
  read.csv(
    "C:/Users/hazem/Desktop/Case-Studies/Bike-Share/data/202103-divvy-tripdata.csv"
  )
Apr_2021 <-
  read.csv(
    "C:/Users/hazem/Desktop/Case-Studies/Bike-Share/data/202104-divvy-tripdata.csv"
  )
May_2021 <-
  read.csv(
    "C:/Users/hazem/Desktop/Case-Studies/Bike-Share/data/202105-divvy-tripdata.csv"
  )
Jun_2021 <-
  read.csv(
    "C:/Users/hazem/Desktop/Case-Studies/Bike-Share/data/202106-divvy-tripdata.csv"
  )
Jul_2021 <-
  read.csv(
    "C:/Users/hazem/Desktop/Case-Studies/Bike-Share/data/202107-divvy-tripdata.csv"
  )
Aug_2021 <-
  read.csv(
    "C:/Users/hazem/Desktop/Case-Studies/Bike-Share/data/202108-divvy-tripdata.csv"
  )
Sep_2021 <-
  read.csv(
    "C:/Users/hazem/Desktop/Case-Studies/Bike-Share/data/202109-divvy-tripdata.csv"
  )
Oct_2021 <-
  read.csv(
    "C:/Users/hazem/Desktop/Case-Studies/Bike-Share/data/202110-divvy-tripdata.csv"
  )
Nov_2021 <-
  read.csv(
    "C:/Users/hazem/Desktop/Case-Studies/Bike-Share/data/202111-divvy-tripdata.csv"
  )
Dec_2021 <-
  read.csv(
    "C:/Users/hazem/Desktop/Case-Studies/Bike-Share/data/202112-divvy-tripdata.csv"
  )
Jan_2022 <-
  read.csv(
    "C:/Users/hazem/Desktop/Case-Studies/Bike-Share/data/202201-divvy-tripdata.csv"
  )
Feb_2022 <-
  read.csv(
    "C:/Users/hazem/Desktop/Case-Studies/Bike-Share/data/202202-divvy-tripdata.csv"
  )

# Let's fast explore the data of first month Mar-2021 using DataExplorer package
create_report(Mar_2021)

# Now we had a look on the data. So, we can start cleaning data ===================
# Start with `glimpse` to make sure that same columns specs are identical
glimpse(Mar_2021)
glimpse(Apr_2021)
glimpse(Mar_2021)
glimpse(Jun_2021)
glimpse(Jul_2021)
glimpse(Aug_2021)
glimpse(Sep_2021)
glimpse(Oct_2021)
glimpse(Nov_2021)
glimpse(Dec_2021)
glimpse(Jan_2022)
glimpse(Feb_2022)

# We can use `compare_df_cols` to make it easy
compare_df_cols(
  Mar_2021,
  Apr_2021,
  May_2021,
  Jun_2021,
  Jul_2021,
  Aug_2021,
  Sep_2021,
  Oct_2021,
  Nov_2021,
  Dec_2021,
  Jan_2022,
  Feb_2022,
  return = "mismatch"
)

# Clean names
clean_names(Mar_2021)
clean_names(Apr_2021)
clean_names(May_2021)
clean_names(Jun_2021)
clean_names(Jul_2021)
clean_names(Aug_2021)
clean_names(Sep_2021)
clean_names(Oct_2021)
clean_names(Nov_2021)
clean_names(Dec_2021)
clean_names(Jan_2022)
clean_names(Feb_2022)

# Check if there any duplicates to be removed
Mar_2021[duplicated(Mar_2021)]

Apr_2021[duplicated(Apr_2021)]

May_2021[duplicated(May_2021)]

Jun_2021[duplicated(Jun_2021)]

Jul_2021[duplicated(Jul_2021)]

Aug_2021[duplicated(Aug_2021)]

Sep_2021[duplicated(Sep_2021)]

Oct_2021[duplicated(Oct_2021)]

Nov_2021[duplicated(Nov_2021)]

Dec_2021[duplicated(Dec_2021)]

Jan_2022[duplicated(Jan_2022)]

Feb_2022[duplicated(Feb_2022)]

# All datatypes are good except start and end datetime. Let's convert them to date
Mar_2021 <-
  mutate(Mar_2021,
         started_at = as_datetime(started_at),
         ended_at = as_datetime(ended_at))

Apr_2021 <-
  mutate(Apr_2021,
         started_at = as_datetime(started_at),
         ended_at = as_datetime(ended_at))

May_2021 <-
  mutate(May_2021,
         started_at = as_datetime(started_at),
         ended_at = as_datetime(ended_at))

Jun_2021 <-
  mutate(Jun_2021,
         started_at = as_datetime(started_at),
         ended_at = as_datetime(ended_at))

Jul_2021 <-
  mutate(Jul_2021,
         started_at = as_datetime(started_at),
         ended_at = as_datetime(ended_at))

Aug_2021 <-
  mutate(Aug_2021,
         started_at = as_datetime(started_at),
         ended_at = as_datetime(ended_at))

Sep_2021 <-
  mutate(Sep_2021,
         started_at = as_datetime(started_at),
         ended_at = as_datetime(ended_at))

Oct_2021 <-
  mutate(Oct_2021,
         started_at = as_datetime(started_at),
         ended_at = as_datetime(ended_at))

Nov_2021 <-
  mutate(Nov_2021,
         started_at = as_datetime(started_at),
         ended_at = as_datetime(ended_at))

Dec_2021 <-
  mutate(Dec_2021,
         started_at = as_datetime(started_at),
         ended_at = as_datetime(ended_at))

Jan_2022 <-
  mutate(Jan_2022,
         started_at = as_datetime(started_at),
         ended_at = as_datetime(ended_at))

Feb_2022 <-
  mutate(Feb_2022,
         started_at = as_datetime(started_at),
         ended_at = as_datetime(ended_at))

# Double check the comparison between all tables column before binding them in one table
compare_df_cols(
  Mar_2021,
  Apr_2021,
  May_2021,
  Jun_2021,
  Jul_2021,
  Aug_2021,
  Sep_2021,
  Oct_2021,
  Nov_2021,
  Dec_2021,
  Jan_2022,
  Feb_2022,
  return = "mismatch"
)

# Now we can bind all 12 months data frames into one and later we are gonna split variables to make it easy while visualizing data
last_12_months_trips <- bind_rows(
  Mar_2021,
  Apr_2021,
  May_2021,
  Jun_2021,
  Jul_2021,
  Aug_2021,
  Sep_2021,
  Oct_2021,
  Nov_2021,
  Dec_2021,
  Jan_2022,
  Feb_2022
)

# Have a look on last 12 months historical data
summary(last_12_months_trips)

# Since rideable_type and member_casual are categorical, we can transform them to factors
last_12_months_trips <- last_12_months_trips %>%
  mutate(rideable_type = as.factor(rideable_type)) %>%
  mutate(member_casual = as.factor(member_casual))

# Let's add new column that contain the trip day, month, and day of the week and also ride duration which we will use them later to catch trends
# last_12_months_trips$date <- as.Date(last_12_months_trips$started_at)
#
# last_12_months_trips$day <-
#   format(as.Date(last_12_months_trips$started_at), "%d")

last_12_months_trips$day_of_week <-
  format(as.Date(last_12_months_trips$started_at), "%A")

last_12_months_trips$month <-
  format(as.Date(last_12_months_trips$started_at), "%b")

last_12_months_trips$hour_of_day <-
  format(last_12_months_trips$started_at, format = "%H")

last_12_months_trips$ride_duration <-
  difftime(last_12_months_trips$ended_at,
           last_12_months_trips$started_at)

# Convert ride duration to numeric to be shown as seconds and then format to be show as minutes
last_12_months_trips <-
  mutate(last_12_months_trips, ride_duration = as.numeric(ride_duration))

# Check if there are ride duration < 0
count(last_12_months_trips, ride_duration < 0)

# There are 142 records. We can exclude these records
last_12_months_trips <-
  filter(last_12_months_trips, ride_duration >= 0)

# check again
count(last_12_months_trips, ride_duration < 0) # cool

# convert ride duration from seconds to minutes
last_12_months_trips <-
  mutate(last_12_months_trips, ride_duration = (ride_duration / 60))



# Analyzing and visualizing data ===============

# Summary of ride duration
summary(last_12_months_trips$ride_duration)

# Store the last version of last 12 months trips for further analysis
write.csv(last_12_months_trips, "last_12_months_trips.csv")

# No of ride trips by month
trips_per_month <- last_12_months_trips %>%
  group_by(month) %>%
  summarise(number_of_rides = n()) %>%
  arrange(desc(number_of_rides))
trips_per_month

# How casual and member rides are different?
ggplot(data = last_12_months_trips,
       mapping = aes(x = month, fill = member_casual)) +
  geom_bar(position = "dodge") +
  labs(title = "No. Of Rides vs Months",
       x = "Month",
       y = "Number Of Rides",
       fill = "Subscription Type") +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_discrete(
    limits = c(
      "Jan",
      "Feb",
      "Mar",
      "Apr",
      "May",
      "Jun",
      "Jul",
      "Aug",
      "Sep",
      "Oct",
      "Nov",
      "Dec"
    )
  )
# It seems that number of trips is high goes up during summer
# During summer months (June, July, and August), casual rides exceeds member rides

# How casual and member rides are different?
ggplot(data = last_12_months_trips,
       mapping = aes(x = day_of_week, fill = member_casual)) +
  geom_bar(position = "dodge") +
  labs(title = "No. Of Rides vs Day Of The Week",
       x = "Day Of The Week",
       y = "Number Of Rides",
       fill = "Subscription Type") +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_discrete(
    limits = c(
      "Monday",
      "Tuesday",
      "Wednesday",
      "Thursday",
      "Friday",
      "Saturday",
      "Sunday"
    )
  )
# Member rides almost the same all days of the week. Unlikely, casual rides rise up and even more than member rides during weekends

# How casual and member rides are different?
ggplot(data = last_12_months_trips,
       mapping = aes(x = hour_of_day, fill = member_casual)) +
  geom_bar(position = "dodge") +
  labs(title = "No. Of Rides vs Hour Of Day",
       x = "Hour Of Day",
       y = "Number Of Rides",
       fill = "Subscription Type") +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_discrete(
    limits = c(
      "04",
      "05",
      "06",
      "07",
      "08",
      "09",
      "10",
      "11",
      "12",
      "13",
      "14",
      "15",
      "16",
      "17",
      "18",
      "19",
      "20",
      "21",
      "22",
      "23"
    )
  )
# Member rides are high during rush hours which can be an indicator that they use bikes to go to and come from work

# Top high number of rides start stations
start_stations <- last_12_months_trips %>%
  filter(!start_station_name == "") %>%
  group_by(start_station_name) %>%
  summarise(number_of_rides = n()) %>%
  arrange(desc(number_of_rides))

head(start_stations, n = 15)

# Total casual rides vs total member rides
summary(last_12_months_trips$member_casual)

# Create a pie chart
piechart <- data.frame(
  subscription_type = c("casual", "member"),
  value = summary(last_12_months_trips$member_casual)
)

ggplot(data = piechart,
       mapping = aes(x = "", y = value, fill = subscription_type)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Casual Rides vs Member Rides")+
  theme(
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    legend.position = "none"
  ) +
  annotate(
    "text",
    label = "Member",
    x = 1,
    y = 900000,
    size = 6
  ) +
  annotate(
    "text",
    label = "3,127,207",
    x = 1,
    y = 1250000,
    size = 6
  ) +
  annotate(
    "text",
    label = "55.17%",
    x = 1,
    y = 1600000,
    size = 6
  ) +
  annotate(
    "text",
    label = "Casual",
    x = 1,
    y = 4750000,
    size = 6
  ) +
  annotate(
    "text",
    label = "2,540,634",
    x = 1,
    y = 4400000,
    size = 6
  ) +
  annotate(
    "text",
    label = "44.83%",
    x = 1,
    y = 4050000,
    size = 6
  )

# How casual riders differ from member riders according to bike type?
ggplot(data = last_12_months_trips,
       mapping = aes(x = rideable_type, fill = member_casual)) +
  geom_bar(position = "dodge") +
  labs(title = "Bike Type vs Subscription Type",
       x = "Bike Type",
       y = "Number Of Rides",
       fill = "Subscription Type") +
  scale_x_discrete(limits = c("docked_bike",
                              "classic_bike",
                              "electric_bike"))
