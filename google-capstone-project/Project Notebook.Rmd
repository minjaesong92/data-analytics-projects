---
title: "Cyclistic Case Study"
output: html_notebook
---
##### For our case study, we will be importing the last 13 months of Cyclistic data, wrangle and combine the data into a single file, clean the data and prepare it for analysis, conduct analysis, create visualizations, and export our results.
##############
##### Before beginning, we will install the needed packages
###### tidyverse for data importing and wrangling
###### lubridate for date functions
###### ggplot for visualization
##############
```{r}
library(tidyverse)  #helps wrangle data
library(lubridate)  #helps wrangle date attributes
library(ggplot2)  #helps visualize data
getwd() #displays your working directory
setwd("/Users/MinJae/Documents/Google Capstone - Cyclistic/Datasets") #sets your working directory to simplify calls to data
```

Collect and import the last 13 months of data
```{r}
m1_2021 <- read_csv("Datasets/202101-divvy-tripdata.csv", show_col_types = FALSE)
m2_2021 <- read_csv("Datasets/202102-divvy-tripdata.csv", show_col_types = FALSE)
m3_2021 <- read_csv("Datasets/202103-divvy-tripdata.csv", show_col_types = FALSE)
m4_2021 <- read_csv("Datasets/202104-divvy-tripdata.csv", show_col_types = FALSE)
m5_2021 <- read_csv("Datasets/202105-divvy-tripdata.csv", show_col_types = FALSE)
m6_2021 <- read_csv("Datasets/202106-divvy-tripdata.csv", show_col_types = FALSE)
m7_2021 <- read_csv("Datasets/202107-divvy-tripdata.csv", show_col_types = FALSE)
m8_2021 <- read_csv("Datasets/202108-divvy-tripdata.csv", show_col_types = FALSE)
m9_2021 <- read_csv("Datasets/202109-divvy-tripdata.csv", show_col_types = FALSE)
m10_2021 <- read_csv("Datasets/202110-divvy-tripdata.csv", show_col_types = FALSE)
m11_2021 <- read_csv("Datasets/202111-divvy-tripdata.csv", show_col_types = FALSE)
m12_2021 <- read_csv("Datasets/202112-divvy-tripdata.csv", show_col_types = FALSE)
m1_2022 <- read_csv("Datasets/202201-divvy-tripdata.csv", show_col_types = FALSE)
```

```{r}
colnames(m1_2021)
colnames(m2_2021)
colnames(m3_2021)
colnames(m4_2021)
colnames(m5_2021)
colnames(m6_2021)
colnames(m7_2021)
colnames(m8_2021)
colnames(m9_2021)
colnames(m10_2021)
colnames(m11_2021)
colnames(m12_2021)
colnames(m1_2022)
```

```{r}
str(m1_2021)
```
```{r}
str(m2_2021)
```


```{r}
all_trips <- bind_rows(m1_2021, m2_2021, m3_2021, m4_2021, m5_2021, m6_2021, m7_2021, m8_2021, m9_2021, m10_2021, m11_2021, m12_2021, m1_2022)
```

```{r}
all_trips <- all_trips %>%  
  select(-c(start_lat, start_lng, end_lat, end_lng))

```


inspection
```{r}
colnames(all_trips)  #List of column names
nrow(all_trips)  #How many rows are in data frame?
dim(all_trips)  #Dimensions of the data frame?
head(all_trips)  #See the first 6 rows of data frame.  Also tail(all_trips)
str(all_trips)  #See list of columns and data types (numeric, character, etc)
summary(all_trips)  #Statistical summary of data. Mainly for numerics

```
```{r}
table(all_trips$member_casual)
```

```{r}
all_trips <-  all_trips %>% 
  mutate(member_casual = recode(member_casual,
                           "Subscriber" = "member",
                           "Customer" = "casual"))


table(all_trips$member_casual)
```


# use "started_at" column to get the date so we can aggegate fo each month, day, year
```{r}
all_trips$date <- as.Date(all_trips$started_at) #The default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

```


# add and calculate ride_length
```{r}
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)
```

```{r}
str(all_trips)
```



```{r}
is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)


head(all_trips, 50)
```

# sometimes we might have odd numbers for ride_length like negative

```{r}
all_trips_ex1 <- filter(all_trips, ride_length<=0)

head(all_trips_ex1)
```
# so we need to get rid of them for more accurate analysis (could be for maintenance for the bike)
```{r}
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]
all_trips_v3 <- subset(all_trips_v2, select = -c(start_station_name,start_station_id,end_station_name,end_station_id))
which(is.na(all_trips_v3$ride_length))
sum(is.na(all_trips_v3$ride_length))
sapply(all_trips_v3, function(x) sum(is.na(x)))
all_trips_v4 <- na.omit(all_trips_v3)
head(all_trips_v4)
nrow(all_trips_v4)
dim(all_trips_v4)
```


# we can begin descriptive analysis since our data is now clean, accurate, and ready
```{r}
# Descriptive analysis on ride_length (all figures in seconds)
mean(all_trips_v4$ride_length) #straight average (total ride length / rides)
median(all_trips_v4$ride_length) #midpoint number in the ascending array of ride lengths
max(all_trips_v4$ride_length) #longest ride
min(all_trips_v4$ride_length) #shortest ride

```

```{r}
summary(all_trips_v4$ride_length)
```

compare members and casual riders
```{r}
aggregate(all_trips_v4$ride_length ~ all_trips_v4$member_casual, FUN = mean)
aggregate(all_trips_v4$ride_length ~ all_trips_v4$member_casual, FUN = median)
aggregate(all_trips_v4$ride_length ~ all_trips_v4$member_casual, FUN = max)
aggregate(all_trips_v4$ride_length ~ all_trips_v4$member_casual, FUN = min)

```

```{r}
# See the average ride time by each day for members vs casual users
aggregate(all_trips_v4$ride_length ~ all_trips_v4$member_casual + all_trips_v4$day_of_week, FUN = mean)

```

```{r}
all_trips_v4$day_of_week <- ordered(all_trips_v4$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
```

```{r}
# Now, let's run the average ride time by each day for members vs casual users
aggregate(all_trips_v4$ride_length ~ all_trips_v4$member_casual + all_trips_v4$day_of_week, FUN = mean)

```


```{r}
# analyze ridership data by type and weekday
all_trips_v4 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
  ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(member_casual, weekday)								# sorts
```

```{r}
all_trips_v4 %>% 
  group_by(member_casual, month) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
  ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(member_casual, month)
```


```{r}
# lets visualize number of rides by month and sort rider type
all_trips_v4 %>% 
  group_by(member_casual, month) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, month)  %>% 
  ggplot(aes(x = month, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")
```

```{r}
# lets visualize average duration by month and sort rider type
all_trips_v4 %>% 
  group_by(member_casual, month) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, month)  %>% 
  ggplot(aes(x = month, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")
```

```{r}
# Let's visualize the number of rides by rider type
all_trips_v4 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

```


```{r}
# Let's create a visualization for average duration
all_trips_v4 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")

```

```{r}

# lets export a summary file showing member/casual, day of week, and ride_length
counts <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
write.csv(counts, file = '/Users/MinJae/Documents/Google Capstone - Cyclistic/avg_ride_length.csv')

```

```{r}
# lets export the all_trips_v4 for more analysis and visualizations in Tableau
write.csv(all_trips_v4, file= "/Users/MinJae/Documents/Google Capstone - Cyclistic/all_trips")
```


Add a new chunk by clicking the *Insert Chunk* button on the toolbar or by pressing *Ctrl+Alt+I*.

When you save the notebook, an HTML file containing the code and output will be saved alongside it (click the *Preview* button or press *Ctrl+Shift+K* to preview the HTML file).

The preview shows you a rendered HTML copy of the contents of the editor. Consequently, unlike *Knit*, *Preview* does not run any R code chunks. Instead, the output of the chunk when it was last run in the editor is displayed.
