## Set enviroment----
setwd("~/Google Drive/100 - Publicly hosted/rootR/Austin Pets Alive")
suppressPackageStartupMessages(require(dplyr))
suppressPackageStartupMessages(require(lubridate))
suppressPackageStartupMessages(require(ggplot2))
suppressPackageStartupMessages(require(ggthemes))
suppressPackageStartupMessages(require(stringi))
suppressPackageStartupMessages(require(reshape2))
source("Citation.R")

### Import hours ----
# Selected export of TLAC - Dog  hours
Hours <- read.csv("Dog Walking/data/Hours.csv", na.strings="") %>%
  tbl_df() %>%
  mutate(
    Date  = as.Date(mdy_hms(DateVolunteered)),
    Start = ymd_hm(paste(Date, ClockStartTime)),
    End   = ymd_hm(paste(Date, ClockEndTime)),
    Name  = paste(FirstName, LastName),
    Hours = round(HoursWorked, 2)
  ) %>%
  select(Date, Activity=ActivityName, Hours, Name, Start, End, 
         User.ID=DatabaseUserId, Type=EntryType)

# Factor IDs
Hours$User.ID <- as.factor(Hours$User.ID)

# Preview data
glimpse(Hours)

## Theme object ----
APA.theme = theme_fivethirtyeight() + theme(
  axis.title = element_text(family="Verdana", color = "#DD5928", face = "bold"),
  plot.title = element_text(hjust = 0, size = rel(1.5), face = "bold", family="American Typewriter", color = "#DD5928"),
  strip.background = element_rect(fill="#4D4D4D"),
  strip.text = element_text(family="Verdana", color = "#5BC002", face = "bold")
)

## PLOT #1 ----
Hours %>% 
  group_by(Date, Activity) %>%
  filter(Type=="Timeclock") %>%
  mutate(
    NumVols = n_distinct(User.ID), 
    median  = median(Hours),
    wk      = floor_date(Start, unit="week"),
    wkday   = weekdays(Start, abbreviate = T)
  )  %>%
  ggplot(aes(x=Start, y=NumVols, group=Activity)) + 
  geom_line() + 
  geom_point(aes(color=wkday)) + 
  facet_wrap("wk", scales = "free_x", ncol=3) + 
  APA.theme
citation.apa()

# Acts = c("Support (laundry, clean up, kongs)", "RuffTail Runner", "New Volunteers - Mentor Sessions 1 & 2", "Dog Walking")

### Weather ----
## Install the weatherData package
# library("devtools")
# suppressMessages(install_github("weatherData", "Ram-N"))
require(weatherData)

## Get weather
weather <- getSummarizedWeather("AUS", opt_all_columns = T,
                                start_date = "2014-11-06", end_date = "2015-11-06") %>%
  mutate(Date = as.Date(Date)) %>%
  select(Date, Events, Temp=Mean_TemperatureF, Vis=Mean_VisibilityMiles, 
         Wind=Mean_Wind_SpeedMPH, Prec=PrecipitationIn, Cloud=CloudCover)

## Join to data
Hours.Weather <- Hours %>%
  filter(
    Activity %in% c("RuffTail Runner", "Dog Walking"),
    Date < as.Date(ymd("2015-11-06"))
  ) %>%
  select(-Activity) %>%
  group_by(Date) %>%
  summarise(
    NumVols = n_distinct(User.ID), 
    median=median(Hours), 
    sum = sum(Hours)) %>%
  left_join(weather) %>%
  mutate(
    wk      = floor_date(Date, unit="week"),
    wkday   = weekdays(Date, abbreviate = T)
  )

## Formatting

# Make precipitation vector numerical
Hours.Weather$PrecNum <- as.numeric(Hours.Weather$Prec)

# Remove Fog
Hours.Weather$Fog    <- grepl(pattern = "Fog", Hours.Weather$Events)
Hours.Weather$Events <- gsub(pattern = "Fog-", replacement = "", Hours.Weather$Events)
Hours.Weather$Events <- gsub(pattern = "Fog",  replacement = "", Hours.Weather$Events)

# Make 'Rain-Thunderstorm' just 'Thunderstorm'
Hours.Weather$Events <- gsub(pattern = "Rain-Thunderstorm", replacement = "Thunderstorm", Hours.Weather$Events)

# Change no event to 'None
Hours.Weather$Events[Hours.Weather$Events==""] <- "None"

# Make events factors
Hours.Weather$Events <- factor(Hours.Weather$Events)

# Make weekday as factors
Hours.Weather$wkday <- factor(Hours.Weather$wkday, 
                   levels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"),
                   ordered = T)  
# PLOT #2 ----
Hours.Weather %>%
  # Geoms
  ggplot(aes(x=wkday, y=wk)) + 
  geom_tile(aes(fill=NumVols)) + 
  geom_text(aes(
    label=paste(round(sum), ""),
    color=Events), family="Verdana") +
  # Scales
  scale_fill_continuous(name="Number of Volunteers", low="#FF8888", high="#66CC66") +
  scale_color_manual(values = c("None"="#7D7D7D", "Rain"="black", "Thunderstorm"="blue")) + 
  # Labels & Theme
  labs(x="Day of the week", y="Week", 
       title="Hours of Dog Walking Logged & \nNumber of Dog Walkers Over the Last 14 Weeks") + 
  APA.theme
citation.apa()

# PLOT #3 ----
Hours.Weather %>%
  # Geoms
  ggplot(aes(x=Events,y=NumVols)) + 
  geom_boxplot(aes(fill=Events)) + # geom_violin(aes(fill=Events)) + 
  geom_jitter(aes(shape=wkday, color=dense_rank(PrecNum))) + 
  # Scales
  scale_shape_manual(
    name="Weekday or weekend?", 
    values = c("Sun"=15, "Mon"=3, "Tue"=3, "Wed"=3, "Thu"=3, "Fri"=3, "Sat"=17)) + # values = c("Sun"=15, "Mon"=3, "Tue"=2, "Wed"=8, "Thu"=6, "Fri"=4, "Sat"=17))
  scale_color_continuous(
    name="How wet was it?", 
    low="#66CC66", high="#FF8888") + 
  scale_fill_manual(
    values = c("None"="#F7FBFD", "Rain"="#B2D5EE", "Thunderstorm"="#4485F5")) + 
    guides(fill=F) +
  # Labels & Theme
  labs(x="Weather event", y="Number of\nvolunteers\nwho walked\ndogs", 
       title="Number of Dog Walkers vs\nType of Weather vs Day of the Week") +
  APA.theme
citation.apa()


  