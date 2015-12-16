citation = function (text="HunterRatliff1@gmail.com", pos="BR", size=12, color="grey", style="plain")
{
  require(grid)
  require(gridExtra)
  
  if(pos=="BR")
  {
    grid.text(text,
              x = unit(0.97, "npc"), 
              y = unit(0.03, "npc"), 
              just = c("right", "bottom"), 
              gp = gpar(
                fontface = style, 
                fontsize = size, 
                col = color))
  }
  if(pos=="BL")
  {
    grid.text(text,
              x = unit(0.03, "npc"), 
              y = unit(0.03, "npc"), 
              just = c("left", "bottom"), 
              gp = gpar(fontface = style, fontsize = size, col = color))
  }
  if(pos=="TR")
  {
    grid.text(text,
              x = unit(0.97, "npc"), 
              y = unit(0.97, "npc"), 
              just = c("right", "top"), 
              gp = gpar(fontface = style, fontsize = size, col = color))
  }
  if(pos=="TL")
  {
    grid.text(text,
              x = unit(0.03, "npc"), 
              y = unit(0.97, "npc"), 
              just = c("left", "top"), 
              gp = gpar(fontface = style, fontsize = size, col = color))
  }
}


### -------------------- ###
import_dir = "~/Downloads" 
folder_dir = "~/Google Drive/100 - Publicly hosted/rootR/Austin Pets Alive/weather"

HOURS <- read.csv(paste0(import_dir, "/HoursExport.csv"), na.strings="")
hours = HOURS %>% 
  select(DateVolunteered, HoursWorked, ends_with("Time"), ends_with("Id")) %>%
  mutate(
    DateVolunteered = mdy_hms(DateVolunteered),
    ClockStartTime  = hm(ClockStartTime),
    ClockEndTime    = hm(ClockEndTime)) %>%
  rename(
    Date = DateVolunteered,
    Start = ClockStartTime,
    End = ClockEndTime,
    ID.Category = DatabaseActivityCategoryId,
    ID.Activity = DatabaseActivityId,
    ID.User     = DatabaseUserId
  ) %>%
  mutate(
    Start = Date + Start,
    End   = Date + End,
    Date = as.Date(Date))

### ------------------------------------------------------------ ###
## Install the weatherData package
library("devtools")
install_github("weatherData", "Ram-N")
library(weatherData)

## See what we can get
checkDataAvailabilityForDateRange("AUS", start_date = "2014-10-22", end_date = "2015-10-22")

## Get weather
weather = getSummarizedWeather("AUS", start_date = "2014-10-22", end_date = "2015-10-22",opt_all_columns = TRUE)

# Formatting
weather$Date   = as.Date(weather$Date)
weather$Events = factor(weather$Events)
weather$TempC  = round((weather$Mean_TemperatureF - 32) * (5/9), 1)


temp1 = hours %>%
  filter(ID.Category == 54883) %>%
  group_by(Date, ID.Activity) %>%
  summarise(hours = sum(HoursWorked), Vols = n())

temp2 = weather %>%
  select(Date, TempC, PrecipitationIn,  CloudCover, Events) %>%
  rename(#Temperature = Mean_TemperatureF, 
         Precip = PrecipitationIn, 
         Clouds = CloudCover)
 

df = left_join(temp1, temp2) %>%
  filter(ID.Activity == 316605)
df[, 6][df[, 6] == "T"] <- "0.00"
df$Precip = as.numeric(df$Precip)
df$Precip = round(df$Precip, digits = 1)


# qplot(data=df, x=Temperature, y=Clouds, size=hours, color=hours, geom="jitter") +
#   scale_color_continuous(low="yellow", high="blue") + facet_wrap("Events")
# qplot(data=df, x=Temperature, y=Clouds, alpha=hours, fill=Vols, geom="tile") + 
#   scale_fill_continuous(low="yellow", high="blue")
# qplot(data=df, x=Precip, y=Clouds, alpha=hours, color=hours, geom="jitter") + 
#   scale_color_continuous(low="yellow", high="blue") + scale_x_log10()
# ggplot(df, aes(x=hours)) + 
#   geom_density(aes(fill=Events), alpha=I(0.5)) 
#   geom_jitter(aes(y=hours, color=Events, size=Precip))


# df[df == 0] <- FALSE
# df[df == 0] <- FALSE
# df[, 6:7][df[, 6:7] == 0] <- NA

# df[, 6][df[, 6] == ] <- FALSE
# df[, 6][df[, 6] > 0] <- TRUE
df$Precip <- as.integer(df$Precip)
  (df$Precip)


# ggplot(df, aes(x=hours, y=Precip)) + geom_jitter(aes(size=Vols)) + scale_y_sqrt()
ggplot(df, aes(x=Temperature, y=Clouds, color=factor(Precip)), na.rm=T) + 
  geom_jitter(aes(size=Vols), alpha=0.5) + #geom_rug(sides = "r") + 
  geom_jitter(aes(size=Vols), alpha=1, data=subset(df, Precip!= 0)) + 
  scale_colour_colorblind() + theme_hc() + scale_size(range = c(1,10)) +
  ggtitle("Who walks dogs in the rain?")
citation(text = "Hunter Ratliff",pos = "TL")
citation(text = "hunter.ratliff@austinpetsalive.org", size = 10)
  # stat_density2d(aes(fill=Precip, alpha=..level..), geom="polygon", na.rm = T) + 
  # geom_violin()
  
ggplot(df, aes(x=Date)) + 
  geom_smooth(aes(y=hours/Vols, size=Vols), color=I("red")) + 
  geom_smooth(aes(y=Precip*5), color=I("blue")) + 
  geom_smooth(aes(y=Clouds), color=I("orange")) + 
  theme_fivethirtyeight()
  
# dcast(df, Date ~ .) %>% View()
