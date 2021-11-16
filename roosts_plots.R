#' Author: Varalika Jain
#' Heat maps: roosting locations near Cumberland Wildpark

#------------------------------------------------------------------------------
####----(i) LOAD LIBRARIES----####
library(move)
library(lubridate)
library(suncalc)
library(dplyr)
library(rgdal)
library(sp)
library(ggmap)
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
####----(ii) DOWNLOAD DATA FROM MOVEBANK (MOVE OBJECT)----####
#' Permissions required to download data
#' Input movebank login details
login <- movebankLogin()

#' Source data from GPS tagged ravens
roosts <- getMovebankData(study="Corvus corax, Common Raven - Eastern Alps", 
                          login=login,
                          timestamp_start = "20171221000000000", 
                          timestamp_end = "2020122000000000",
                          removeDuplicatedTimestamps=TRUE)

#' Assigning the correct time zone
head(timestamps(roosts))
timestamps(roosts) <- with_tz(timestamps(roosts), tz="Europe/Vienna")

#' Check the timezone is correct
head(timestamps(roosts))

#' Check which individuals are in the dataset
all_IDs <- levels(roosts@trackId)
all_IDs

#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
####----(iii) CONVERT MOVE OBJECT TO DATAFRAME----####
roosts_df <- as.data.frame(roosts)
head(roosts_df)
#' Check that timestamps are still CET/CEST
roosts_df$timestamps
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
####----(iv) ISOLATING NIGHT TIME GPS FIXES----####
#' Loggers programmed to sleep at sunset, log a fix 6 hrs after sunset,
#' and start again at sunrise 

####----(1) Mark real sunset and sunrise----#### 
#' Create a dataframe with timestamps and location for 'suncalc' package
roosts_sun <- data.frame(date= as.Date(roosts_df$timestamps, tz = "Europe/Vienna"), 
                         lat=roosts_df$location_lat, lon=roosts_df$location_long)

#' Caluclate sunrise and sunset times
roosts_sunrise <-getSunlightTimes(data=roosts_sun, keep="sunrise", tz = "Europe/Vienna") 
roosts_sunset <- getSunlightTimes(data=roosts_sun, keep="sunset", tz = "Europe/Vienna") 

####----(2) Retain GPS locations only at night----####
#' Creating a new column for the move object, where fixes that have timestamps
#' before sunrise and after sunset (i.e., night) should be marked as 1, else 0
roosts_df$night <- ifelse(roosts_df$timestamps < roosts_sunrise$sunrise | 
                         roosts_df$timestamps > roosts_sunset$sunset, 
                       night <- 1, night <- 0)

#' Subset the points at night
roosts_night <- roosts_df[roosts_df$night==1,]
head(roosts_night)

#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
####----(v) ISOLATING ROOST FIXES ----####

####----(1) Method 1: Slicing first and last GPS fix from nighttime subset----####
#' Create a column for date
roosts_night$date <- as.Date(roosts_night$timestamps)

#' Group the fixes by individual and data
#' Arrange timestamps and select first and last 
roost_fixes_1 <- roosts_night %>% group_by(local_identifier, date)%>%
  arrange(timestamps) %>%
  filter(row_number()==1 | row_number()==n())
head(roost_fixes_1)

#' Visualize ground speed and time of fix
plot(roost_fixes_1$time, roost_fixes_1$ground_speed,
     xlab="Hour ", ylab="Ground speed in ms-1 ", xlim=c(0, 24), xaxt='n')
axis(side = 1, at=0:23)

#' Filter out the GPS fixes that are stationary
stationary_pts_1 <- roost_fixes_1 %>% 
  filter(ground_speed < 0.3) #but why are some ravens moving in the middle of the night?

####----(2) Method 2: Getting rid of time lags less than an hour long----####
#' Calculate the time difference
roosts_night_timediff <- 
  roosts_night %>% mutate(timediff = timestamps - lag(timestamps, default = first(timestamps)))
head(roosts_night_timediff)

#' Create the timelag seconds to numeric format
roosts_night_timediff$timediff <- as.numeric(roosts_night_timediff$timediff, units="secs")

#' These night recordings should have some hours between them (based on the device settings)
#' so we exclude points that have less than 3600 seconds between them. For the new
#' column timelag, GPS fixes with a difference of <3600 will be 0, else 1
roosts_night$timelag <- ifelse(roosts_night_timediff$timediff < 3600, 
                               timelag <- 0, timelag <- 1)
roost_fixes_2 <- roosts_night[roosts_night$timelag==1,]
head(roost_fixes_2)

#' Filter out the GPS fixes that are stationary
stationary_pts_2 <- roost_fixes_2 %>% 
  filter(ground_speed < 0.3) #but why are some ravens moving in the middle of the night?

####----(3) Comparing the two methods ----####
#' Method 1 produced fewer GPS fixes than method 2

#' Checking differences, setdiff(x,y) are those elements in x but not in y
setdiff(as.character(roost_fixes_1$timestamps), as.character(roost_fixes_2$timestamps))
setdiff(as.character(roost_fixes_2$timestamps), as.character(roost_fixes_1$timestamps))

#' Plotting the data
roost_fixes_1$time <- format(roost_fixes_1$timestamps, format = "%H")
roost_fixes_2$time <- format(roost_fixes_2$timestamps, format = "%H")

plot(roost_fixes_1$date, roost_fixes_1$time)
plot(roost_fixes_2$date, roost_fixes_2$time)
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
####----(vi) HEAT MAPS OF THE ROOSTS - METHOD 1----####
####----(1) Download the map----####
#' Insert personal API key
ggmap::register_google(key = "insert API key")

map <- ggmap(get_googlemap(center = c(lon = 13.94729, lat = 47.80939),
                           zoom = 15, scale = 2,
                           maptype ='roadmap'))
map

####----(1) Create season-year column----####
season <- function(dates) {
  win <- as.Date("2012-12-21", format = "%Y-%m-%d") # Winter Solstice
  spr <- as.Date("2012-03-21",  format = "%Y-%m-%d") # Spring Equinox
  sum <- as.Date("2012-06-21",  format = "%Y-%m-%d") # Summer Solstice
  aut <- as.Date("2012-09-23",  format = "%Y-%m-%d") # Autumn Equinox
  
  # Convert dates from any year to 2012 dates (as a reference year)
  d <- as.Date(strftime(dates, format="2012-%m-%d"))
  
  #'If else function to categorize dates according to season
  ifelse (d >= win | d < spr, "Winter",
          ifelse (d >= spr & d < sum, "Spring",
                  ifelse (d >= sum & d < aut, "Summer", "Autumn")))
}

#' Create a season column 
stationary_pts_1$ssn <- season(stationary_pts_1$timestamps)
#' Season-yr column
stationary_pts_1$season_yr <- as.factor(paste0(stationary_pts_1$ssn," ",format(stationary_pts_1$timestamps, "%Y")))

levels(stationary_pts_1$season_yr)
stationary_pts_1$local_identifier <- as.factor(stationary_pts_1$local_identifier)
levels(stationary_pts_1$local_identifier)

####----(2) Heat maps per season and year----####
#' Create empty lists to store the results
set = list()
maps = list()

for (i in unique(stationary_pts_1$season_yr)){
  set[[i]] <- stationary_pts_1 %>% filter(season_yr == i)
  for (j in 1:length(set)){
    pdf(paste0(names(set)[j]," roost",".pdf"))
    maps[[j]] <- map + stat_density2d(data= as.data.frame(set[[j]]),
                         aes(x=location_long, y=location_lat, 
                             fill = ..level.., alpha=..level..),
                         contour = TRUE, geom="polygon") +
      theme(legend.position = "none") + scale_fill_viridis_c()+
     ggtitle(names(set[j]))
   print(maps[[j]])
   dev.off()
  }
}
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
####----(vii) SUMMARISE----####
count(roosts_df)
count(as.data.frame(stationary_pts_1))

no_fixes <- stationary_pts_1 %>% group_by(season_yr) %>% tally() %>%
  rename("no_fixes" = "n")
no_indv <- stationary_pts_1 %>% group_by(season_yr, local_identifier) %>% 
  tally() %>%
  select(season_yr, local_identifier) %>% tally() %>%
  rename("no_indv" = "n")

summary <- merge(no_fixes, no_indv, by = "season_yr")
#------------------------------------------------------------------------------
####----END----####


