library(data.table)
library(dplyr)
library(lubridate)

#-----------#
# Load data #
#-----------#

#' Here you don't have to load the data from https:://opendata.muenchen.de/dataset.
#' If you want to know how to import data and preprocess from scratch, go to Data_original script.

df_cycling <- fread("data_folder/df_cycling.csv")
df_air_temp <- fread("data_folder/air_temperature.txt", sep = ";", header = TRUE)
df_precip <- fread("data_folder/precipitation.txt", sep = ";", header = TRUE)
df_school <- fread("data_folder/school_holidays.csv", sep = ";", header = TRUE)
df_public <- fread("data_folder/public_holidays.csv", sep = ";", header = TRUE)


#-------------------------#
# Preprocess cycling data #
#-------------------------#

# switch class of ds from character to POSIXct
df_cycling$ds <- as.POSIXct(df_cycling$ds, tz = "UTC")

# Replace empty strings with NA in comment
df_cycling$comment <- ifelse(df_cycling$comment == "", NA, df_cycling$comment)
df_cycling %>% distinct(comment)

# Remove repeated time-stamps (rows) based on `ds`
df_cycling <- df_cycling %>% 
  group_by(station) %>%
  distinct(ds, .keep_all = TRUE) %>%
  ungroup() 

# Modify erroneous time-stamps: e.g., "2020-04-09 13:59:00" -> "2020-04-09 14:00:00"
# and re-define hour_weekday
df_cycling <- df_cycling %>%
  mutate(ds = lubridate::round_date(ds, unit = "15 minutes"),
         year = lubridate::year(ds),
         month = lubridate::month(ds),
         day = lubridate::day(ds),
         hour = lubridate::hour(ds),
         dayofweek = lubridate::wday(ds, week_start = 1), # Monday == 1 through Sunday == 7
         hour_weekday = (dayofweek - 1) * 24 + hour,
         month_year = (year - 2008) * 12 + (month - 6))

# Make up the missing data with NA values
df_cycling <- rbind(
  df_cycling,
  missing_data_frame(df_cycling)
)


#-------------------------#
# Preprocess holiday data #
#-------------------------#

df_school$date <- as.POSIXct(df_school$date, format = "%d.%m.%y", tz = "UTC")
df_public$date <- as.POSIXct(df_public$date, format = "%Y-%m-%d", tz = "UTC")
df_public <- df_public %>% 
  mutate(public_holiday = ifelse(public_holiday == 2, 1, public_holiday)) %>% # Make holiday binary
  select("date", "public_holiday")


#-------------------------#
# Preprocess weather data #
#-------------------------#

df_air_temp <- df_air_temp %>% select("MESS_DATUM", "TT_TU")
df_precip <- df_precip %>% select("MESS_DATUM", "R1")

colnames(df_air_temp) <- c("date", "air_temp")
colnames(df_precip) <- c("date", "precipitation")

df_air_temp <- extract_DateTime(df_air_temp)
df_precip <- extract_DateTime(df_precip)

# Filter out relevant time stamps
df_air_temp <- df_air_temp %>% filter((year == 2008 & month >= 6) | (year > 2008))
df_precip <- df_precip %>% filter((year == 2008 & month >= 6) | (year > 2008))

# Replace -999 with NA
df_air_temp <- df_air_temp %>% mutate(air_temp = replace(air_temp, which(air_temp == -999), NA))
df_precip <- df_precip %>% mutate(precipitation = replace(precipitation, which(precipitation == -999), NA))

# Factorize precipitation:
# No rain (0), drizzle (< 0.5) rain (>= 0.5)
df_precip <- df_precip %>% 
  mutate(ind = ifelse(precipitation == 0, "no_rain", ifelse(precipitation < 0.5, "drizzle", "rain"))) 
df_precip$ind <- factor(df_precip$ind, levels = c("no_rain", "drizzle", "rain"))


#----------------#
# Merge all data #
#----------------#

temp_precip <- merge(df_air_temp, df_precip, by = c("year", "month", "day", "hour"), all.x = TRUE)

df <- merge(df_cycling, df_school, by = 'date', all = TRUE)
df <- merge(df, df_public, by = 'date', all = TRUE)
df$school_holiday[is.na(df$school_holiday)] <- 0
df$public_holiday[is.na(df$public_holiday)] <- 0
df <- df %>% 
  mutate(holiday = ifelse(school_holiday == 1 | public_holiday == 1, 1, 0)) %>%
  select(-c(school_holiday, public_holiday))

df_temp_precip <- merge(df, temp_precip, by = c("year", "month", "hour", "day"), all.x = TRUE) %>% select(-c(date.x, date.y))
df_temp_precip <- data.table(df_temp_precip)


#--------------------------------------------------------#
# Split data by station and add the response lagged by 1 #
#--------------------------------------------------------#

# Arnulf first day of operation 01.06.2008 
df_Arnulf <- split_data_by_station(df_temp_precip, "Arnulf")

# Kreuther first day of operation 20.06.2008
df_Kreuther <- split_data_by_station(df_temp_precip, "Kreuther")
df_Kreuther <- df_Kreuther %>% filter(date >= "2008-06-20")

# Olympia first day of operation 30.07.2009
df_Olympia <- split_data_by_station(df_temp_precip, "Olympia")
df_Olympia <- df_Olympia %>% filter(date >= "2009-07-30")

# Hirsch first day of operation 07.12.2009
df_Hirsch <- split_data_by_station(df_temp_precip, "Hirsch")
df_Hirsch <- df_Hirsch %>% filter(date >= "2009-12-07")

# Margareten first day of operation 08.07.2011
df_Margareten <- split_data_by_station(df_temp_precip, "Margareten")
df_Margareten <- df_Margareten %>% filter(date >= "2011-07-08")

# Erhardt first day of operation 25.07.2011 
df_Erhardt <- split_data_by_station(df_temp_precip, "Erhardt")
df_Erhardt <- df_Erhardt %>% filter(date >= "2011-07-25")
