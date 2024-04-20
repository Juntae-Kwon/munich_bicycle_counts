extract_DateTime = function(df) {
  
  df$date = as.character(df$date)
  df$date = as.POSIXct(df$date, format = "%Y%m%d%H", tz = "UTC")
  df$year = as.integer(format(df$date, "%Y"))
  df$month = as.integer(format(df$date, "%m"))
  df$day = as.integer(format(df$date, "%d"))
  df$hour = as.integer(format(df$date, "%H"))
  
  return(df)
}


missing_data_frame <- function(df) {
  df_missing <- df %>%
    group_by(station, year, month, day) %>%
    count(hour) %>%
    filter(n != 4)
  
  n <- nrow(df_missing)
  
  ret <- NULL
  
  for(i in 1:n) {
    
    target_station <- df_missing$station[i]
    target_year <- df_missing$year[i]
    target_month <- df_missing$month[i]
    target_day <- df_missing$day[i]
    target_hour <- df_missing$hour[i]
    
    start_time <- ISOdate(year = target_year, month = target_month, day = target_day, hour = target_hour, tz = "UTC")
    seq_time <- seq.POSIXt(start_time, by = "15 mins", length.out = 4)
    
    temp_time <- subset(df, 
                        station == target_station & year == target_year & month == target_month & day == target_day & hour == target_hour,
                        select = ds,
                        drop = TRUE)
    
    target_time <- as.POSIXct(setdiff(seq_time, temp_time), tz = "UTC")
    
    nn <- length(target_time)
    
    for(j in 1:nn) {
      ret <- bind_rows(ret,
                       data.frame(
                         date = format(target_time[j], "%Y-%m-%d"),
                         station = target_station,
                         direction_1 = NA,
                         direction_2 = NA,
                         comment = "Missing point",
                         ds = target_time[j],
                         year = lubridate::year(target_time[j]),
                         month = lubridate::month(target_time[j]),
                         day = lubridate::day(target_time[j]),
                         dayofweek = lubridate::wday(target_time[j], week_start = 1),
                         hour = lubridate::hour(target_time[j])
                       ) %>%
                         mutate(hour_weekday = (dayofweek - 1) * 24 + hour,
                                month_year = (year - 2008) * 12 + (month - 6))
      )
    }
  }
  return(ret)
}


split_data_by_station <- function(df, sta) {
  ret <- df %>% filter(station == sta)
  ret <- ret %>% 
    mutate(lag1_d1 = dplyr::lag(direction_1),
           lag1_d2 = dplyr::lag(direction_2))
  return(ret)
}
