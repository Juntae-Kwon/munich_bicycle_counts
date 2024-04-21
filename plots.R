library(ggplot2)

#-------------------------------#
# plots of seasonality in slide #
#-------------------------------#

# Monthly 
df_temp_precip %>% 
  select(month, direction_1, station) %>% 
  group_by(month, station) %>% 
  summarise(mean_cyclists_per_day = mean(direction_1, na.rm = T)) %>% 
  ggplot(aes(x = month, y = mean_cyclists_per_day, color = station)) +
  geom_line() +
  ggtitle("direction_1: Average of cyclists by month") +
  xlab("month") +
  ylab("cyclists per Month") +
  theme_grey() +
  scale_x_continuous(breaks = seq(1, 12, by = 1), expand = c(0, 0), 
                     labels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

# Yearly 
ggplot() +
  df_temp_precip %>% 
  group_by(year, station) %>% 
  summarise(mean_cyclists_per_day = mean(direction_1, na.rm = T)) %>% 
  geom_line(mapping = aes(x = year, y = mean_cyclists_per_day, color = station)) +
  df_temp_precip %>% 
  group_by(year) %>% 
  summarise(mean_cyclists_per_day = mean(direction_1, na.rm = T)) %>% 
  geom_line(mapping = aes(x = year, y = mean_cyclists_per_day, color = "All stations")) +
  ggtitle("direction_1: Yearly averaged cyclists") +
  xlab("date") +
  theme_grey() +
  scale_x_continuous(breaks = scales::breaks_width(1), expand = c(0, 0)) 


# Hourly + Weekly
df_temp_precip %>% 
  select(hour_weekday, direction_1, station) %>% 
  group_by(hour_weekday, station) %>% 
  summarise(mean_cyclists_per_day = mean(direction_1, na.rm = T)) %>% 
  ggplot(aes(x = hour_weekday, y = mean_cyclists_per_day, color = station)) +
  geom_line() +
  ggtitle("direction_1: Average of cyclists by hour_weekday") +
  xlab("hour_weekday") +
  ylab("cyclists per hour") +
  theme_grey() +
  geom_vline(xintercept = seq(0, 167, by = 24), linetype = "dashed", color = "red") +
  scale_x_continuous(breaks = seq(0, 167, by = 24), expand = c(0, 0), 
                     labels = paste0(seq(0, 167, by = 24), "\n", c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")))

