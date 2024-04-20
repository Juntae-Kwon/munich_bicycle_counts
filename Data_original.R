library(data.table)
library(dplyr)
library(lubridate)

#------------------------------------#
# Load cycling data from the website #
#------------------------------------#

cycling_url = c(
  "https://opendata.muenchen.de/dataset/022a11ff-4dcb-4f03-b7dd-a6c94a094587/resource/66be7619-a672-4382-bf88-e3688c5abc2b/download/rad_2008_15min_06_06_23_r.csv",
  "https://opendata.muenchen.de/dataset/022a11ff-4dcb-4f03-b7dd-a6c94a094587/resource/3ef8aad9-a6b0-4c97-a6b7-8c3a63226b37/download/rad_2009_15min_06_06_23_r.csv",
  "https://opendata.muenchen.de/dataset/022a11ff-4dcb-4f03-b7dd-a6c94a094587/resource/6558a5f9-2c96-4e4b-985d-8eb99b7b73b1/download/rad_2010_15min_06_06_23_r.csv",
  "https://opendata.muenchen.de/dataset/022a11ff-4dcb-4f03-b7dd-a6c94a094587/resource/86962013-4854-4deb-aaf9-36e3770cde24/download/rad_2011_15min_export_06_06_23_r.csv",
  "https://opendata.muenchen.de/dataset/022a11ff-4dcb-4f03-b7dd-a6c94a094587/resource/ff5d2ebf-dde6-4f21-9c68-2aab74addeec/download/rad_2012_15min_06_06_23_r.csv",
  "https://opendata.muenchen.de/dataset/022a11ff-4dcb-4f03-b7dd-a6c94a094587/resource/9f4f798f-0ad1-4e86-8157-15c5e46eaf91/download/rad_2013_15min_06_06_23_r.csv",
  "https://opendata.muenchen.de/dataset/022a11ff-4dcb-4f03-b7dd-a6c94a094587/resource/784b925b-1d5f-43d3-8353-fd5d02fc7c53/download/rad_2014_15min_06_06_23_r.csv",
  "https://opendata.muenchen.de/dataset/022a11ff-4dcb-4f03-b7dd-a6c94a094587/resource/d6b3a72c-b180-40a0-a2ab-d97040737f20/download/rad_2015_15min_06_06_23_r.csv",
  "https://opendata.muenchen.de/dataset/022a11ff-4dcb-4f03-b7dd-a6c94a094587/resource/3913d9e6-1be8-4ee4-ab88-1266cbf161f1/download/rad_2016_15min_06_06_23_r.csv",
  "https://opendata.muenchen.de/dataset/022a11ff-4dcb-4f03-b7dd-a6c94a094587/resource/694b9927-b4d5-4e8f-9c62-09b8ac03c39a/download/rad_2017_15min_06_06_23_r.csv",
  "https://opendata.muenchen.de/dataset/022a11ff-4dcb-4f03-b7dd-a6c94a094587/resource/0a97a624-daa4-4cd8-a820-7d2fa6ffe89a/download/rad_2018_15min_06_06_23_r.csv",
  "https://opendata.muenchen.de/dataset/022a11ff-4dcb-4f03-b7dd-a6c94a094587/resource/893e1f16-6504-4f4f-b8b3-f907ef406cd5/download/rad_2019_15min_06_06_23_r.csv",
  "https://opendata.muenchen.de/dataset/022a11ff-4dcb-4f03-b7dd-a6c94a094587/resource/9be77b23-a444-4ba5-be9e-7f8594aa0188/download/rad_2020_15min_06_06_23_r.csv",
  "https://opendata.muenchen.de/dataset/022a11ff-4dcb-4f03-b7dd-a6c94a094587/resource/205c5c9e-9689-4c28-97cb-e575c6c772ce/download/rad_2021_15min_06_06_23_r.csv",
  "https://opendata.muenchen.de/dataset/022a11ff-4dcb-4f03-b7dd-a6c94a094587/resource/e65e0e26-ce1f-4e58-9260-beccac196e75/download/rad_2022_15min_06_06_23_r.csv"
)

cycling_ls = vector(mode = "list", length = length(cycling_url))

for (i in seq_len(length(cycling_ls))) {
    cycling_ls[[i]] = data.table::fread(cycling_url[i])
  }

cycling_ls[[15]] <- cycling_ls[[15]] %>% select(-t) # to unify the format
df_cycling = do.call(rbind, cycling_ls)


#------------------------------------------#
# Preprocess cycling data from the website #
#------------------------------------------#

# mutate date and uhrzeit_start into YYYY-MM-DD HH:MM:SS for prophet
df_cycling <- df_cycling %>% mutate(ds = as.character(paste(datum, uhrzeit_start)))

# Extract time-related variables
df_cycling <- df_cycling %>%
  mutate(
    year = lubridate::year(ds),
    month = lubridate::month(ds),
    day = lubridate::day(ds),
    hour = lubridate::hour(ds),
    dayofweek = lubridate::wday(ds, label = TRUE),
    hour_weekday = (dayofweek - 1) * 24 + hour
  )

# Translate German into English
df_cycling %>%
  rename("date" = datum,
         "station" = zaehlstelle,
         "direction_1" = richtung_1,
         "direction_2" = richtung_2,
         "comment" = kommentar) %>%
  select(
    date, station, direction_1, direction_2, comment, ds, year, month, day, hour, dayofweek, hour_weekday
  ) -> df_cycling

df_cycling$comment[which(df_cycling$comment == "Zählstelle noch nicht in Betrieb")] <- "station not yet in operation"
df_cycling$comment[which(df_cycling$comment == "Radweg vereist / nach Schneefall nicht geräumt / keine Messung möglich")] <- "bikelane icy / not cleared after snowfall / no measure possible"
df_cycling$comment[which(df_cycling$comment == "Baustelle")] <- "construction"
df_cycling$comment[which(df_cycling$comment == "Austausch Sensor")] <- "replacing sensor"
df_cycling$comment[which(df_cycling$comment == "Ausfall nach Beschädigung")] <- "failure after damage"

# Replace value of -1 in direction_1 or direction_2 with NA
df_cycling$direction_1 <- ifelse(df_cycling$direction_1 == -1, NA, df_cycling$direction_1)
df_cycling$direction_2 <- ifelse(df_cycling$direction_2 == -1, NA, df_cycling$direction_2)
