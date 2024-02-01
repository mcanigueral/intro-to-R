library(dplyr)
library(lubridate)
library(tidyr)

# Each section created with Ctrl+Shift+R

# Reading data from JSON ------------------------------------------------------------

acn_json <- jsonlite::fromJSON("data/acndata_sessions_jpl.json")
acn_sessions <- acn_json[["_items"]]

# Data frame
acn_sessions
# Tibble
as_tibble(acn_sessions)


# Convert to datetime
acn_sessions$connectionTime[1]
acn_sessions[["connectionTime"]][1]
acn_sessions[1, "connectionTime"]

dmy_hms(acn_sessions$connectionTime[1])


# Charging point ID
unique(acn_sessions$siteID)
unique(acn_sessions$spaceID)

# Tidy data set
acn_sessions_tb <- acn_sessions %>%
  as_tibble() %>%
  separate(connectionTime, sep=',', into = c('Weekday', 'connectionDateTime')) %>%
  mutate(
    connectionDateTime = dmy_hms(connectionDateTime) %>% with_tz("America/Los_Angeles"),
    disconnectionDateTime = dmy_hms(disconnectTime) %>% with_tz("America/Los_Angeles"),
    chargingEndDateTime = dmy_hms(doneChargingTime) %>% with_tz("America/Los_Angeles")
  ) %>%
  rename(
    id = `_id`,
    Energy = kWhDelivered
  ) %>%
  select(-c(clusterID, userInputs, disconnectTime, doneChargingTime, timezone)) %>%
  select(spaceID, everything())




# Another data set --------------------------------------------------------

sessions_models <- readr::read_csv("data/sessions_models.csv")
sessions_models

# Types of profiles
unique(sessions_models$Profile)


# Data set preparation
# - timezone conversions (`readr` saves everything in UTC)
# - Date times rounded to hours
# - Connection datetime variable
# - Number of charging hours

sessions_models <- sessions_models %>%
  mutate(
    ConnectionStartDateTime = with_tz(ConnectionStartDateTime, tzone = "Europe/Amsterdam"),
    ChargingEndDateTime = with_tz(ChargingEndDateTime, tzone = "Europe/Amsterdam"),

    ChargingEndDateTime = floor_date(ChargingEndDateTime, unit = "hour"),

    ConnectionEndDateTime = ConnectionStartDateTime + hours(ChargingHours %/% 1) + minutes(round((ChargingHours %% 1)*60)),

    ChargingHours = as.numeric(ChargingEndDateTime - ConnectionStartDateTime, units = "hours")
  ) %>%
  filter(
    !(Profile %in% c("Shortstay", "Visit"))
  )


# Data set summary (nicer with tibbles than data.frames)
sessions_models %>% summary()

# Own summary
sessions_summary <- sessions_models %>%
  mutate(
    date = floor_date(ConnectionStartDateTime, "day")
  ) %>%
  group_by(date, Profile) %>%
  summarise(
    n = n(),
    max_kWh = max(Energy),
    max_kW = max(Power),
    avg_conn = mean(ConnectionHours)
  )






