library(purrr)


# Introduction to purrr::map --------------------------------------------------
random_data <- tibble(
  a = c(1, 2, 3, 4, 5),
  b = c(10, 20, 30, 40, 50),
  c = c(100, 200, 300, 400, 500)
)

simple_func <- function(x=1, y=2, z=3, df = FALSE) {

  if (!df) {
    x + y + z
  } else {
    tibble(result = x + y + z)
  }

}


map(random_data$a, ~ simple_func(.x))

map_dbl(random_data$a, ~ simple_func(.x))

map2_dbl(random_data$a, random_data$b, ~ simple_func(.x, .y))

map2_df(random_data$a, random_data$b, ~ simple_func(.x, .y, df=T))

pmap_dfr(random_data, ~ simple_func(..1, ..2, ..3, df=T))

pmap_dfc(random_data, ~ simple_func(..1, ..2, ..3, df=T))




# Functions to get demand -------------------------------------------------

get_interval_demand <- function(interval, sessions_flex, interval_mins, by = c("Profile", "Session")) {
  print(paste(interval, " -------------------------------------------------------- "))
  sessions_flex %>%
    filter(
      ConnectionStartDateTime <= interval &
        ChargingEndDateTime >= (interval + minutes(interval_mins))
    ) %>%
    group_by(!!sym(by)) %>%
    summarise(Power = sum(Power)) %>%
    mutate(datetime = interval) %>%
    spread(!!sym(by), Power)
}

get_demand <- function(sessions_flex, seq_dt, interval_mins, by = "Profile") {

  demand <- map_dfr(seq_dt, ~get_interval_demand(.x, sessions_flex, interval_mins, by))


  tibble(datetime = seq_dt) %>%
    left_join(
      demand,
      by = "datetime"
    ) %>%
    replace(is.na(.), 0)
}


dates <- seq.Date(from = dmy(10022020), length.out = 15, by = "day")
interval_mins <- 15

seq_dt <- seq.POSIXt(
  from = as_datetime(dates[1], tz = "Europe/Amsterdam")-hours(1),
  length.out = 60/interval_mins*24*length(dates),
  by = paste(interval_mins, "min")
)


demand <- get_demand(sessions_models, seq_dt, interval_mins)


demand %>%
  df_to_ts() %>%
  dygraph() %>%
  dyOptions(fillGraph = T, useDataTimezone = T)







